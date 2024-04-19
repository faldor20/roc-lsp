interface NewJson
    exposes [
        JsonToken,
        start,
    ]

    imports [
    ]

# ======Tokenizer=====
# -----Utilities-----

# This is probably Slower untill the getUnchecked PR is merged

JsonToken : [
    StartRecord,
    EndRecord,
    StartList,
    EndList,
    Key Str,
    Bool Bool,
    Null,
    RawNum (List U8),
    String Str,
]

getUnchecked : List a, U64 -> a
getUnchecked = \lst, index ->
    when List.get lst index is
        Ok a -> a
        Err _ ->
            indexS = index |> Num.toStr
            crash "index: $(indexS) out of bounds"
u8ToStr = \u8 ->
    u8 |> Str.fromUtf8 |> Result.withDefault "NotUtf8"

# whitespaceArr =
#     List.range { start: At 0, end: At 255 }
#     |> List.map \i ->
#         when i is
#             ' ' -> Bool.true
#             '\r' -> Bool.true
#             '\n' -> Bool.true
#             '\t' -> Bool.true
#             _ -> Bool.false

# isWhitespace = \c ->
#     whitespaceArr
#     |> getUnchecked (c |> Num.toU64)
isWhitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

eatWhitespace : List U8 -> List U8
eatWhitespace = \bytes ->
    when bytes is
        [a, ..] if isWhitespace a -> eatWhitespace (List.dropFirst bytes 1)
        _ -> bytes

expect
    bytes =
        """
        {"hi":"hey"}
        """
        |> Str.toUtf8
    res = start bytes []
    Ok ([], [StartRecord, Key "hi", String "hey", EndRecord]) == res
expect
    bytes =
        """
        {"hi":[]}
        """
        |> Str.toUtf8
    res = start bytes []
    Ok ([], [StartRecord, StartList,EndList, EndRecord]) == res

expect
    bytes =
        """
        {"hi":{"nested":["item1","item2"]}}
        """
        |> Str.toUtf8
    res = start bytes []
    Ok ([], [StartRecord, Key "hi", StartRecord, Key "nested", StartList, String "item1", String "item2", EndList, EndRecord, EndRecord]) == res
expect
    bytes =
        """
        {"hi":{"num":10,"num2":-10,"null":null,"bool":true}}
        """
        |> Str.toUtf8
    res = start bytes []
    Ok ([], [StartRecord, Key "hi", StartRecord, Key "num", RawNum [49, 48], Key "num2", RawNum [45, 49, 48], Key "null", Null, Key "bool", Bool Bool.true, EndRecord, EndRecord]) == res
start : _, _ -> Result (List U8, List JsonToken) _
start = \bytes, tokens ->
    when eatWhitespace bytes is
        ['[', .. as rest] -> list rest tokens
        ['{', .. as rest] -> record rest tokens
        a -> parseError a "error at sart expected '[' or '{'"

parseError = \bytes, msg -> Err (ParseError "$(msg). Found: '$(bytes|>List.takeFirst 100 |> u8ToStr)'")

record = \bytes, tokens ->
    nTokens = tokens |> List.append StartRecord
    recordField bytes nTokens
recordField = \bytes, tokens ->
    when eatWhitespace bytes is
        ['}', .. as rest] -> Ok (rest, tokens |> List.append EndRecord)
        ['"', .. as keyBytes] ->
            string keyBytes tokens Key
            |> Result.try \(restB, sTokens) ->

                when eatWhitespace restB is
                    [':', .. as rest] -> 
#Eli: This has been inlined because I'm not sure if mutual recursion is tail recursive  
                        when value rest sTokens is
                            Ok (rest1,nTokens)->
                                when eatWhitespace rest1 is
                                    # TODO: This will allow for a trailing comma because recordValue matches '}'
                                    [',', .. as rest2] -> recordField rest2 nTokens
                                    ['}', .. as rest2] -> Ok (rest2, nTokens |> List.append EndRecord)
                                    a -> parseError a "record should contain values or close with '}'"
                            e->e
                            #end ilining
                    a -> parseError a "key should be followed by a ':'"

        a -> parseError a "record should contain keys or close"

value = \bytes, tokens ->
    wBytes=eatWhitespace bytes 
    when wBytes is
        ['{', .. as rest] ->
            record rest tokens

        ['[', .. as rest] ->
            list rest tokens

        ['"', .. as rest] ->
            string rest tokens String

        [num, ..] if isNumStart num -> takeJsonNumber wBytes tokens
        ['f', 'a', 'l', 's', 'e', ..] -> Ok (List.dropFirst wBytes 5, tokens |> List.append (Bool Bool.false))
        ['t', 'r', 'u', 'e', ..] -> Ok (List.dropFirst wBytes 4, tokens |> List.append (Bool Bool.true))
        ['n', 'u', 'l', 'l', ..] -> Ok (List.dropFirst wBytes 4, tokens |> List.append Null)
        # TODO: numbers and null
        a -> parseError a "expected a value of some sort"
# TODO this could be a LUT
isNumStart = \char -> char >= '0' && char <= '9' || char == '+' || char == '-' || char == '.' || char == 'e' || char == 'E'
takeNumRaw = \bytes, tokens ->
    endIdx = bytes |> List.findFirstIndex (isValidEnd)
    when endIdx is
        Ok end ->
            numBytes = bytes |> List.sublist { start: 0, len: end + 1 }
            Ok (bytes |> List.dropFirst (end + 1), tokens |> List.append (RawNum numBytes))

        Err a -> parseError bytes "Expected number to end "

list = \bytes, tokens ->
    nTokens = tokens |> List.append StartList
    when bytes is
        [']', .. as rest] -> Ok (rest, nTokens |> List.append EndList)
        listBytes -> 
            listBody listBytes nTokens


listBody = \bytes, tokens ->
    when value bytes tokens is 
        Ok (afterValueBytes, vTokens) ->
            when eatWhitespace afterValueBytes is
                [',', .. as rest] -> listBody rest vTokens
                [']', .. as rest] ->
                    lTokens = vTokens |> List.append EndList
                    Ok (rest, lTokens)

                a -> parseError a "expected list seperator or end."
        e->e

# JSON NUMBER PRIMITIVE --------------------------------------------------------

# Takes the bytes for a valid Json number primitive into a RocStr
#
# Note that this does not handle leading whitespace, any whitespace must be
# handled in json list or record decoding.
#
# |> List.dropIf \b -> b == '+'
# TODO ^^ not needed if roc supports "1e+2", this supports
# "+" which is permitted in Json numbers
#
# |> List.map \b -> if b == 'E' then 'e' else b
# TODO ^^ not needed if roc supports "1E2", this supports
# "E" which is permitted in Json numbers
takeJsonNumber : List U8, List JsonToken -> Result (List U8, List JsonToken) _
takeJsonNumber = \bytes, tokens ->

    when List.walkUntil bytes Start numberHelp is
        Finish n | Zero n | Integer n | FractionB n | ExponentC n ->
            taken : List U8
            taken =
                bytes
                |> List.sublist { start: 0, len: n }
                |> List.dropIf \b -> b == '+'
                |> List.map \b -> if b == 'E' then 'e' else b
            Ok (List.dropFirst bytes n, tokens |> List.append (RawNum taken))

        _ ->
            parseError bytes "Not a valid number"

numberHelp : NumberState, U8 -> [Continue NumberState, Break NumberState]
numberHelp = \state, byte ->
    when (state, byte) is
        (Start, b) if b == '0' -> Continue (Zero 1)
        (Start, b) if b == '-' -> Continue (Minus 1)
        (Start, b) if isDigit1to9 b -> Continue (Integer 1)
        (Minus n, b) if b == '0' -> Continue (Zero (n + 1))
        (Minus n, b) if isDigit1to9 b -> Continue (Integer (n + 1))
        (Zero n, b) if b == '.' -> Continue (FractionA (n + 1))
        (Zero n, b) if isValidEnd b -> Break (Finish n)
        (Integer n, b) if isDigit0to9 b && n <= maxBytes -> Continue (Integer (n + 1))
        (Integer n, b) if b == '.' && n < maxBytes -> Continue (FractionA (n + 1))
        (Integer n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        (FractionA n, b) if isDigit0to9 b && n <= maxBytes -> Continue (FractionB (n + 1))
        (FractionB n, b) if isDigit0to9 b && n <= maxBytes -> Continue (FractionB (n + 1))
        (FractionB n, b) if b == 'e' || b == 'E' && n <= maxBytes -> Continue (ExponentA (n + 1))
        (FractionB n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        (ExponentA n, b) if b == '-' || b == '+' && n <= maxBytes -> Continue (ExponentB (n + 1))
        (ExponentA n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentB n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentC n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentC n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        _ -> Break Invalid

NumberState : [
    Start,
    Minus U64,
    Zero U64,
    Integer U64,
    FractionA U64,
    FractionB U64,
    ExponentA U64,
    ExponentB U64,
    ExponentC U64,
    Invalid,
    Finish U64,
]

# TODO confirm if we would like to be able to decode
# "340282366920938463463374607431768211455" which is MAX U128 and 39 bytes
maxBytes : U64
maxBytes = 21 # Max bytes in a double precision float

isDigit0to9 : U8 -> Bool
isDigit0to9 = \b -> b >= '0' && b <= '9'

isDigit1to9 : U8 -> Bool
isDigit1to9 = \b -> b >= '1' && b <= '9'

isValidEnd : U8 -> Bool
isValidEnd = \b ->
    when b is
        ']' | ',' | ' ' | '\n' | '\r' | '\t' | '}' -> Bool.true
        _ -> Bool.false

# -----Strings------
# Decode a Json string primitive into a RocStr
#
# Note that decodeStr does not handle leading whitespace, any whitespace must be
# handled in json list or record decodin.
# TODO: There is a good case to be made that we shouldn't do any escaping or string processing here because we may just want to re-encode this same json and that requires converting back again
string = \bytes, tokens, tokenType ->

    { taken: strBytes, rest } = takeJsonString bytes

    # Remove starting and ending quotation marks, replace unicode
    # escpapes with Roc equivalent, and try to parse RocStr from
    # bytes
    result =
        strBytes
        |> List.sublist {
            start: 0,
            len: Num.subSaturated (List.len strBytes) 1,
        }
        |> \bytesWithoutQuotationMarks ->
            replaceEscapedChars { inBytes: bytesWithoutQuotationMarks, outBytes: [] }
        |> .outBytes
        |> Str.fromUtf8

    when result is
        Ok str ->
            Ok (rest, tokens |> List.append (tokenType str))

        Err _ ->
            Err (ParseError "Error parsing String")

takeJsonString : List U8 -> { taken : List U8, rest : List U8 }
takeJsonString = \bytes ->
    # TODO:ELI: I removed start from this because we would have already read "
    when List.walkUntil bytes (Chars 0) stringHelp is
        Finish n ->
            {
                taken: List.sublist bytes { start: 0, len: n },
                rest: List.dropFirst bytes n,
            }

        _ ->
            { taken: [], rest: bytes }

stringHelp : StringState, U8 -> [Continue StringState, Break StringState]
stringHelp = \state, byte ->
    when (state, byte) is
        (Chars n, b) if b == '"' -> Break (Finish (n + 1))
        (Chars n, b) if b == '\\' -> Continue (Escaped (n + 1))
        (Chars n, _) -> Continue (Chars (n + 1))
        (Escaped n, b) if isEscapedChar b -> Continue (Chars (n + 1))
        (Escaped n, b) if b == 'u' -> Continue (UnicodeA (n + 1))
        (UnicodeA n, b) if isHex b -> Continue (UnicodeB (n + 1))
        (UnicodeB n, b) if isHex b -> Continue (UnicodeC (n + 1))
        (UnicodeC n, b) if isHex b -> Continue (UnicodeD (n + 1))
        (UnicodeD n, b) if isHex b -> Continue (Chars (n + 1))
        _ -> Break (InvalidNumber)

StringState : [
    Chars U64,
    Escaped U64,
    UnicodeA U64,
    UnicodeB U64,
    UnicodeC U64,
    UnicodeD U64,
    Finish U64,
    InvalidNumber,
]

isEscapedChar : U8 -> Bool
isEscapedChar = \b ->
    when b is
        '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' -> Bool.true
        _ -> Bool.false

escapedCharFromJson : U8 -> U8
escapedCharFromJson = \b ->
    when b is
        '"' -> 0x22 # U+0022 Quotation mark
        '\\' -> 0x5c # U+005c Reverse solidus
        '/' -> 0x2f # U+002f Solidus
        'b' -> 0x08 # U+0008 Backspace
        'f' -> 0x0c # U+000c Form feed
        'n' -> 0x0a # U+000a Line feed
        'r' -> 0x0d # U+000d Carriage return
        't' -> 0x09 # U+0009 Tab
        _ -> b

expect escapedCharFromJson 'n' == '\n'

isHex : U8 -> Bool
isHex = \b ->
    (b >= '0' && b <= '9')
    || (b >= 'a' && b <= 'f')
    || (b >= 'A' && b <= 'F')

expect isHex '0' && isHex 'f' && isHex 'F' && isHex 'A' && isHex '9'
expect !(isHex 'g' && isHex 'x' && isHex 'u' && isHex '\\' && isHex '-')

jsonHexToDecimal : U8 -> U8
jsonHexToDecimal = \b ->
    if b >= '0' && b <= '9' then
        b - '0'
    else if b >= 'a' && b <= 'f' then
        b - 'a' + 10
    else if b >= 'A' && b <= 'F' then
        b - 'A' + 10
    else
        crash "got an invalid hex char"

expect jsonHexToDecimal '0' == 0
expect jsonHexToDecimal '9' == 9
expect jsonHexToDecimal 'a' == 10
expect jsonHexToDecimal 'A' == 10
expect jsonHexToDecimal 'f' == 15
expect jsonHexToDecimal 'F' == 15

decimalHexToByte : U8, U8 -> U8
decimalHexToByte = \upper, lower ->
    Num.bitwiseOr (Num.shiftLeftBy upper 4) lower

expect
    actual = decimalHexToByte 3 7
    expected = '7'
    actual == expected

expect
    actual = decimalHexToByte 7 4
    expected = 't'
    actual == expected

hexToUtf8 : U8, U8, U8, U8 -> List U8
hexToUtf8 = \a, b, c, d ->
    i = jsonHexToDecimal a
    j = jsonHexToDecimal b
    k = jsonHexToDecimal c
    l = jsonHexToDecimal d

    if i == 0 && j == 0 then
        [decimalHexToByte k l]
    else
        [decimalHexToByte i j, decimalHexToByte k l]

# Test for \u0074 == U+74 == 't' in Basic Multilingual Plane
expect
    actual = hexToUtf8 '0' '0' '7' '4'
    expected = ['t']
    actual == expected

# Test for \u0068 == U+68 == 'h' in Basic Multilingual Plane
expect
    actual = hexToUtf8 '0' '0' '6' '8'
    expected = ['h']
    actual == expected

# Test for \u2c64 == U+2C64 == 'Ɽ' in Latin Extended-C
expect
    actual = hexToUtf8 '2' 'C' '6' '4'
    expected = [44, 100]
    actual == expected

unicodeReplacement = hexToUtf8 'f' 'f' 'd' 'd'

replaceEscapedChars : { inBytes : List U8, outBytes : List U8 } -> { inBytes : List U8, outBytes : List U8 }
replaceEscapedChars = \{ inBytes, outBytes } ->

    firstByte = List.get inBytes 0
    secondByte = List.get inBytes 1
    inBytesWithoutFirstTwo = List.dropFirst inBytes 2
    inBytesWithoutFirstSix = List.dropFirst inBytes 6

    when Pair firstByte secondByte is
        Pair (Ok a) (Ok b) if a == '\\' && b == 'u' ->
            # Extended json unicode escape
            when inBytesWithoutFirstTwo is
                [c, d, e, f, ..] ->
                    utf8Bytes = hexToUtf8 c d e f

                    replaceEscapedChars {
                        inBytes: inBytesWithoutFirstSix,
                        outBytes: List.concat outBytes utf8Bytes,
                    }

                _ ->
                    # Invalid Unicode Escape
                    replaceEscapedChars {
                        inBytes: inBytesWithoutFirstTwo,
                        outBytes: List.concat outBytes unicodeReplacement,
                    }

        Pair (Ok a) (Ok b) if a == '\\' && isEscapedChar b ->
            # Shorthand json unicode escape
            replaceEscapedChars {
                inBytes: inBytesWithoutFirstTwo,
                outBytes: List.append outBytes (escapedCharFromJson b),
            }

        Pair (Ok a) _ ->
            # Process next character
            replaceEscapedChars {
                inBytes: List.dropFirst inBytes 1,
                outBytes: List.append outBytes a,
            }

        _ ->
            { inBytes, outBytes }

# Test replacement of both extended and shorthand unicode escapes
expect
    inBytes = Str.toUtf8 "\\\\\\u0074\\u0068\\u0065\\t\\u0071\\u0075\\u0069\\u0063\\u006b\\n"
    actual = replaceEscapedChars { inBytes, outBytes: [] }
    expected = { inBytes: [], outBytes: ['\\', 't', 'h', 'e', '\t', 'q', 'u', 'i', 'c', 'k', '\n'] }

    actual == expected

# tokenize=\bytes->

