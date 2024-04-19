interface JsonVal
    exposes [
        fromBytes,
    ]

    imports [
        NewJson.{ JsonToken, start },
    ]

# ====JsonVal====
JsonVal : [
    Null,
    Bool Bool,
    Num Dec,
    String Str,
    Record (Dict Str JsonVal),
    List (List JsonVal),
]

expect
    bytes =
        """
        {"hi":{"num":10,"num2":-10,"null":null,"bool":true}}
        """
        |> Str.toUtf8
    res = fromBytes bytes
    expected=
        Dict.fromList [
            ("hi", Dict.fromList [
                    ("num", Num 10),
                    ("num2", Num -10),
                    ("null", Null),
                    ("bool", Bool Bool.true),
                ]
                |> Record,
            ),
        ]
        |> Record
        |> Ok
    res==expected

expect
    bytes =
        """
        {"hi":["a","b","c"]}
        """
        |> Str.toUtf8
    res = fromBytes bytes
    expected=
        Dict.fromList [
            ("hi", [
                    String "a",
                    String "b",
                    String "c",
                
                ]
                |> List,
            ),
        ]
        |> Record
        |> Ok
    res==expected

# dropTrailingWhitespace = \bytes->
#     List.walkBackwardsUntil bytes i (\a -> )
#     List.dropLast
    
fromBytes : List U8 -> Result JsonVal _
fromBytes = \bytes ->
    start bytes []
    |> Result.try \(rest, tokens) ->

        when rest is
            []|['\n']|['\n',' '] ->
                Ok (fromTokens tokens)

            a -> Err (TooLong "Extra bytes after parsing '$(a|>Str.fromUtf8|>Result.withDefault "bad utf8")'. tokens:$(tokens|>Inspect.toStr)" )
toDecUnsafe = \str ->
    str
    |> Str.fromUtf8
    |> Result.try (\a -> a |> Str.toDec) # TODO: This should crash
    |> Result.withDefault (0)
valueToken = \tokens ->
    dbg tokens

    when tokens is
        [StartRecord, .. as rest] ->
            recordToken rest
        [StartList, .. as rest] -> listToken rest
        [String a, .. as rest] -> (String a, rest)
        [Bool b, .. as rest] -> (Bool b, rest)
        [RawNum bytes, .. as rest] -> (bytes |> toDecUnsafe |> Num, rest)
        [Null, .. as rest] -> (Null, rest)
        [a, ..] -> crash "Value: Invalid token: $(a |> Inspect.toStr) "
        [] -> crash "Value: no Token "

recordBody = \dict, tokens ->
    when tokens is
        [EndRecord, .. as rest] -> (dict, rest)
        [Key key, .. as rest] ->
            (value, vRest) = valueToken rest
            recordBody (dict |> Dict.insert key value) vRest

        a -> crash "Record: Invalid token: $(a |> Inspect.toStr)"

recordToken : List JsonToken -> (JsonVal, List JsonToken)
recordToken = \tokens ->
    dbg tokens

    (dict, nTokens) = recordBody (Dict.empty {}) tokens
    (Record dict, nTokens)

listBody = \list, tokens ->
    when tokens is
        [EndList, .. as rest] -> (list, rest)
        _ ->
            (value, rest) = valueToken tokens
            listBody (List.append list value) rest

listToken : List JsonToken -> (JsonVal, List JsonToken)
listToken = \tokens ->
    (list, ntokens) = listBody [] tokens
    (List list, ntokens)

fromTokens : List JsonToken -> JsonVal
fromTokens = \tokens ->
    (val, leftovers) =
        when tokens is
            [StartRecord, .. as rest] -> recordToken rest
            [StartList, .. as rest] -> listToken rest
            a -> crash "Invalid Token"
    when leftovers is
        [] -> val
        a -> crash "Invalid Token"

