interface TagDecoding
    exposes [
    ]

    imports [
    ]


CompletionItemKind := [
    Text,
    Method,
    Function,
    Constructor,
    Field,
    Variable,
    Class,
    Interface,
    Module,
    Property,
    Unit,
    Value,
    Enum,
    Keyword,
    Snippet,
    Color,
    File,
    Reference,
    Folder,
    EnumMember,
    Constant,
    Struct,
    Event,
    Operator,
    TypeParameter,
]
    implements [
        Decoding {
            decoder: decodeCompletionItemKind,
        },
        Encoding {
            toEncoder: encodeCompletionItemKind,
        },
    ]

decodeCompletionItemKind =
    ok = \tag -> Ok (@CompletionItemKind tag)
    DecodeUtils.wrapDecode \val ->
        when val is
            1 -> ok Text
            2 -> ok Method
            3 -> ok Function
            4 -> ok Constructor
            5 -> ok Field
            6 -> ok Variable
            7 -> ok Class
            8 -> ok Interface
            9 -> ok Module
            10 -> ok Property
            11 -> ok Unit
            12 -> ok Value
            13 -> ok Enum
            14 -> ok Keyword
            15 -> ok Snippet
            16 -> ok Color
            17 -> ok File
            18 -> ok Reference
            19 -> ok Folder
            20 -> ok EnumMember
            21 -> ok Constant
            22 -> ok Struct
            23 -> ok Event
            24 -> ok Operator
            25 -> ok TypeParameter
            _ -> Err TooShort

encodeCompletionItemKind = \@CompletionItemKind val ->
    num =
        when val is
            Text -> 1
            Method -> 2
            Function -> 3
            Constructor -> 4
            Field -> 5
            Variable -> 6
            Class -> 7
            Interface -> 8
            Module -> 9
            Property -> 10
            Unit -> 11
            Value -> 12
            Enum -> 13
            Keyword -> 14
            Snippet -> 15
            Color -> 16
            File -> 17
            Reference -> 18
            Folder -> 19
            EnumMember -> 20
            Constant -> 21
            Struct -> 22
            Event -> 23
            Operator -> 24
            TypeParameter -> 25
    Encode.u32 num
