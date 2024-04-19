
## Represents either a value, or nothing and returns a result when getting it's value it
## **NAME NOT FINAL** It is called ResOption to differentiate from normal Option
interface ResOption
    exposes [none, some, get, from, fromResult]
    imports [
        Encode,
        Core,
    ]

ResOption val := Result val [None]
    implements [
        Eq,
        Decoding {
            decoder: decoderRes,
        },
        Encoding {
            toEncoder: toEncoderRes,
        },
    ]
none = \{} -> @ResOption (Err None)
some = \val -> @ResOption (Ok val)

get = \@ResOption val -> val
from = \val -> @ResOption val

fromResult : Result a * -> _
fromResult = \val ->
    when val is
        Ok a -> some a
        Err _ -> none {}

toEncoderRes = \@ResOption val ->
    Encode.custom \bytes, fmt ->
        when val is
            Ok contents -> bytes |> Encode.append contents fmt
            Err None -> bytes

decoderRes = Decode.custom \bytes, fmt ->
    when bytes is
        [] -> { result: Ok (none {}), rest: [] }
        _ ->
            when bytes |> Decode.decodeWith (Decode.decoder) fmt is
                { result: Ok res, rest } -> { result: Ok (some res), rest }
                { result: Err a, rest } -> { result: Err a, rest }

OptionTest2 : { name : Str, lastName : ResOption Str, age : ResOption U8 }
expect
    decoded : Result OptionTest2 _
    decoded =
        """
        {"age":1,"name":"hi"}
        """
        |> Str.toUtf8
        |> Decode.fromBytes Core.json

    expected = Ok ({ name: "hi", lastName: none {}, age: some 1u8 })
    expected == decoded

expect
    toEncode : OptionTest2
    toEncode =
        { name: "hi", lastName: none {}, age: some 1u8 }
    encoded =
        toEncode
        |> Encode.toBytes Core.json
        |> Str.fromUtf8

    expected =
        Ok
            """
            {"age":1,"name":"hi"}
            """
    expected == encoded

