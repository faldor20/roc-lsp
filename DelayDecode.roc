## This will allow you to either encode or decode some data later
## A way of storing any data type into some json when you aren't quite sure what it will be until runtime
## This is the simplest way, but it's quite innefficient because we are storing this big string. We should be able to decode into some type of JSONValue like serde
## Ideally
interface DelayDecode
    exposes [
        getBytes,
        decodeBytes,
        Value,
        toJsonValue,
        decodeJsonBytes,
        fromJsonBytes

    ]
    imports [
        Core,
    ]

Value := {data:List U8}
    implements [
        Decoding {
            decoder: delayDecodeDecoder,
        },
        Encoding {
            toEncoder: toValueEncoder,
        },
        Eq,
    ]
# JsonValue:Value Core.Json

delayDecodeDecoder =
    Decode.custom \bytes, fmt ->
        result = Ok (@Value {data:bytes})
        rest = []
        { result, rest }
toValueEncoder = \@Value val ->
    Encode.custom \bytes, fmt -> bytes |> List.concat val.data
getBytes = \@Value val -> val
fromJsonBytes = \val -> @Value {data:val}

decodeBytes = \@Value val, fmt -> Decode.fromBytes val.data fmt
decodeJsonBytes = \@Value val -> Decode.fromBytes val.data Core.json
toJsonValue = \val-> @Value {data:Encode.toBytes val Core.json}



expect 
    testVal=@Value {data:['1','2']}
    encoded=Encode.toBytes testVal Core.json
    encoded==['1','2']
expect 

    testVal={method:"hi",params:@Value {data:"""{"myparam":10}"""|>Str.toUtf8}}
    encoded=Encode.toBytes testVal Core.json|>Str.fromUtf8
    expected=
        """
        {"method":"hi","params":{"myparam":10}}
        """

    encoded==Ok expected

expect 

    str=
        """
        {"method":"initialized","params":{"hi":"this"}}
        """
    paramsStr="""{"hi":"this"}"""
    expected={method:"hi",params:@Value {data:"""{"myparam":10}"""|>Str.toUtf8}}
    decoded=Decode.fromBytes str Core.json
    decoded==Ok expected



# This could be implemented a few different ways:
# 1. I could decode the entire jsonValue immediately 
# 2. I could make a slice of the byte array for each value once we hit an object and then just put the data in there  
JsonValue:=[
    Null,
    Bool Bool,
    Number (I64),
    String Str,
    Array List JsonValue,
    Object (Dict Str JsonValue),
]
    implements [
        Decoding {
            decoder: jsonDecoder,
        },
        Encoding {
            toEncoder: jsonEncoder,
        },
        Eq,
    ]

