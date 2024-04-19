interface DecodeDSL
    exposes [
        wrap,
        decode,
        wrapOnErr,
        tryWrap,
        wrapResult,
        makeState
    
    ]
    imports [Decode,DecodeUtils]
## Generates a decode DSL, This currently doesn't work because functions in records can't be truly generic and have specialisation
## When module params drops I can use that to get the bytes and fmt and have a proper specialisation
wrap = \{ bytes, fmt }, wrapper -> bytes |> Decode.fromBytesPartial fmt |> Decode.mapResult wrapper
decode = \{ bytes, fmt } -> bytes |> Decode.fromBytesPartial fmt
wrapOnErr = \res, state, wrapper -> res |> DecodeUtils.tryOnErr \_ -> wrap state wrapper
tryWrap = \res, wrapper -> res |> DecodeUtils.tryResult wrapper
wrapResult = \res, wrapper -> res |> Decode.mapResult wrapper
makeState= \bytes, fmt -> { bytes, fmt }
