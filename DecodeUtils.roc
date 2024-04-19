interface DecodeUtils
    exposes [
        wrapDecode,
        wrapOnErr,
        wrapSuccess,
        tryWrapDecode,
        tagDecoder,
        tryResult,
        tryOnErr,
        tryMapResult,
    ]
    imports [Decode]
## Try to another decode using the first decode as input
## *rest* - The "rest" bytes from the first decode
tryResult : DecodeResult _, _ -> _
tryResult = \decoded, try ->
    when decoded.result is
        Err e -> { result: Err e, rest: decoded.rest }
        Ok res -> try res decoded.rest
## Try another decode if the last decode failed
tryOnErr : DecodeResult _, _ -> _
tryOnErr = \decoded, try ->
    when decoded.result is
        Err e -> try {}
        Ok res -> decoded
## Applys the mapper to a succcecesful decode result. The mapper also returns a result
## This is useful if you have a mapping that may fail, like decoding a custom string to a tag
## ```roc
## dec = Decode.custom \bytes, fmt ->
##     Decode.fromBytesPartial bytes fmt |> tryMap \str ->
##         when str is
##             "hi" -> Ok Hey
##             _ -> Err TooShort
## ```
## If you don't need your mapper to be able to fail, use `Decode.mapResult`
## If you want to produce a whole new decode output using the input of the previous decode use `tryResult`
tryMapResult : DecodeResult _, (_ -> Result val DecodeError) -> DecodeResult _
tryMapResult = \decoded, mapper ->
    when decoded.result is
        Err e -> { result: Err e, rest: decoded.rest }
        Ok res -> { result: mapper res, rest: decoded.rest }

## Makes a decoder that decodes the input and then wraps it using the wrapper
## Useful for decoding Opaque types or any decode where you wish to decode to an intermediate and the furthur pass that intermediate
## eg: string enum to tags, int enum to tags
tryWrapDecode = \wrapper ->
    Decode.custom \bytes, fmt ->
        Decode.fromBytesPartial bytes fmt |> tryMapResult wrapper
#====Decode DSL using intermediate decoders====


## Makes a decoder that decodes the input and then wraps it using the wrapper
## Useful for decoding Opaque types or any decode where you wish to decode to an intermediate and the furthur pass that intermediate
## eg: string enum to tags, int enum to tags
wrapDecode = \wrapper ->
    Decode.custom \bytes, fmt ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult wrapper
## makes a decoder that tries to decode using the proveded firstDecoder
## If the firstDecoder fails, it attempts a new decode with a new wrapper
## Use Like:
## ```roc
## contentChangeDecode =
##     wrapDecode PartialChange
##     |> wrapOnErr FullChange
##     |> wrapSuccess @TextDocumentContentChangeEvent
## ``
wrapOnErr = \firstDecoder, wrapper -> Decode.custom \bytes, fmt ->
        Decode.decodeWith bytes firstDecoder fmt
        |> DecodeUtils.tryOnErr \_ -> (Decode.fromBytesPartial bytes fmt |> Decode.mapResult wrapper)

## Takes a decoder and wraps it's output using the provided wrapper
## Use Like:
## ```roc
## contentChangeDecode =
##     wrapDecode PartialChange
##     |> wrapOnErr FullChange
##     |> wrapSuccess @TextDocumentContentChangeEvent
## ``
wrapSuccess = \decoder, wrapper -> Decode.custom \bytes, fmt ->
        Decode.decodeWith bytes decoder fmt |> Decode.mapResult wrapper

## Makes a decoder that turns a string into an opaque tag type
##
## ```roc
## MarkupKind := [PlainText, Markdown] implements [Decoding { decoder: markupKindDecoder }]
## markupKindDecoder =
##     [
##         ("plainText", PlainText),
##         ("markdown", Markdown),
##     ]
##     |> tagDecoder @MarkupKind
## ```roc
tagDecoder = \map, opaqueType ->
    wrapDecode \str ->
        map
        |> List.walkUntil (Err TooShort) \state, (tagStr, tag) ->
            if tagStr == str then
                Break (Ok (opaqueType tag))
            else
                Continue state

## Makes a decoder that will decode a number to a tag, based on it's index in a list
tagDecoderNumbered = \tagsList, offset, opaqueType ->
    wrapDecode \decoded ->
        when tagsList |> List.get (decoded + offset) is
            Ok tag -> opaqueType tag
            Err _ -> Err TooShort

## Makes a decoder that will decode a number to a tag, based on it's index in a list
tagEncoderNumbered = \tagsList, offset ->
    \tag ->
        when tagsList |> List.findFirstIndex tag is
            Ok idx -> (idx + offset) |> Encode.u64
            Err _ -> crash "didn't have a match for this tag in the tagsList"
# ## Attempt many decodes, one after another generally this is used for tag variants
# ## ```roc
# ##  multiDecode [U1, U2]
# multiDecode = \wrappers,finalWrapper ->
#     Decode.custom \bytes, fmt ->
#         decode = \wrapper ->
#             Decode.fromBytesPartial bytes fmt |> Decode.mapResult wrapper

#         first = wrappers |> List.first |> Result.withDefault (crash "multiDecode must have at least one item")
#         wrappers
#         |> List.dropFirst 1
#         |> List.walkUntil (decode first) \state, wrapper ->
#             when state is
#                 { result: Ok _ } -> Continue (decode wrapper)
#                 a -> Break a
#         |>Decode.mapResult finalWrapper


# startDecode = \bytes, fmt -> { bytes, fmt }
# wrap = \{ bytes, fmt }, wrapper ->
#     res = bytes |> Decode.fromBytesPartial fmt |> Decode.mapResult wrapper
#     { state: { bytes, fmt }, res }

# wrapOnErr = \{ state: { bytes, fmt } as state, res }, wrapper ->
#     res2 = res |> DecodeUtils.tryOnErr \_ -> bytes |> Decode.fromBytesPartial fmt |> Decode.mapResult wrapper
#     { state, res2 }

# wrapResult = \{ state, res }, wrapper -> res |> Decode.mapResult wrapper
