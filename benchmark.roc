app "time"
    packages {
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.6.3/_2Dh4Eju2v_tFtZeMq8aZ9qw2outG04NbkmKpFhXS_4.tar.br",
     pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        pf.Utc,
        pf.Sleep,
        json.Core,
    ]

    provides [main] to pf

benchmark = \func, name, times ->
    start <- Utc.now |> Task.await

    _ <- runBenchmark func times |> Task.await

    finish <- Utc.now |> Task.await

    duration = (Utc.deltaAsMillis start finish) |> Num.toStr

    Stdout.line "$(name): Completed in $(duration)ms"
runBenchmark = \func, times ->
    Task.loop times \state ->
        if state != 0 then
            _ <- (benchLoop func) |> Task.await
            Task.ok (Step (state - 1))
        else
            Task.ok (Done {})

# Tight loop to reduce task overhead
benchLoop = \fn ->
    List.repeat 0 10000
    |> List.map \_ ->
        _ = fn {}
        {}
    |> Task.ok

input =
    """
    {"other9":9}
    """
    |> Str.toUtf8

main =
    {} <- (\_ -> Decode.decodeWith input contentChangeDecode Core.json) |> benchmark "single Decoder" 20 |> Task.await
    {} <- (\_ -> Decode.decodeWith input contentChangeDecode2 Core.json) |> benchmark "multi Decoder" 20 |> Task.await
    Task.ok {}

PartialChangeEvent : {
    text : Str,
}
FullChangeEvent : {
    range : U8,
    rangeLength : U64,
    text : Str,
}
Other : {
    text : Str,
    other : U8,
}
Other2 : {
    text : Str,
    other2 : U8,
}
Other3 : {
    text : Str,
    other3 : U8,
}
Other4 : {
    text : Str,
    other4 : U8,
}
Other5 : {
    text : Str,
    other5 : U8,
}
Other6 : {
    text : Str,
    other6 : U8,
}
Other7 : {
    text : Str,
    other7 : U8,
}
Other8 : {
    text : Str,
    other8 : U8,
}
Other9 : {
    text : Str,
    other9 : U8,
}

TextDocumentContentChangeEvent := [
    PartialChange PartialChangeEvent,
    FullChange FullChangeEvent,
    Other Other,
    Other2 Other2,
    Other3 Other3,
    Other4 Other4,
    Other5 Other5,
    Other6 Other6,
    Other7 Other7,
    Other8 Other8,
    Other9 Other9,
]
    implements [
        Decoding { decoder: contentChangeDecode },
        Encoding { toEncoder: contentChangeEncoder },
    ]

contentChangeEncoder = \@TextDocumentContentChangeEvent change ->
    when change is
        PartialChange a -> Encode.toEncoder a
        FullChange a -> Encode.toEncoder a
        Other a -> Encode.toEncoder a
        Other2 a -> Encode.toEncoder a
        Other3 a -> Encode.toEncoder a
        Other4 a -> Encode.toEncoder a
        _ -> Encode.toEncoder 1

## Try another decode if the last decode failed
tryOnErr : DecodeResult _, _ -> _
tryOnErr = \decoded, try ->
    when decoded.result is
        Err e -> try {}
        Ok res -> decoded

contentChangeDecode = Decode.custom \bytes, fmt ->
    Decode.fromBytesPartial bytes fmt
    |> Decode.mapResult PartialChange
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult FullChange
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other2
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other3
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other4
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other5
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other6
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other7
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other8
    |> tryOnErr \_ ->
        Decode.fromBytesPartial bytes fmt |> Decode.mapResult Other9
    |> Decode.mapResult @TextDocumentContentChangeEvent

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
        |> tryOnErr \_ -> (Decode.fromBytesPartial bytes fmt |> Decode.mapResult wrapper)

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

contentChangeDecode2 =
    wrapDecode PartialChange
    |> wrapOnErr FullChange
    |> wrapOnErr Other
    |> wrapOnErr Other2
    |> wrapOnErr Other3
    |> wrapOnErr Other4
    |> wrapOnErr Other5
    |> wrapOnErr Other6
    |> wrapOnErr Other7
    |> wrapOnErr Other8
    |> wrapOnErr Other9
    |> wrapSuccess @TextDocumentContentChangeEvent
