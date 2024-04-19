app "time"
    packages {
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.6.3/_2Dh4Eju2v_tFtZeMq8aZ9qw2outG04NbkmKpFhXS_4.tar.br",
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
    }
    imports [
        pf.Path,
        pf.File,
        pf.Stdout,
        pf.Task,
        pf.Utc,
        pf.Sleep,
        JsonVal,
        JsonVal2,
        Core,
    ]

    provides [main] to pf

benchmark = \func, name, times ->
    _ <- Stdout.line "$(name): Starting $(times |> Num.toStr) runs" |> Task.await

    start <- Utc.now |> Task.await

    _ <- runBenchmark func times |> Task.await

    finish <- Utc.now |> Task.await

    duration = Utc.deltaAsMillis start finish

    average= Num.divCeil (duration|>Num.toU64) times

    Stdout.line "$(name): Completed $(times |> Num.toStr) runs in $(duration |> Num.toStr)ms, average $(average|> Num.toStr)ms"
runBenchmark = \func, times ->
    _ <- (benchLoop func times) |> Task.await
    Task.ok {}

# Tight loop to reduce task overhead
benchLoop = \fn, times ->
    List.repeat 0 times
    |> List.map \_ ->
        _ = fn {}
        {}
    |> Task.ok
isWhitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false
trimWhitespace = \file ->
    lastWhitespace =
        file
        |> List.walkBackwardsUntil 0 \state, byte ->
            if byte |> isWhitespace then
                state + 1 |> Continue
            else
                (state) |> Break
    file |> List.dropLast (lastWhitespace)

input = \fileName ->
    file <- File.readBytes (Path.fromStr fileName) |> Task.await
    trimmed = file |> trimWhitespace
    {} <- Stdout.line "file read,last $(List.last trimmed |> Inspect.toStr)" |> Task.await
    Task.ok trimmed

handleDecode = \res ->
    when res is
        Ok a -> Ok a
        Err (Leftover e) -> crash "Failed to decode json $(e |> List.takeLast 100 |> Str.fromUtf8 |> Result.withDefault "utf8fail")"
        e -> crash "error $(e |> Inspect.toStr)"

runBenchmarks = \_ ->
    file <- input "small-json.json" |> Task.await
    formattedFile <- input "formatted-json.json" |> Task.await
    times=5
    {} <- Stdout.line "starting" |> Task.await
    {} <- (\_ ->
            a : Result JsonVal2.JsonVal _
            a = Decode.fromBytes file Core.json |> handleDecode
            a
        )
        |> benchmark "jsonVal2" times
        |> Task.await
    {} <- Stdout.line "starting" |> Task.await
    {} <- (\_ ->
            a : Result JsonVal2.JsonVal _
            a = Decode.fromBytes file Core.json |> handleDecode
            a
        )
        |> benchmark "jsonVal2 Lots of whitespace" 1
        |> Task.await
    {} <- Stdout.line "starting Next" |> Task.await
    {} <- (\_ ->
            a : Result (List { id : Str }) _
            a = Decode.fromBytes file Core.json |> handleDecode
            a
        )
        |> benchmark "json id only" times
        |> Task.await
    {} <- Stdout.line "starting Next" |> Task.await
    # {} <- (\_ ->
    #     when JsonVal.fromBytes file is
    #         Ok _ -> Ok {}
    #         Err e -> crash "JsonVal Failed to decode json $(e|>Inspect.toStr)"
    #     ) |> benchmark "jsonVal" 1 |> Task.await
    Task.ok {}
main =
    runBenchmarks {} |> Task.onErr \err -> Stdout.line (Inspect.toStr err)

