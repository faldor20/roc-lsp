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
    start <- Utc.now |> Task.await

    _ <- runBenchmark func times |> Task.await

    finish <- Utc.now |> Task.await

    duration = (Utc.deltaAsMillis start finish) |> Num.toStr

    Stdout.line "$(name): Completed $(times |> Num.toStr) runs in $(duration)ms"
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

input = \{} ->
    file<-File.readBytes (Path.fromStr "./small-json.json") |>Task.await
    {}<-Stdout.line "file read" |> Task.await
    Task.ok (file|>List.dropLast 1)



runBenchmarks = \_ ->
    file <- input {}  |>Task.await
    {} <- Stdout.line "starting" |> Task.await
    {} <- (\_ ->
            a : Result JsonVal2.JsonVal _
            a = Decode.fromBytes file Core.json
            when a is
                Ok _ -> Ok {}
                Err (Leftover e) -> crash "JsonVal2 Failed to decode json $(e|>List.takeLast 100|>Str.fromUtf8|>Result.withDefault "utf8fail")"
                e->crash "error $(e|>Inspect.toStr)"
            )
        |> benchmark "jsonVal2" 1
        |> Task.await
    # {} <- (\_ ->
    #         a : Result (List {id:Str}) _
    #         a = Decode.fromBytes file Core.json
    #         when a is
    #             Ok _ -> Ok {}
    #             Err (Leftover e) -> crash "JsonIdOnly Failed to decode json $(e|>List.takeLast 100|>Str.fromUtf8|>Result.withDefault "utf8fail")"
    #             e->crash "error $(e|>Inspect.toStr)"
    #         )
    #     |> benchmark "json id only" 1
    #     |> Task.await
    # {} <- Stdout.line "starting Next" |> Task.await
    # {} <- (\_ -> 
    #     when JsonVal.fromBytes file is
    #         Ok _ -> Ok {}
    #         Err e -> crash "JsonVal Failed to decode json $(e|>Inspect.toStr)"
    #     ) |> benchmark "jsonVal" 1 |> Task.await
    Task.ok {}
main =
    runBenchmarks {} |> Task.onErr \err -> Stdout.line (Inspect.toStr err)

