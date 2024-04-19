// For more information see https://aka.ms/fsharp-console-apps
open System.Text.Json
open System
open System.Text.Json
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.IO
open System.Text.Json.Nodes

[<CLIMutable>]
type MyObject = { id: string }

let json = File.ReadAllText("/home/eli/Code/roc/lsp/small-json.json")
let jsonSpaces = File.ReadAllText("/home/eli/Code/roc/lsp/formatted-json.json")

type JsonBenchmark() =
    [<Benchmark>]
    member _.IdOnly() =
        let objects = JsonSerializer.Deserialize<MyObject ResizeArray>(json)
        let item=(objects.Item (objects.Count-1)).id
        item

    [<Benchmark>]
    member _.JsonVal() =
        let objects = JsonSerializer.Deserialize<JsonNode>(json)
        //get id from the first item
        let arr= objects.AsArray()
        let item = arr.Item (arr.Count - 1)
        let id = (item.AsObject()["id"]).GetValue<string>()
        id

    [<Benchmark>]
    member _.JsonValWhitespace() =
        let objects = JsonSerializer.Deserialize<JsonNode>(jsonSpaces)
        //get id from the first item
        let arr= objects.AsArray()
        let item = arr.Item (arr.Count - 1)
        let id = (item.AsObject()["id"]).GetValue<string>()
        id

[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<JsonBenchmark>()
    0 // return an integer exit code
