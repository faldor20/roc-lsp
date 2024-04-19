# Lsp implemented in roc 

Currently this is just a huge mess of experiments and examples. Because roc doesn't allow dependencies between subfolders or import aliases, basically everything is in the top level. Yay 


# Benchmarks
I have added some benchmarks of json decoding for use in the various languages.
ensure you have the correct dependencies by running `nix develop` before running
rust:
```
cd rust
cargo bench
```
dotnet:
```
cd jsonBench
dotnet run -c release
```
roc:
```
roc run --optimize --linker=legacy ./jsonBenchmark.roc
```
my Results are:
roc:

```
jsonVal2: Completed 5 runs in 1775ms, average 355ms
jsonVal2 Lots of whitespace: Completed 1 runs in 347ms, average 347ms
json id only: Completed 5 runs in 4647ms, average 930ms  

```
dotnet:
```
| Method            | Mean      | Error     | StdDev    |
|------------------ |----------:|----------:|----------:|
| IdOnly            | 39.112 ms | 0.7548 ms | 0.7060 ms |
| JsonVal           | 80.999 ms | 1.5198 ms | 3.6120 ms |
| JsonValWhitespace |  8.567 ms | 0.5367 ms | 1.5655 ms |
```
Rust:
```
decode_json_value      time:   [120.99 ms 121.21 ms 121.43 ms]

decode_json_id_only    time:   [15.524 ms 15.557 ms 15.591 ms]

decode_json_value_whitespace time: [7.2021 ms 7.3084 ms 7.4242 ms]
```
