## Implements basic Lsp Types for
## Init, Hover, DidOpen, DidChange, Completion
interface Handlers
    exposes [
        handleRequest,
        handleNotification,
    ]
    imports [
        CompletionItemKind,
        Types.Option.{ some, none },
        LspTypes.{ InitializeParams, InitializeResponse, textDocumentSync, toResponse, CompletionParams, CompletionResponse, DidOpenTextDocumentParams, DidChangeTextDocumentParams },
        DelayDecode.{Value},
        Core,
    ]

handleRequest = \requestMsg ->
    jsonResult = \res -> res |> Result.map \v -> DelayDecode.toJsonValue v
    when requestMsg is
        Init msg -> initialize msg |>jsonResult
        Completion msg -> completion msg |> jsonResult
        _ -> Err (Bad "Not implemented") 
handleNotification = \requestMsg ->
    when requestMsg is
        DidOpen msg -> didOpen msg
        DidChange msg -> didChange msg
        _ -> Err (Bad "no Implemented")

initialize : InitializeParams -> Result InitializeResponse _
initialize = \params -> {
        capabilities: {
            completionProvider: some {
                resolveProvider: some Bool.true,
                triggerCharacters: some ["."],
            },
            hoverProvider: some Bool.true,
            textDocumentSync: some (textDocumentSync Incremental),
        },
        serverInfo: some {
            name: "Eli's simple language server",
            version: some "0.1",
        },
    }
    |> Ok
completion : CompletionParams -> Result CompletionResponse _
completion = \params -> [
        {
            label: "Hello",
            kind: some (CompletionItemKind.from Text),
            detail: some "This is a test completion",
            documentation: none {},
        },
        {
            label: "World",
            kind: some (CompletionItemKind.from Text),
            detail: some "This is another test completion",
            documentation: none {},
        },
    ]
    |> Ok

didOpen : DidOpenTextDocumentParams -> Result {} _
didOpen = \params -> Ok {}

didChange : DidChangeTextDocumentParams -> Result {} _
didChange = \params -> Ok {}

