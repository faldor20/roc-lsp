## Implements basic Lsp Types for
## Init, Hover, DidOpen, DidChange, Completion
interface LspTypes
    exposes [
        RequestMessage,
        NotificationMessage,
        # LspMessage,
        fromRequestMessage,
        toRequestMessage,
        # ResponseMessage,
        notificationMessage,
        textDocumentSync,
        DidOpenTextDocumentParams,
        DidChangeTextDocumentParams,
        HoverParams, 
        CompletionParams,
        InitializeParams,
        InitializeResponse,
        CompletionResponse,
        HoverResponse,
        Position,
        Range,
        MarkupKind,
        MarkupContent,
        DocumentUri,
        TextDocumentIdentifier,
        VersionedTextDocumentIdentifier,
        TextDocumentItem,
        WorkDoneProgressParams,
        CompletionTriggerKind,




    ]
    imports [
        Types.Union2.{ Union2 },
        Types.Option.{ Option, none, some },
        Core,
        DecodeUtils,
        DelayDecode,
        CompletionItemKind.{ CompletionItemKind },
    ]
Position : {
    line : U64,
    character : U64,
}
position : _, _ -> Position
position = \line, character -> { line, character }
Range : {
    start : Position,
    end : Position,
}

## MarkupKind: 'plainText'|'markdown'
# TODO:implement encoding
MarkupKind := [PlainText, Markdown] implements [Decoding { decoder: markupKindDecoder }, Encoding { toEncoder: markupKindEncoder }]

markupKindDecoder =
    DecodeUtils.tryWrapDecode \str ->
        when str is
            "plainText" -> Ok (@MarkupKind PlainText)
            "Markdown" -> Ok (@MarkupKind Markdown)
            _ -> Err TooShort

markupKindEncoder = \@MarkupKind val ->
    str =
        when val is
            PlainText -> "plainText"
            Markdown -> "Markdown"

    str |> Encode.toEncoder

MarkupContent : {
    kind : MarkupKind,
    value : Str,
}
##TODO: This should have some decoding constraints and probably be opaque
DocumentUri : Str

TextDocumentIdentifier : {
    uri : DocumentUri,
}
VersionedTextDocumentIdentifier : {
    version : I64,
}

TextDocumentItem : {
    ## The text document's URI.
    uri : DocumentUri,

    ## The text document's language identifier.
    languageId : Str,

    ## The version number of this document (it will increase after each
    ## change, including undo/redo).
    version : I64,

    ## The content of the opened text document.
    text : Str,
}

WorkDoneProgressParams : {
    workDoneToken : Union2 I64 Str,
}

## Doesn't work
# ProgressToken : Union2 I64 Str

## **Invoked**
## Completion was triggered by typing an identifier (24x7 code
## complete), manual invocation (e.g Ctrl+Space) or via API.
##
## **TriggerCharacter**
## Completion was triggered by a trigger character specified by
## the `triggerCharacters` properties of the
## `CompletionRegistrationOptions`.
##
## **TriggerCharacter**
## Completion was re-triggered as the current completion list is incomplete.
CompletionTriggerKind := [Invoked, TriggerCharacter, TriggerForIncompleteCompletions]
    implements [
        Decoding {
            decoder: decodeCompletionTriggerKind,
        },
        Encoding { toEncoder: encodeCompletionTriggerKind },
        Eq,
    ]

decodeCompletionTriggerKind =
    ok = \tag -> Ok (@CompletionTriggerKind tag)
    DecodeUtils.tryWrapDecode \val ->
        when val is
            1 -> ok Invoked
            2 -> ok TriggerCharacter
            3 -> ok TriggerForIncompleteCompletions
            _ -> Err TooShort
encodeCompletionTriggerKind = \@CompletionTriggerKind tag ->
    num =
        when tag is
            Invoked -> 1
            TriggerCharacter -> 2
            TriggerForIncompleteCompletions -> 3
    num |> Encode.u8

## How a completion was triggered
CompletionContext : {
    triggerKind : CompletionTriggerKind,
    triggerCharacter : Option Str,
}
CompletionParams : {
    textDocument : TextDocumentIdentifier,
    position : Position,
    workDoneToken : Option (Union2 I64 Str),
    partialResultToken : Option (Union2 I64 Str),
    context : Option CompletionContext,
}

CompletionItem : {
    label : Str,
    kind : Option CompletionItemKind,
    detail : Option Str,
    documentation : Option MarkupContent,
    # There are many other fields we will be ommiting for the sake of brevity. They are not needed for simple completion
    # tags : Option (List CompletionItemTag),
    # labelDetails : Option CompletionItemLabelDetails,
    # deprecated: Option Bool,
    # preselect : Option Bool,
    # sortText : Option Str,
    # filterText : Option Str,
    # insertText : Option Str,
    # insertTextFormat : Option InsertTextFormat,
    # insertTextMode : Option InsertTextMode,
    # textEdit : Option (Union2 TextEdit InsertReplaceEdit),
    # textEditText : Option Str,
    # additionalTextEdits : Option (List TextEdit),
    # commitCharacters : Option (List Str),
    # command : Option Command,
    # data : Option LSPAny,
}

# ==== Content Change====

PartialChangeEvent : {
    text : Str,
}
FullChangeEvent : {
    range : Range,
    rangeLength : Option U64,
    text : Str,
}

TextDocumentContentChangeEvent := [PartialChange PartialChangeEvent, FullChange FullChangeEvent]
    implements [
        Eq,
        Decoding { decoder: contentChangeDecode },
        Encoding { toEncoder: contentChangeEncoder },
    ]

contentChangeEncoder = \@TextDocumentContentChangeEvent change ->
    when change is
        PartialChange a -> Encode.toEncoder a
        FullChange a -> Encode.toEncoder a

contentChangeDecode =
    DecodeUtils.wrapDecode PartialChange
    |> DecodeUtils.wrapOnErr FullChange
    |> DecodeUtils.wrapSuccess @TextDocumentContentChangeEvent

# BOOK: We won't implement any of this because it isn't something we need for our simple  server. if you needed some of these options you could add them.
InitializeParams : {
    processId : Option I64,
    # workspaceFolders : Option (List WorkspaceFolder),
    # clientInfo : Option {
    #     name : Str,
    #     version : Option Str,
    # },
    # locale : Option Str,
    # rootPath : Option Str,
    # rootUri : Option Str,
    # initializationOptions : Option LSPAny,
    # capabilities : ClientCapabilities,
    # trace : Option TraceValue,
}
DidChangeTextDocumentParams : {
    textDocument : VersionedTextDocumentIdentifier,
    contentChanges : List TextDocumentContentChangeEvent,
}

DidOpenTextDocumentParams : {
    ## The document that was opened.
    textDocument : TextDocumentItem,
}
HoverParams : {
    textDocument : TextDocumentIdentifier,
    position : Position,
    workDoneToken : Option (Union2 I64 Str),
}

RequestMessage := {
    id : Union2 I64 Str,
    method : Str,
    # TODO: This should techincally be a union of array and object
    # BOOk: notice how we don't make it optional, We do that because we know if it exists when we differentiate types by their method
    params : [
        Init InitializeParams,
        Hover HoverParams,
        Completion CompletionParams,
        Unknown (DelayDecode.Value)
    ],
}
    implements [
        Decoding {
            decoder: decodeRequestMessage,

        },
        Encoding {
            toEncoder: requestEncode,
        },
        Eq,
    ]


## This is not optimal, we are allocating everything twice because we first decode to a type with one specific params type and then convert that to the requestMessage type wrapping it in a tag
decodeRequestMessage = Decode.custom \bytes, fmt ->
    decodeRequest = \requestType ->
        Decode.fromBytesPartial bytes fmt
        |> Decode.mapResult \res -> @RequestMessage { params: requestType res.params, id: res.id, method: res.method }

    Decode.fromBytesPartial bytes fmt
    |> DecodeUtils.tryResult \res, rest ->
        when res.method is
            "initialize" -> decodeRequest Init
            "textDocument/hover" -> decodeRequest Hover
            "textDocument/completion" -> decodeRequest Completion
            _->decodeRequest Unknown

requestEncode = \@RequestMessage val ->
    encodeRequest=\ param->
        Encode.custom \bytes, fmt -> bytes |> Encode.append { params: param, id: val.id, method: val.method } fmt
    when val.params is
        Init a -> encodeRequest a
        Hover a -> encodeRequest a
        Completion a -> encodeRequest a
        Unknown a-> encodeRequest a

fromRequestMessage = \@RequestMessage req -> req
toRequestMessage = \req -> @RequestMessage req

NotificationMessage := {
    method : Str,
    # TODO: This should techincally be a union of array and object
    # BOOk: notice how we don't make it optional, We do that because we know if it exists when we differentiate types by their method
    params : [
        DidOpen DidOpenTextDocumentParams,
        DidChange DidChangeTextDocumentParams,
        Unknown (DelayDecode.Value)
    ],
}
    implements [
        Decoding {
            decoder: decodeNotificationMessage,

        },
        Encoding {
            toEncoder: notificationEncode,
        },
        Eq,
    ]

## This is not optimal, we are allocating everything twice because we first decode to a type with one specific params type and then convert that to the requestMessage type wrapping it in a tag
decodeNotificationMessage = Decode.custom \bytes, fmt ->
    decodeRequest = \requestType ->
        Decode.fromBytesPartial bytes fmt
        |> Decode.mapResult \res -> @NotificationMessage { params: requestType res.params,  method: res.method }

    Decode.fromBytesPartial bytes fmt
    |> DecodeUtils.tryResult \res, rest ->
        when res.method is
            "textDocument/didOpen" -> decodeRequest DidOpen
            "textDocument/didChange" -> decodeRequest DidChange

            _ -> 
                dbg "unknown method"
                Decode.fromBytesPartial bytes fmt|>Decode.mapResult \d->

                    @NotificationMessage({params:Unknown d.params,method:res.method})


notificationEncode= \@NotificationMessage val ->
    encodeRequest=\ param->
        Encode.custom \bytes, fmt -> bytes |> Encode.append { params: param,  method: val.method } fmt
    when val.params is
        DidOpen a -> encodeRequest a
        DidChange a -> encodeRequest a
        Unknown a->encodeRequest a

notificationMessage = \@NotificationMessage notif -> notif 

# LspMessage: Union2 RequestMessage NotificationMessage 

# =====Testing====
sampleHover =
    """
    {"jsonrpc":"2.0","method":"textDocument/hover","params":{"position":{"character":0,"line":5},"textDocument":{"uri":"file:///home/eli/Code/roc/langServer/main.roc"}},"id":1}        
    """
    |> Str.toUtf8

# Decode HoverParams
expect
    testDecode : Result RequestMessage _
    testDecode = sampleHover |> Decode.fromBytes Core.json
    when testDecode is
        Ok (@RequestMessage { params: Hover hover }) ->
            hover.position == (position 5 0)

        _ -> Bool.false

# RequestMessage should be opaque
# It will have its own decoder.
# In the decoder we will decide which Request it should decode to
# It will return a tag union of all the possible types

# ResponseMessageIntern a : {
#     id : Option (Union2 I64 Str),
#     result : Option a,
#     # TODO: This should techincally be a union of array and object
#     error : Option ResponseErr,
# }

ResponseErr : {
    code : I64,
}

# ResponseMessage := {

#     id : Option (Union2 I64 Str),
#     result : Option ,
#     # TODO: This should techincally be a union of array and object
#     error : Option ResponseErr,

# }
#     implements [
#         # Decoding {
#         #     decoder: decodeRequestMessage,
#         # },
#         Encoding {
#             toEncoder: responseToEncoder,
#         },
#     ]

# responseToEncoder : ResponseMessage -> _
# responseToEncoder = \@ResponseMessage val ->
#     when val.result is
#         Hover a ->
#             # Encode.toEncoder
#             Encode.custom \bytes, fmt -> bytes |> Encode.append a fmt

#         Completion a ->
#             # Encode.toEncoder
#             Encode.custom \bytes, fmt -> bytes |> Encode.append a fmt

#         Init a ->
#             Encode.custom \bytes, fmt -> bytes |> Encode.append a fmt
# # Encode.custom \bytes, fmt -> bytes |> Encode.append a fmt

# getResponse = \@ResponseMessage response -> response
# toResponse = \response -> @ResponseMessage response

HoverResponse : {
    ##The hover's content
    # Note, usually you can return a markedString or a markedString list,or markupcontent we will only return a markupContent for simplicity
    contents : MarkupContent,

    ## An optional range is a range inside a text document
    ## that is used to visualize a hover, e.g. by changing the background color.
    range : Option Range,
}

CompletionResponse : List CompletionItem

InitializeResponse : {
    capabilities : ServerCapabilities,
    serverInfo : Option {
        name : Str,
        version : Option Str,
    },
}

TextDocumentSyncKind := [None, Full, Incremental]
    implements [
        Decoding { decoder: decodeTextDocumentSyncKind },
        Encoding { toEncoder: encodeTextDocumentSyncKind },
    ]
decodeTextDocumentSyncKind =
    DecodeUtils.tryWrapDecode \val ->
        when val is
            0 -> Ok (@TextDocumentSyncKind None)
            1 -> Ok (@TextDocumentSyncKind Full)
            2 -> Ok (@TextDocumentSyncKind Incremental)
            _ -> Err TooShort

encodeTextDocumentSyncKind = \@TextDocumentSyncKind kind ->
    num =
        when kind is
            None -> 0
            Full -> 1
            Incremental -> 2
    num |> Encode.u8
textDocumentSync=\sync-> @TextDocumentSyncKind sync


CompletionOptions : {
    resolveProvider : Option Bool,
    triggerCharacters : Option (List Str),
}

ServerCapabilities : {
    textDocumentSync : Option TextDocumentSyncKind,
    completionProvider : Option CompletionOptions,
    hoverProvider : Option Bool,
    # Many many other capabilities we won't bother supporting
}

# expect
#     expected =
#         """
#         {"error":null,"id":10,"result":[{"detail":"hello there","documentation":null,"kind":1,"label":"Hi"}]}
#         """

#     response:ResponseMessage CompletionResponse
#     response = 
#         (
#             {
#                 id: some (Types.Union2.u1 10),
#                 result: some [
#                     {
#                         label: "Hi",
#                         kind: some (CompletionItemKind.from Text),
#                         detail: some "hello there",
#                         documentation: none {},
#                     },
#                 ],
#                 error: none {},
#             }

#         )
#     actual = Encode.toBytes response Core.json |> Str.fromUtf8

#     (Ok expected) == actual

params : InitializeParams
params = { processId: some (Num.toI64 100) }
testRequest = @RequestMessage
    (
        {
            id: Types.Union2.u1 10,
            params: Init params,
            method: "initialize",
        }
    )
json = \a -> a
# Request message Encode
expect
    expected = json
        # TODO: I think the fact i need to put a \ before the / in the string is a bug
        """
        {"id":10,"method":"initialize","params":{"processId":100}}
        """
    requestStr = [] |> Encode.appendWith (requestEncode testRequest) Core.json |> Str.fromUtf8

    Ok expected == requestStr
# # Request message Decode
expect
    str =
        """
        {"id":10,"method":"initialize","params":{"processId":100}}
        """
    decoded = str |> Str.toUtf8 |> Decode.fromBytes Core.json
    decoded == Ok testRequest
expect
    str =
        """
        {"method":"initialized","params":{"hi":"this"}}
        """
    paramsStr="""{"hi":"this"}"""
    decoded = str |> Str.toUtf8 |> Decode.fromBytes Core.json
    expected=
        Ok(@NotificationMessage{
                method:"initialized",
                params:(Unknown (paramsStr|>Str.toUtf8|>DelayDecode.fromJsonBytes))})
    dbg decoded

    when (expected,decoded) is
        (Ok e,Ok d)->
            dbg (notificationMessage d)
            dbg d
            d==e
        _->Bool.false


