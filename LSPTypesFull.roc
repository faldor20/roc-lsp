interface LSPTypesFull
    exposes [
        RequestMessage,
        ResponseMessage,
        responseMessage,
        requestMessage,
    ]
    imports [
        Types.Union2.{ Union2 },
        Types.Option.{ Option, none, some },
        Core,
        DecodeUtils,
        CompletionItemKind.{CompletionItemKind}
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
    DecodeUtils.wrapDecode \str ->
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

RequestMessageIntern a : {
    id : Union2 I64 Str,
    method : Str,
    # TODO: This should techincally be a union of array and object
    # BOOk: notice how we don't make it optional, We do that because we know if it exists when we differentiate types by their method
    params : a,
}
HoverParams : {
    textDocument : TextDocumentIdentifier,
    position : Position,
    workDoneToken : Option (Union2 I64 Str),
}

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
    ]

decodeCompletionTriggerKind =
    ok = \tag -> Ok (@CompletionTriggerKind tag)
    DecodeUtils.wrapDecode \val ->
        when val is
            1 -> ok Invoked
            2 -> ok TriggerCharacter
            3 -> ok TriggerForIncompleteCompletions
            _ -> Err TooShort

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

CompletionList : {
    isIncomplete : Bool,

    itemDefaults: Option {
        commitCharacters: Option (List Str),
        editRange:Option (Union2 Range  {
        insert: Range,
        replace: Range,
        }),
        insertTextFormat: Option InsertTextFormat,
        insertTextMode:Option InsertTextMode,
        data: Option LSPAny
        },
    items : List CompletionItem,
}

CompletionItemLabelDetails : {
    detail : Option Str,
    description : Option Str,
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

DidOpenTextDocumentParams : {
    ## The document that was opened.
    textDocument : TextDocumentItem,
}

# HoverRequest : RequestMessageIntern HoverParams
# CompletionRequest : RequestMessageIntern CompletionParams

RequestMessage := [
    Hover (RequestMessageIntern HoverParams),
    DidOpen (RequestMessageIntern DidOpenTextDocumentParams),
    Completion (RequestMessageIntern CompletionParams),
]
    implements [
        Decoding {
            decoder: decodeRequestMessage,
        },
    ]
decodeRequestMessage = Decode.custom \bytes, fmt ->
    decodeRequest = \requestType ->
        Decode.fromBytesPartial bytes fmt
        |> Decode.mapResult \res -> @RequestMessage (requestType res)

    Decode.decodeWith bytes Decode.decoder fmt
    |> DecodeUtils.tryResult \res, rest ->
        when res.method is
            "textDocument/hover" -> decodeRequest Hover
            "textDocument/completion" -> decodeRequest Completion
            "textDocument/didOpen" -> decodeRequest DidOpen
            _ -> { result: Err (TooShort), rest }
requestMessage = \@RequestMessage req -> req

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
        Ok (@RequestMessage (Hover hover)) ->
            hover.params.position == (position 5 0)

        _ -> Bool.false

# RequestMessage should be opaque
# It will have its own decoder.
# In the decoder we will decide which Request it should decode to
# It will return a tag union of all the possible types

ResponseMessageIntern a : {
    id : Option (Union2 I64 Str),
    result : Option a,
    # TODO: This should techincally be a union of array and object
    error : Option ResponseErr,
}

ResponseErr : {
    code : I64,
}

ResponseMessage := [
    Hover (ResponseMessageIntern HoverResponse),
    Completion (ResponseMessageIntern CompletionResponse),
]
    implements [
        # Decoding {
        #     decoder: decodeRequestMessage,
        # },
        Encoding {
            toEncoder: responseToEncoder,
        },
    ]
responseToEncoder : ResponseMessage -> _
responseToEncoder = \@ResponseMessage val ->
    when val is
        Hover a ->
            # Encode.toEncoder
            Encode.custom \bytes, fmt -> bytes |> Encode.append a fmt

        Completion a ->
            # Encode.toEncoder
            Encode.custom \bytes, fmt -> bytes |> Encode.append a fmt

responseMessage = \@ResponseMessage req -> req
# decodeResponseMessage = Decode.custom \bytes, fmt ->
#     decodeResponse = \requestType ->
#         Decode.fromBytesPartial bytes fmt
#         |> Decode.mapResult \res -> @ResponseMessage (requestType res)

#     Decode.decodeWith bytes Decode.decoder fmt
#     |> tryResult \res, rest ->
#         when res.method is
#             "textDocument/hover" -> decodeResponse Hover
#             _ -> { result: Err (TooShort), rest }

HoverResponse : {
    ##The hover's content
    # Note, usually you can return a markedString or a markedString list,or markupcontent we will only return a markupContent for simplicity
    contents : MarkupContent,

    ## An optional range is a range inside a text document
    ## that is used to visualize a hover, e.g. by changing the background color.
    range : Option Range,
}

CompletionResponse : List CompletionItem

expect
    expected =
        """
        {"error":null,"id":10,"result":[{"detail":"hello there","documentation":null,"kind":1,"label":"Hi"}]}
        """

    response = @ResponseMessage
        (
            Completion {
                id: some (Types.Union2.u1 10),
                result: some [
                    {
                        label: "Hi",
                        kind: some (CompletionItemKind.from Text),
                        detail: some "hello there",
                        documentation: none {},
                    },
                ],
                error: none {},
            }

        )
    actual = Encode.toBytes response Core.json |> Str.fromUtf8

    (Ok expected) == actual
