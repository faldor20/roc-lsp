## Implements basic Lsp Types for
## Init, Hover, DidOpen, DidChange, Completion
interface RequestMessage
    exposes [
    ]
    imports [
        Types.Union2.{ Union2 },
        Types.Option.{ Option, none, some },
        Core,
        DecodeUtils,
        CompletionItemKind.{ CompletionItemKind },
    ]

RequestMessage a := {
    id : Union2 I64 Str,
    method : Str,
    # TODO: This should techincally be a union of array and object
    # BOOk: notice how we don't make it optional, We do that because we know if it exists when we differentiate types by their method
    params : a,
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
            "textDocument/init" -> decodeRequest Init
            "textDocument/hover" -> decodeRequest Hover
            "textDocument/completion" -> decodeRequest Completion
            "textDocument/didOpen" -> decodeRequest DidOpen
            "textDocument/didChange" -> decodeRequest DidChange
            _ -> { result: Err (TooShort), rest }

requestEncode = \@RequestMessage val ->
    encodeRequest=\ param->
        Encode.custom \bytes, fmt -> bytes |> Encode.append { params: param, id: val.id, method: val.method } fmt
    when val.params is
        Init a -> encodeRequest a
        Hover a -> encodeRequest a
        DidOpen a -> encodeRequest a
        DidChange a -> encodeRequest a
        Completion a -> encodeRequest a

requestMessage = \@RequestMessage req -> req
