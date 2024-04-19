interface ServerMessages
    exposes [
        makeLog,
        notificationEncode,
        notificationDecode,
        messageType
    ]
    imports [
        Types.Union2.{ Union2 },
        Types.Option.{ Option, none, some },
        Core,
        DecodeUtils,
        DelayDecode,
        CompletionItemKind.{ CompletionItemKind },
    ]

NotificationMsg : {
    method : Str,
    params : DelayDecode.Value Core.Json,
}


# notificationEncode:[LogMessage a, ShowMessage b] -> { method : Str, params : c }
# notificationEncode : _ -> NotificationMsg
notificationEncode = \notif ,fmt->
    encode = \method, param ->
        { method, params: param |> DelayDecode.toJsonValue }|>Encode.toBytes fmt
    when notif is
        LogMessage param -> encode "window/logMessage" param

notificationDecode : NotificationMsg -> _
notificationDecode = \msg ->
    decode = \tag -> (msg.params |> DelayDecode.decodeJsonBytes|> Result.map tag)
    when msg.method is
        "window/logMessage" -> decode LogMessage
        _ -> Ok (Custom msg.method msg.params)





MessageType := [Error, Warning, Info, Log, Debug]
    implements [
        Decoding { decoder: decodeMessageType },
        Encoding { toEncoder: encodeMessageType },
    ]

decodeMessageType =
    DecodeUtils.tryWrapDecode \val ->
        when val is
            1 -> Ok (Error)
            2 -> Ok (Warning)
            3 -> Ok (Info)
            4 -> Ok (Log)
            5 -> Ok (Debug)
            _ -> Err TooShort
    |>
    DecodeUtils.wrapSuccess @MessageType

encodeMessageType = \@MessageType kind ->
    num =
        when kind is
            Error -> 1
            Warning -> 2
            Info -> 3
            Log -> 4
            Debug -> 5
    num |> Encode.u8
messageType =\ kind -> @MessageType kind 

LogMessageParams : {
    type : MessageType,
    message : Str,
}
makeLog:_,_->[LogMessage LogMessageParams]
makeLog= \type, message -> LogMessage { type, message }

