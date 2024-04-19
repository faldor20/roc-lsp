app "lang-server"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Stderr,
        pf.Task,
        Decode.{ DecodeResult },
        Core,
        Types.Union2.{ u1, u2,Union2 },
        Types.Option.{ some, none },
        LspTypes.{ RequestMessage, HoverMessage, HoverRequest, toResponse, LspMessage, notificationMessage,NotificationMessage, fromRequestMessage },
        JsonRpc,
        Handlers.{ handleRequest, handleNotification },
        ServerMessages.{ notificationEncode, makeLog, messageType },
        # Wrap.{ from, to },
    ]
    provides [main] to pf

processRequest = \req ->
    {}<-Stderr.line "handle RequestMessage"|>Task.await
    responseMsg =
        when handleRequest req.params is
            Ok response ->
                { id: some req.id, result: some response, error: none {} }

            Err err ->
                { id: some req.id, result: none {}, error: some err }
    {} <- responseMsg |> Encode.toBytes Core.json |> JsonRpc.sendBytes |> Task.await
    Task.ok Continue

processNotification = \notif ->
    {}<-Stderr.line "handle NotificationMessage"|>Task.await

    when handleNotification notif.params is
        Ok {} -> Task.ok Continue
        Err err ->
            errMsg =err|>Inspect.toStr
            log = (makeLog (messageType Error) errMsg) |> notificationEncode Core.json
            {} <- log |> JsonRpc.sendBytes |> Task.await

            Task.ok Continue
onErr=\err ->
    {}<- Stderr.line "Message loop error,continuing. Err: $(Inspect.toStr err)"|>Task.await
    Task.err (Recoverable err)

runRpc =\{}->
    {}<-Stderr.line "starting"|>Task.await
    messageBytes <- JsonRpc.messageLoop
    (
    {}<-Stderr.line "got_message"|>Task.await
    msg <- messageBytes 
        |>Decode.fromBytes Core.json 
        |>Task.fromResult 
        |>Task.onErr(onErr)
        |>Task.await

    {}<-Stderr.line "decoded_message"|>Task.await
    m:Union2 RequestMessage NotificationMessage
    m=msg
    res =
        when Types.Union2.get msg is
            U1 reqOpaque ->
                reqOpaque
                |> fromRequestMessage
                |> processRequest

            U2 notifOpaque ->
                notifOpaque |> notificationMessage |> processNotification

    res)|>Task.onErr \err-> 
        when err is 
            Recoverable e-> Task.ok Continue
            e-> Task.err e

main = 
    # {}<-Stdout.line"hi"|>Task.await
    _<-runRpc {}|>Task.onErr (\a-> Stderr.line (Inspect.toStr a))|>Task.await
    Stdout.line "goodbye!"
    





