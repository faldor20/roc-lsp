interface JsonRpc
    exposes [messageLoop,  sendBytes, readMessage]
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task,
        pf.Stderr,
        Core,
        LspTypes.{  RequestMessage, ResponseMessage,fromRequestMessage },
        Types.Option,
    ]

##====JSONRPC Implimentation====##

##Converts a result to a task then awaits it
awaitResult = \res, next -> res |> Task.fromResult |> Task.await next
# sendMessage a:
# sendMessage = \messageObj ->
#     messageBytes = messageObj |> Encode.toBytes Core.json
#     sendBytes messageBytes

sendBytes = \messageBytes ->
    len = messageBytes |> List.len
    messageStr <- messageBytes |> Str.fromUtf8 |> awaitResult
    msg = "Content-Length: $(len |> Num.toStr)\r\n\r\n$(messageStr)"
    Stdout.write msg

## Continueously handle incoming messages
messageLoop : (List U8 -> Task.Task [Continue, Exit] _) -> _
messageLoop = \messageHandler ->
    Task.loop [] \leftOver ->
        { content, leftOver: nextLeftOver } <- readMessage leftOver |> Task.await
        {}<-Stderr.line "handling Message" |>Task.await
        continue <- messageHandler content |> Task.map
        when continue is
            Exit -> Done []
            Continue -> Step nextLeftOver
    |>Task.map \_->{}

readMessage = \partialMessage ->
    # This is slow, we don't need to check the whole thing, just the new message with 3 chars from the previous message appended at the start, so the last 256+3 (259)

    # reads the message untill we find a message start \r\n\r\n
    {}<-Stderr.line "reading message" |>Task.await
    message <- readTill partialMessage (\msg -> (msg |> List.walkUntil [] matchContentStart) == ['\r', '\n', '\r', '\n']) |> Task.await
    {}<-Stderr.line "read message" |>Task.await
    # now we try to parse
    message |> parseMessage

matchContentStart = \state, char ->
    when (state, char) is
        (['\r', '\n', '\r'], '\n') -> Break (state |> List.append char)
        (['\r', '\n'], '\r')
        | (['\r'], '\n')
        | ([], '\r') -> Continue (state |> List.append char)

        _ -> Continue []

readTill = \message, pred ->
    Task.loop message \msg ->
        bytes <- Stdin.bytes |> Task.map
        newMsg = msg |> List.concat bytes
        if pred newMsg then
            Done newMsg
        else
            Step newMsg

readTillAtLeastLen = \msg, len -> readTill msg \newMsg -> List.len newMsg >= len
# TODO!: header is ascii encoded
parseHeader : _ -> Result (U64, Str) _
parseHeader = \message ->
    { before: header, after } <-
        message
        |> Str.fromUtf8
        |> Result.try (\s -> Str.splitFirst s "\r\n\r\n")
        |> Result.try
    length <- getContentLength header |> Result.map
    (length, after)

parseMessage : List U8 -> _
parseMessage = \message ->
    (length, rest) <- parseHeader message |> awaitResult
    {}<-Stderr.line "parsed header,next parsing Message" |>Task.await
    read <- (rest |> Str.toUtf8 |> readTillAtLeastLen length) |> Task.map
    { before: content, others: leftOver } = read |> List.split length
    { content, leftOver }

## Get's the content lenght header
## Tolerant of having an unparsed body from a malformed message in the header section because it looks from the end of the text we think is a header
getContentLength = \header ->
    headers = header |> Str.split "\r\n"
    contentHeaderName = "Content-Length: "
    # we do contians here because if we failed to parse the last body it might be stuck at the end of this header
    dbg headers

    contentHeader = headers |> List.findFirst \a -> a |> Str.contains contentHeaderName
    when contentHeader is
        Err _ -> Err NoContentHeader
        Ok cHead ->
            # Because we might have some junk before this header we just keep anything after the header name
            cHead |> Str.splitLast contentHeaderName |> Result.try \split -> split.after |> Str.toU64

makeTest = \content ->
    length = content |> Str.countUtf8Bytes
    """
    Content-Length: $(length |> Num.toStr)\r\n\r\n$(content)
    """
hoverJson =
    """
    {"jsonrpc":"2.0","method":"textDocument/hover","params":{"position":{"character":0,"line":5},"textDocument":{"uri":"file:///home/eli/Code/roc/langServer/main.roc"}},"id":1}        
    """
expect
    input = makeTest hoverJson |> Str.toUtf8
    res =
        (length, after) <- parseHeader input |> Result.try
        { before: content, others: leftOver } = after |> Str.toUtf8 |> List.split length
        msg <- (content) |> Decode.fromBytes Core.json |> Result.map
        msgDat : RequestMessage
        msgDat = msg
        when (fromRequestMessage msgDat).params is
            Hover a ->
                leftOver == [] && (a.position.line == 5)

            _ -> Bool.false

    res == Ok (Bool.true)

