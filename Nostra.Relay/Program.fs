module Relay

open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Threading
open Microsoft.Data.Sqlite
open Microsoft.FSharp.Control
open FsToolkit.ErrorHandling
open Nostra
open Nostra.ClientContext
open Nostra.Relay
open Relay.Request
open Relay.Response
open EventStore
open ClientRegistry
open MessageProcessing

open System.Text
open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

[<TailCall>]
let rec processRelayMessagesLoop
        (webSocket: WebSocket)
        (cleanup: unit -> unit)
        (inbox: MailboxProcessor<RelayMessage>) = async {
    let! msg = inbox.Receive()
    try
        let send = webSocket.send Text (toPayload msg) true
        let! _ = send.AsTask() |> Async.AwaitTask
        return! processRelayMessagesLoop webSocket cleanup inbox
    with _ ->
        cleanup ()
}

let private processCompleteMessage
        (env: Context)
        (send: RelayMessage -> unit)
        (processRequest: string -> Async<Result<RelayMessage list,EventProcessingError>>)
        (requestText: string) =
    processRequest requestText
    |> AsyncResult.map (function
        | [ ] -> ()
        | final::messages ->
            messages
            |> List.rev
            |> List.iter send
            send final
            ())
    |> Async.map (Result.defaultWith (function
        | BusinessError e ->
            send e
        | UnexpectedError e ->
            env.logger.logError (e.ToString())
            send (RMNotice "unexpected error")))
    |> SocketOp.ofAsync

[<TailCall>]
let rec processRequestLoop
        (clientId: ClientId)
        (webSocket: WebSocket)
        (env: Context)
        (send: RelayMessage -> unit)
        (cleanup: unit -> unit)
        (processRequest: string -> Async<Result<RelayMessage list,EventProcessingError>>)
        (fragmentBuffer: ResizeArray<byte>) = socket {
    let! msg = webSocket.read()
    match msg with
    | Text, data, true when fragmentBuffer.Count = 0 ->
        // Complete message, no pending fragments
        let requestText = Encoding.UTF8.GetString(data.Span)
        do! processCompleteMessage env send processRequest requestText
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Text, data, true ->
        // Final fragment - combine with buffer and process
        fragmentBuffer.AddRange(data.ToArray())
        let requestText = Encoding.UTF8.GetString(fragmentBuffer.ToArray())
        fragmentBuffer.Clear()
        do! processCompleteMessage env send processRequest requestText
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Text, data, false ->
        // First fragment of a fragmented message
        fragmentBuffer.Clear()
        fragmentBuffer.AddRange(data.ToArray())
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Continuation, data, false ->
        // Continuation fragment, not final
        fragmentBuffer.AddRange(data.ToArray())
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Continuation, data, true ->
        // Final continuation fragment - combine and process
        fragmentBuffer.AddRange(data.ToArray())
        let requestText = Encoding.UTF8.GetString(fragmentBuffer.ToArray())
        fragmentBuffer.Clear()
        do! processCompleteMessage env send processRequest requestText
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Close, _, _ ->
        cleanup ()
        let emptyResponse = System.Memory<byte>.Empty
        do! webSocket.send Close emptyResponse true
    | Ping, data, _ ->
        do! webSocket.send Pong data true
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Pong, _, _ ->
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | Binary, _, _ ->
        env.logger.logDebug "Ignoring binary WebSocket frame"
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    | opcode, _, _ ->
        env.logger.logWarn $"Unexpected WebSocket opcode: {opcode}"
        return! processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
}
let webSocketHandler () =
    let handle (env : Context) (webSocket : WebSocket) (context: HttpContext) =
        let subscriptions = Dictionary<SubscriptionId, Filter list>()

        let clientId =
            let ip = context.clientIp true ["127.0.0.1"]
            let port = context.clientPort true ["127.0.0.1"]
            ClientId(ip , port)

        let cleanup () =
            env.clientRegistry.unsubscribe clientId

        let worker =
            MailboxProcessor<RelayMessage>.Start(processRelayMessagesLoop webSocket cleanup)

        let send msg = worker.Post msg

        let notifyEvent : EventEvaluator =
            fun event ->
                subscriptions
                |> Seq.map (fun (KeyValue(s,fs)) -> s, Filter.eventMatchesAnyFilter fs event)
                |> Seq.tryFind(fun (_, m) -> m = true)
                |> Option.iter (fun (subscriptionId, _) ->
                    send (RMEvent (subscriptionId, event.Serialized)))

        let processRequest req = processRequest env clientId subscriptions req

        env.clientRegistry.subscribe clientId notifyEvent
        let fragmentBuffer = ResizeArray<byte>()
        processRequestLoop clientId webSocket env send cleanup processRequest fragmentBuffer
    Monad.Reader (fun (ctx : Context) -> handle ctx)

open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful
open Thoth.Json.Net
open Relay.Configuration
open Relay.InfoDocument
open Nostra.Relay.Plugin

let relayInformationDocument (relayInfo: RelayInfo) =
    OK <| InfoDocument.getRelayInfoDocument relayInfo
    >=> Writers.setMimeType """application/json; charset="utf-8";"""
    >=> Writers.setHeader "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "*"
    >=> Writers.setHeader "Access-Control-Allow-Methods" "*"

let buildContext (config: RelayConfig) (logger: TextWriter) =
    let connectionString = $"Data Source={config.DatabasePath}"
    let connectionFactory () =
        let conn = new SqliteConnection(connectionString)
        conn.Open()
        conn

    use initConn = connectionFactory ()
    Database.createTables (Fumble.Sql.existingConnection initConn)

    let limits = config.RelayInfo.Limitation
    let ifEnabled minLevel action =
        if minLevel >= config.LogLevel then action else ignore

    let pluginManager =
        match config.WritePolicy.Plugin with
        | Some cmd -> createPluginManager cmd
        | None -> createAcceptAllPluginManager ()

    {
        eventStore = {
            saveEvent = Database.saveEvent connectionFactory
            deleteEvents = Database.deleteEvents connectionFactory
            fetchEvents = Database.fetchEvents connectionFactory limits.DefaultLimit limits.MaxLimit
            countEvents = Database.countEvents connectionFactory limits.DefaultLimit limits.MaxLimit
        }
        clientRegistry = createClientRegistry ()
        logger = {
            logInfo =  ifEnabled LogLevel.Info logger.WriteLine
            logWarn = ifEnabled LogLevel.Warn logger.WriteLine
            logDebug = ifEnabled LogLevel.Debug logger.WriteLine
            logError = ifEnabled LogLevel.Error logger.WriteLine
        }
        config = config
        pluginManager = pluginManager
    }

open System

let app (config: RelayConfig) : WebPart =
    let env = buildContext config Console.Out
    let wsHandler = Monad.injectedWith env (webSocketHandler ())

    let handleRequest continuation (ctx : HttpContext) =
        let acceptHeader = ctx.request.header("Accept")
        let upgradeHeader = ctx.request.header("Upgrade")
        match acceptHeader, upgradeHeader with
        | Choice1Of2 "application/nostr+json", _ -> relayInformationDocument env.config.RelayInfo ctx
        | _, Choice1Of2 "websocket" -> handShake continuation ctx
        | _ -> OK "Use a Nostr client" ctx

    choose [
        path "/" >=> handleRequest wsHandler
        POST >=> path "/api/req" >=>
            fun ctx ->
                let filterResult =
                    Encoding.UTF8.GetString(ctx.request.rawForm)
                    |> Decode.fromString Filter.Decode.filter

                match filterResult with
                | Ok filter ->
                    asyncResult {
                        let! events = filterEvents (env.eventStore.fetchEvents) [filter] DateTime.UtcNow
                        return! events
                                |> List.map Encode.string
                                |> Encode.list
                                |> Encode.toCanonicalForm
                                |> Ok
                    }
                    |> Async.RunSynchronously
                    |> function
                    | Ok events -> OK events ctx
                    | Result.Error e -> ServerErrors.INTERNAL_ERROR (e.ToString()) ctx

                | Result.Error e -> BAD_REQUEST e ctx
    ]

let getDefaultDataDirectory () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "Nostra")
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "Library", "Application Support", "Nostra")
    else
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".nostra")

[<EntryPoint>]
let main argv =
    let args = argv |> Array.toList

    let dataDir, remainingArgs =
        match args with
        | "--datadir" :: dir :: rest -> dir, rest
        | _ -> getDefaultDataDirectory (), args

    Directory.CreateDirectory(dataDir) |> ignore

    let configPath =
        match remainingArgs with
        | path :: _ when not (path.StartsWith("--")) -> path
        | _ -> Path.Combine(dataDir, "config.json")

    let config = RelayConfig.load configPath
    let config =
        if Path.IsPathRooted config.DatabasePath then config
        else { config with DatabasePath = Path.Combine(dataDir, config.DatabasePath) }

    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    startWebServer conf (app config)
    0