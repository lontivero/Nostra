module Relay

open System.Collections.Generic
open System.IO
open System.Threading
open Microsoft.FSharp.Control
open FsToolkit.ErrorHandling
open Nostra
open Nostra.ClientContext
open Nostra.Event
open Nostra.Relay
open Relay.Request
open Relay.Response
open EventStore
open ClientRegistry

open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

type Context = {
    eventStore : EventStore
    clientRegistry : ClientRegistry
    logger: IOLogger
}
           

let webSocketHandler () =
    let handle (env : Context) (webSocket : WebSocket) (context: HttpContext) =
        let subscriptions = Dictionary<SubscriptionId, Filter list>()

        let send = 
            let worker =
                MailboxProcessor<RelayMessage>.Start(fun inbox ->
                    let rec loop () = async { 
                        let! msg = inbox.Receive()
                        let! result = webSocket.send Text (toPayload msg) true
                        return! loop ()
                    }
                    loop () )
            worker.Post
       
        let notifyEvent : EventEvaluator =
            fun event ->
                subscriptions
                |> Seq.map (fun (KeyValue(s,fs)) -> s, Filter.eventMatchesAnyFilter fs event)
                |> Seq.tryFind(fun (_, m) -> m = true)
                |> Option.iter (fun (subscriptionId, _) ->
                    send (RMEvent (subscriptionId, event.Serialized)))
        
        let clientId =
            let ip = context.clientIp true []
            let port = context.clientPort true []
            ClientId(ip , port)

        let processRequest req = MessageProcessing.processRequest env.eventStore subscriptions env.clientRegistry req
            
        let rec loop () = socket {
            let! msg = webSocket.read()
            match msg with
            | Text, data, true ->
                let requestText = UTF8.toString data
                env.logger.logDebug requestText
                asyncResult {
                    let! relayMessages = processRequest requestText
                    match relayMessages with
                    | [ ] -> return ()
                    | (final::messages) ->
                        messages
                        |> List.rev
                        |> List.iter send 
                        send final 
                        return ()
                }
                |> Async.RunSynchronously
                |> function
                    | Ok () -> ()
                    | Error e ->
                        env.logger.logError (e.ToString())
                        send (RMNotice "Invalid message received")  

                return! loop()
            | Close, _, _ ->
                env.clientRegistry.unsubscribe clientId
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
            | _ ->
                return! loop()
        }
        
        env.clientRegistry.subscribe clientId notifyEvent   
        loop ()
    Monad.Reader (fun (ctx : Context) -> handle ctx)
    
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful
open Thoth.Json.Net

let relayInformationDocument =
    OK <| InfoDocument.getRelayInfoDocument ()
    >=> Writers.setMimeType """application/json; charset="utf-8";"""
    >=> Writers.setHeader "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "*"
    >=> Writers.setHeader "Access-Control-Allow-Methods" "*"

let buildContext (connectionString : string) (logger: TextWriter) =
    let dbconnection = Database.connection connectionString
    Database.createTables dbconnection

    {    
        eventStore = {
            saveEvent = Database.saveEvent dbconnection
            deleteEvents = Database.deleteEvents dbconnection
            fetchEvents = Database.fetchEvents dbconnection
        }
        clientRegistry = createClientRegistry ()
        logger = {
            logInfo = logger.WriteLine
            logDebug = logger.WriteLine
            logError = logger.WriteLine
        }
    }   

open System

let app : WebPart =   
    let env = buildContext "Data Source=mydb.db" Console.Out
    let wsHandler = Monad.injectedWith env (webSocketHandler ())
    
    let handleRequest continuation (ctx : HttpContext) =
        let acceptHeader = ctx.request.header("Accept")
        let upgradeHeader = ctx.request.header("Upgrade")
        match acceptHeader, upgradeHeader with
        | Choice1Of2 "application/nostr+json", _ -> relayInformationDocument ctx
        | _, Choice1Of2 "websocket" -> handShake continuation ctx
        | _ -> OK "Use a Nostr client" ctx 
            
    choose [ 
        path "/" >=> handleRequest wsHandler
        POST >=> path "/api/req" >=> 
            fun ctx ->
                let filterResult =
                    UTF8.toString ctx.request.rawForm
                    |> Decode.fromString Filter.Decode.filter
                    
                match filterResult with
                | Ok filter ->
                    asyncResult {
                        let! events = filterEvents (env.eventStore.fetchEvents) [filter]
                        return! events
                                |> List.map Encode.string
                                |> Encode.list
                                |> Encode.toCanonicalForm
                                |> Ok                                
                    }
                    |> Async.RunSynchronously
                    |> function
                    | Ok events -> OK events ctx
                    | Error e -> ServerErrors.INTERNAL_ERROR (e.ToString()) ctx

                | Result.Error e -> BAD_REQUEST e ctx
    ]

open Suave.Logging

let loggingOptions =
  { Literate.LiterateOptions.create() with
      getLogLevelText = function Verbose->"V" | Debug->"D" | Info->"I" | Warn->"W" | Error->"E" | Fatal->"F" }

let logger =
  LiterateConsoleTarget(
    name = [|"Example"|],
    minLevel = Verbose,
    options = loggingOptions,
    outputTemplate = "[{level}] {timestampUtc:o} {message} [{source}]{exceptions}"
  ) :> Logger

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token; logger = logger }
    let listening, server = startWebServerAsync conf app

    Async.Start(server, cts.Token)
    Console.ReadKey true |> ignore  
    cts.Cancel()

    0 // return an integer exit code    