module Relay

open System.Collections.Generic
open System.Threading
open Microsoft.FSharp.Control
open FsToolkit.ErrorHandling
open Nostra
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
        
let webSocketHandler (eventStore : EventStore) (clientRegistry : ClientRegistry) (webSocket : WebSocket) (context: HttpContext) =

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

    let processRequest req = MessageProcessing.processRequest eventStore subscriptions clientRegistry req
        
    let rec loop () = socket {
        let! msg = webSocket.read()
        match msg with
        | Text, data, true ->
            let requestText = UTF8.toString data
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
                | Error e -> send (RMNotice "Invalid message received")  

            return! loop()
        | Close, _, _ ->
            clientRegistry.unsubscribe clientId
            let emptyResponse = [||] |> ByteSegment
            do! webSocket.send Close emptyResponse true
        | _ ->
            return! loop()
    }
    
    clientRegistry.subscribe clientId notifyEvent   
    loop ()
    
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

let app : WebPart =
    // let dbconnection = Database.connection "Data Source=:memory:"
    let dbconnection = Database.connection "Data Source=mydb.db"

    Database.createTables dbconnection
    
    let eventStore = {
        saveEvent = Database.saveEvent dbconnection
        deleteEvents = Database.deleteEvents dbconnection
        fetchEvents = Database.fetchEvents dbconnection
    }
    let clientRegistry = ClientRegistry.createClientRegistry ()
    
    let ws = webSocketHandler eventStore clientRegistry

    let handleRequest continuation (ctx : HttpContext) =
        let acceptHeader = ctx.request.header("Accept")
        let upgradeHeader = ctx.request.header("Upgrade")
        match acceptHeader, upgradeHeader with
        | Choice1Of2 "application/nostr+json", _ -> relayInformationDocument ctx
        | _, Choice1Of2 "websocket" -> handShake continuation ctx
        | _ -> OK "Use a Nostr client" ctx 
            
    choose [
        path "/" >=> handleRequest ws
        POST >=> path "/api/req" >=> 
            fun ctx ->
                let filterResult =
                    UTF8.toString ctx.request.rawForm
                    |> Decode.fromString Filter.Decode.filter
                    
                match filterResult with
                | Ok filter ->
                    asyncResult {
                        let! events = filterEvents (eventStore.fetchEvents) [filter]
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

open System

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync conf app

    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore  
    cts.Cancel()

    0 // return an integer exit code    