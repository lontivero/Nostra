module Relay

open System
open System.Collections.Generic
open System.Net
open System.Threading
open Microsoft.FSharp.Control
open FsToolkit.ErrorHandling
open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Thoth.Json.Net
open Nostra
open Nostra.Event
open Nostra.Relay
open Relay.Request
open Relay.Response
open EventStore
open YoLo

type EventEvaluator = StoredEvent -> unit

module ClientRegistry =
    type ClientRegistry = {
        subscribe : IPEndPoint -> EventEvaluator -> unit
        unsubscribe : IPEndPoint -> unit
        notifyEvent : StoredEvent -> unit        
    }
    
    type ClientRegistryAction =
        | Subscribe of EndPoint * EventEvaluator
        | Unsubscribe of EndPoint
        | NotifyEvent of StoredEvent
        
    let createClientRegistry () =
        let evaluators = Dictionary<EndPoint, EventEvaluator>()

        let notifyToAll event =
            evaluators
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.iter (fun evaluator -> evaluator event)
            
        let worker =
            MailboxProcessor<ClientRegistryAction>.Start(fun inbox ->
                let rec loop () = async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | Subscribe (endpoint, evaluator) -> evaluators.Add(endpoint, evaluator) 
                    | Unsubscribe endpoint -> evaluators.Remove(endpoint) |> ignore
                    | NotifyEvent storedEvent -> notifyToAll storedEvent
                    return! loop ()
                }
                loop () )
            
        {
            subscribe = fun endpoint evaluator -> worker.Post (Subscribe (endpoint, evaluator))
            unsubscribe = fun endpoint -> worker.Post (Unsubscribe endpoint)
            notifyEvent = fun storedEvent -> worker.Post (NotifyEvent storedEvent)
        }
        
let webSocketHandler (eventStore : EventStore) (clientRegistry : ClientRegistry.ClientRegistry) (webSocket : WebSocket) (context: HttpContext) =

    let subscriptions = Dictionary<SubscriptionId, Filter list>()

    let send = 
        let worker =
            MailboxProcessor<RelayMessage * bool>.Start(fun inbox ->
                let rec loop () = async { 
                    let! (msg, fin) = inbox.Receive()
                    let! result = webSocket.send Text (toPayload msg) true
                    return! loop ()
                }
                loop () )
        worker.Post

    let sendFinal msg = send (msg, true)
    let sendAndContinue msg = send (msg, false)
    
    let notifyEvent : EventEvaluator =
        fun event ->
            subscriptions
            |> Seq.map (fun (KeyValue(s,fs)) -> s, Filter.eventMatchesAnyFilter fs event)
            |> Seq.tryFind(fun (_, m) -> m = true)
            |> Option.iter (fun (subscriptionId, _) ->
                sendFinal (RMEvent (subscriptionId, event.Serialized)))
    
    let processRequest requestText = asyncResult {
        let! request =
            deserialize requestText
            |> Result.mapError (fun e -> Exception e)

        match request with
        | CMEvent event ->
            if verify event then
                let serializedEvent = requestText[(requestText.IndexOf "{")..(requestText.LastIndexOf "}")]
                let! storedEvent = storeEvent eventStore.saveEvent eventStore.deleteEvents event serializedEvent
                clientRegistry.notifyEvent storedEvent
                return! Ok [ RMAck (event.Id, true, "added") ]
            else
                return! Ok [ RMAck (event.Id, false, "invalid: the signature is incorrect") ]
                
        | CMSubscribe(subscriptionId, filters) ->
            subscriptions.Add( subscriptionId, filters )
            let! matchingEvents = filterEvents eventStore.fetchEvents filters
            let relayMessages =
                matchingEvents
                |> List.map (fun event ->RMEvent(subscriptionId, event))

            return (RMEOSE subscriptionId) :: relayMessages  
            
        | CMUnsubscribe subscriptionId ->
            subscriptions.Remove subscriptionId |> ignore
            return! Ok []
        }

    let clientEndPoint =
        let ip = context.clientIp true []
        let port = context.clientPort true []
        IPEndPoint(ip , int port)
        
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
                    |> List.iter sendAndContinue 
                    sendFinal final 
                    return ()
            }
            |> Async.RunSynchronously
            |> function
                | Ok () -> ()
                | Error e -> sendFinal (RMNotice "Invalid message received")  

            return! loop()
        | Close, _, _ ->
            clientRegistry.unsubscribe clientEndPoint
            let emptyResponse = [||] |> ByteSegment
            do! webSocket.send Close emptyResponse true
        | _ ->
            return! loop()
    }
    
    clientRegistry.subscribe clientEndPoint notifyEvent   
    loop ()
    
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful

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