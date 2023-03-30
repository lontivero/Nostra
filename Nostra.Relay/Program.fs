module Relay

open System
open System.Collections.Generic
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

let webSocketHandler (eventStore:EventStore) (webSocket : WebSocket) (context: HttpContext) =

    let subscriptions = Dictionary<SubscriptionId, Filter list>()

    let processRequest requestText = asyncResult {
        let! request =
            deserialize requestText
            |> Result.mapError (fun e -> Exception e)

        match request with
        | CMEvent event ->
            if verify event then
                eventStore.storeEvent event
                return! Ok [ RMAck (event.Id, true, "added") ]
            else
                return! Ok [ RMAck (event.Id, false, "invalid: the signature is incorrect") ]
                
        | CMSubscribe(subscriptionId, filters) ->
            subscriptions.Add( subscriptionId, filters )
            let! matchingEvents = eventStore.getEvents filters
            let eventsFromDb =
                matchingEvents
                |> List.map (fun e -> RMEvent(subscriptionId, e))
                |> List.rev
            let messages = (RMEOSE subscriptionId) :: eventsFromDb
            return! Ok messages
            
        | CMUnsubscribe subscriptionId ->
            subscriptions.Remove subscriptionId |> ignore
            return! Ok []
        }
    
    let send = 
        let worker =
            MailboxProcessor<RelayMessage * bool>.Start(fun inbox ->
                let rec loop () = async { 
                    let! (msg, fin) = inbox.Receive()
                    webSocket.send Text (toPayload msg) fin |> Async.Ignore |> Async.Start
                    return! loop ()
                }
                loop () )
        worker.Post

    let sendFinal msg = send (msg, true)
    let sendAndContinue msg = send (msg, false)
    
    let onEvents =
        eventStore.afterEventStored.Subscribe(fun event ->
            subscriptions
            |> Seq.map (fun (KeyValue(s,fs)) -> s, Filter.eventMatchesAnyFilter fs event)
            |> Seq.tryFind(fun (_, m) -> m = true)
            |> Option.iter (fun (subscriptionId, _) ->
                sendFinal (RMEvent (subscriptionId, event.Serialized))))

    let rec loop () = socket {
        let! msg = webSocket.read()
        match msg with
        | Text, data, true ->
            let requestText = UTF8.toString data
            let r = asyncResult {
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
            r |> Async.RunSynchronously |> ignore
            return! loop()
        | Close, _, _ ->
            onEvents.Dispose()
            let emptyResponse = [||] |> ByteSegment
            do! webSocket.send Close emptyResponse true
        | _ ->
            return! loop()
    }
    
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
    let saveEvent = Database.saveEvent dbconnection
    let deleteEvents = Database.deleteEvents dbconnection
    let fetchEvents = Database.fetchEvents dbconnection
    
    let eventStore = EventStore(saveEvent, deleteEvents, fetchEvents)
    let ws = webSocketHandler eventStore

    let handle fCont (ctx : HttpContext) =
        let acceptHeader = ctx.request.header("Accept")
        let upgradeHeader = ctx.request.header("Upgrade")
        match acceptHeader, upgradeHeader with
        | Choice1Of2 "application/nostr+json", _ -> relayInformationDocument ctx
        | _, Choice1Of2 "websocket" -> handShake fCont ctx
        | _ -> OK "Use a Nostr client" ctx 
            
    choose [
        path "/" >=> handle ws
        POST >=> path "/api/req" >=> 
            fun ctx ->
                let filterResult =
                    UTF8.toString ctx.request.rawForm
                    |> Decode.fromString Filter.Decode.filter
                    
                match filterResult with
                | Ok filter ->
                    asyncResult {
                        let! events = eventStore.getEvents [filter]
                        return events
                        |> List.map Encode.string
                        |> Encode.list
                        |> Encode.toCanonicalForm
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