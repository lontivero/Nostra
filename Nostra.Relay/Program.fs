open System
open System.Collections.Generic
open System.Threading
open Microsoft.FSharp.Control
open Nostra.Core
open Nostra.Core.Client
open Nostra.Core.Relay
open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Relay.Request
open Relay.Response
open Thoth.Json.Net

let ws (eventStore:EventStore) (webSocket : WebSocket) (context: HttpContext) =
    let subscriptions = Dictionary<SubscriptionId, Filter list>()

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
                sendFinal (RMEvent (subscriptionId, event.Event))))

    socket {
        let mutable loop = true

        while loop do
            let! msg = webSocket.read()

            match msg with
            | Text, data, true ->
                let requestText = UTF8.toString data

                let msgFromClientResult = Request.deserialize requestText
                match msgFromClientResult with
                | Error _ ->
                    sendFinal <| RMNotice "The message was not deserialized"
                | Ok msgFromClient ->
                    match msgFromClient with
                    | CMEvent event ->
                        if verify event then
                            eventStore.storeEvent event
                            sendFinal <| RMACK (event.Id, true, "added")
                        else
                            sendFinal <| RMACK (event.Id, false, "invalid: the signature is not valid")
                    | CMSubscribe(subscriptionId, filters) ->
                        let subscriptionFilters = filters 
                        subscriptions.Add( subscriptionId, subscriptionFilters )
                        let matchingEvents = eventStore.getEvents subscriptionFilters 
                        matchingEvents
                        |> List.map (fun e -> RMEvent( subscriptionId, e))
                        |> List.iter sendAndContinue
                        sendFinal <| RMEOSE subscriptionId

                    | CMUnsubscribe subscriptionId ->
                        subscriptions.Remove subscriptionId |> ignore
            | Close, _, _ ->
                onEvents.Dispose()
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
      }
    
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
    let webSocketHandler = ws (EventStore())

    let handle fCont (ctx : HttpContext) =
        let acceptHeader = ctx.request.header("Accept")
        let upgradeHeader = ctx.request.header("Upgrade")
        match acceptHeader, upgradeHeader with
        | Choice1Of2 "application/nostr+json", _ -> relayInformationDocument ctx
        | _, Choice1Of2 "websocket" -> handShake fCont ctx
        | _ -> OK "Use a Nostr client" ctx 
            
    choose [
        path "/" >=> handle webSocketHandler
        NOT_FOUND "Found no handlers."
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