namespace Nostra.Tests

open System
open System.IO
open Nostra
open Suave
open Suave.Filters
open Suave.WebSocket
open Suave.Operators
open Suave.Sockets.Control

module EchoServer =
    let startEchoServer ct =
        let local = HttpBinding.createSimple HTTP "127.0.0.1" 0

        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (
            path "/" >=> handShake (
                fun (ws: WebSocket) (context: HttpContext) ->
                    let rec loop () = socket {
                        let! msg = ws.read()
                        match msg with
                        | Text, data, true ->
                            do! ws.send Text data true
                            return! loop ()
                        | Close, _, _ ->
                            let emptyResponse = Memory<byte>.Empty
                            do! ws.send Close emptyResponse true
                        | _ ->
                            return! loop ()
                    }
                    loop ()
                )
        )
        let startedData = listening |> Async.RunSynchronously
        int startedData[0].Value.binding.port

module Client =
    open System.Net.WebSockets
    open System.Text
    open Nostra.Client
    open Nostra.Monad

    let createClient port =
        let ws = new ClientWebSocket()
        let ctx = Communication.buildContext ws Console.Out
        let send (msg: string) =
            let payload = msg |> Encoding.UTF8.GetBytes
            ctx.WebSocket.write payload
        let receive = Communication.receiveMessage |> injectedWith ctx
        async {
            let! ct = Async.CancellationToken
            do! ws.ConnectAsync (Uri $"ws://127.0.0.1:{port}/", ct) |> Async.AwaitTask
            return send, receive
        }

    let createClientDefaultPort () =
        createClient 8080

    let createClientWithFragmentedSend port =
        let ws = new ClientWebSocket()
        let ctx = Communication.buildContext ws Console.Out
        let send (msg: string) =
            let payload = msg |> Encoding.UTF8.GetBytes
            ctx.WebSocket.write payload
        let sendFragmented (fragments: string list) = async {
            let! ct = Async.CancellationToken
            match fragments with
            | [] -> ()
            | [single] ->
                let payload = Encoding.UTF8.GetBytes(single: string)
                do! ws.SendAsync(ArraySegment(payload), WebSocketMessageType.Text, true, ct) |> Async.AwaitTask
            | first :: rest ->
                // Send first fragment with fin=false
                let firstPayload = Encoding.UTF8.GetBytes(first: string)
                do! ws.SendAsync(ArraySegment(firstPayload), WebSocketMessageType.Text, false, ct) |> Async.AwaitTask
                // Send continuation fragments
                let rec sendMore remaining = async {
                    match remaining with
                    | [] -> ()
                    | [last] ->
                        // Last fragment with fin=true
                        let lastPayload = Encoding.UTF8.GetBytes(last: string)
                        do! ws.SendAsync(ArraySegment(lastPayload), WebSocketMessageType.Text, true, ct) |> Async.AwaitTask
                    | middle :: more ->
                        let middlePayload = Encoding.UTF8.GetBytes(middle: string)
                        do! ws.SendAsync(ArraySegment(middlePayload), WebSocketMessageType.Text, false, ct) |> Async.AwaitTask
                        do! sendMore more
                }
                do! sendMore rest
        }
        let receive = Communication.receiveMessage |> injectedWith ctx
        async {
            let! ct = Async.CancellationToken
            do! ws.ConnectAsync (Uri $"ws://127.0.0.1:{port}/", ct) |> Async.AwaitTask
            return send, sendFragmented, receive
        }

module Relay =
    open Relay
    open Relay.Configuration
    open Nostra.Relay.InfoDocument

    let startRelayWithConfig ct (config: RelayConfig) =
        let env = buildContext config TextWriter.Null
        let wsHandler = Monad.injectedWith env (webSocketHandler ())

        let local = HttpBinding.createSimple HTTP "127.0.0.1" 0
        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (path "/" >=> handShake wsHandler)

        let startedData = listening |> Async.RunSynchronously
        int startedData[0].Value.binding.port

    let startRelay ct =
        let uniqueDb = $"file:test{Guid.NewGuid():N}?mode=memory&cache=shared"
        let config = { RelayConfig.defaults with DatabasePath = uniqueDb }
        startRelayWithConfig ct config

    let startRelayWithLimitations ct (limitations: Limitation) =
        let uniqueDb = $"file:test{Guid.NewGuid():N}?mode=memory&cache=shared"
        let relayInfo = { RelayInfo.defaults with Limitation = limitations }
        let config = { RelayConfig.defaults with DatabasePath = uniqueDb; RelayInfo = relayInfo }
        startRelayWithConfig ct config

    let startRelayWithWritePolicy ct (writePolicy: WritePolicy) =
        let uniqueDb = $"file:test{Guid.NewGuid():N}?mode=memory&cache=shared"
        let config = { RelayConfig.defaults with DatabasePath = uniqueDb; WritePolicy = writePolicy }
        startRelayWithConfig ct config
