namespace Nostra.Relay.Tests

open System
open System.IO
open Nostra
open Suave
open Suave.Filters
open Suave.WebSocket
open Suave.Operators
open Suave.Sockets.Control


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
        let receiveRaw = Communication.receiveMessage |> injectedWith ctx
        let receive = async {
            let! result = receiveRaw
            return result |> Option.defaultValue (Result.Error "WebSocket closed")
        }
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
        let receiveRaw = Communication.receiveMessage |> injectedWith ctx
        let receive = async {
            let! result = receiveRaw
            return result |> Option.defaultValue (Result.Error "WebSocket closed")
        }
        async {
            let! ct = Async.CancellationToken
            do! ws.ConnectAsync (Uri $"ws://127.0.0.1:{port}/", ct) |> Async.AwaitTask
            return send, sendFragmented, receive
        }

module Relay =
    open Relay
    open Relay.Configuration
    open Nostra.Relay.InfoDocument

    let relayInformationDocument (relayInfo: RelayInfo) =
        Successful.OK <| InfoDocument.getRelayInfoDocument relayInfo
        >=> Writers.setMimeType """application/json; charset="utf-8";"""
        >=> Writers.setHeader "Access-Control-Allow-Origin" "*"
        >=> Writers.setHeader "Access-Control-Allow-Headers" "*"
        >=> Writers.setHeader "Access-Control-Allow-Methods" "*"

    let startRelayWithConfig ct (config: RelayConfig) =
        let env = buildContext config TextWriter.Null
        let wsHandler = Monad.injectedWith env (webSocketHandler ())

        let handleRequest continuation (ctx : HttpContext) =
            let acceptHeader = ctx.request.header("Accept")
            let upgradeHeader = ctx.request.header("Upgrade")
            match acceptHeader, upgradeHeader with
            | Choice1Of2 "application/nostr+json", _ -> relayInformationDocument config.RelayInfo ctx
            | _, Choice1Of2 "websocket" -> handShake continuation ctx
            | _ -> Successful.OK "Use a Nostr client" ctx

        let local = HttpBinding.createSimple HTTP "127.0.0.1" 0
        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (path "/" >=> handleRequest wsHandler)

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

    let startRelayWithLogger ct (logger: TextWriter) =
        let uniqueDb = $"file:test{Guid.NewGuid():N}?mode=memory&cache=shared"
        let config = { RelayConfig.defaults with DatabasePath = uniqueDb; LogLevel = LogLevel.Debug }
        let env = buildContext config logger
        let wsHandler = Monad.injectedWith env (webSocketHandler ())

        let local = HttpBinding.createSimple HTTP "127.0.0.1" 0
        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (path "/" >=> handShake wsHandler)

        let startedData = listening |> Async.RunSynchronously
        int startedData[0].Value.binding.port
