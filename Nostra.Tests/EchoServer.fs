namespace Nostra.Tests

open System
open System.IO
open Nostra
open Suave
open Suave.Filters
open Suave.WebSocket
open Suave.Operators
open Suave.Sockets.Control.SocketMonad

module EchoServer =
    let startEchoServer ct =
        let port = 8000 + Random.Shared.Next(2000)
        let local = Suave.Http.HttpBinding.createSimple HTTP "127.0.0.1" port

        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (
            path "/" >=> handShake (
                fun (ws: WebSocket) (context: HttpContext) ->
                    let rec loop () = socket {
                        let! msg = ws.read()
                        match msg with
                        | Text, data, true ->
                            do! ws.send Text (ArraySegment data) true
                            return! loop ()
                        | Close, _, _ ->
                            let emptyResponse = ArraySegment [||]
                            do! ws.send Close emptyResponse true
                        | _ ->
                            return! loop ()
                    }
                    loop ()
                )
        )
        Async.Start(server, ct)
        listening |> Async.RunSynchronously |> ignore
        port

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

module Relay =
    open Relay
    open Relay.Configuration
    open Nostra.Relay.InfoDocument

    let startRelayWithConfig ct (config: RelayConfig) =
        let env = buildContext config TextWriter.Null
        let wsHandler = Monad.injectedWith env (webSocketHandler ())

        let port = 8000 + Random.Shared.Next(2000)
        let local = Http.HttpBinding.createSimple HTTP "127.0.0.1" port
        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (path "/" >=> handShake wsHandler)

        Async.Start(server, ct)
        listening |> Async.RunSynchronously |> ignore
        port

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
