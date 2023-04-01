namespace Nostra.Tests

open System
open Suave
open Suave.Filters
open Suave.WebSocket
open Suave.Operators
open Suave.Sockets.Control.SocketMonad

module EchoServer =
    let startEchoServer ct =
        let conf = { defaultConfig with cancellationToken = ct }
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

module Client =
    open System.Net.WebSockets
    open System.Text
    open Nostra.Client
    open Nostra.Monad

    let createClient () =
        let ws = new ClientWebSocket()
        let ctx = Communication.buildContext ws Console.Out

        let send (msg: string) =
            let payload = msg |> Encoding.UTF8.GetBytes
            ctx.WebSocket.write payload

        let receive = Communication.receiveMessage |> injectedWith ctx

        async {
            let! ct = Async.CancellationToken
            do! ws.ConnectAsync(Uri "ws://127.0.0.1:8080/", ct) |> Async.AwaitTask
            return send, receive
        }
