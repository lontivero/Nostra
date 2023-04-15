namespace Nostra.Tests

open System
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
    open EventStore
    
    let startRelay port ct =
        let dbconnection = Database.connection "Data Source=:memory:"

        Database.createTables dbconnection
        
        let eventStore = {
            saveEvent = Database.saveEvent dbconnection
            deleteEvents = Database.deleteEvents dbconnection
            fetchEvents = Database.fetchEvents dbconnection
        }
        let clientRegistry = ClientRegistry.createClientRegistry ()
        
        let ws = webSocketHandler eventStore clientRegistry

        let local = Suave.Http.HttpBinding.createSimple HTTP "127.0.0.1" port
        let conf = { defaultConfig with cancellationToken = ct; bindings = [local] }
        let listening, server = startWebServerAsync conf (
            path "/" >=> handShake ws)
        Async.Start(server, ct)
        port
       
    let startRelayRandomPort ct =
        let port = 8000 + Random.Shared.Next(2000)
        startRelay port ct