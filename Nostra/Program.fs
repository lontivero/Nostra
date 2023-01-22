// For more information see https://aka.ms/fsharp-console-apps
open System
open System.Net.WebSockets
open System.Threading
open Microsoft.FSharp.Control
open Nostra.Core
open Nostra.Core.Client

let printEvent (event, valid) =
    let (XOnlyPubKey pubKey) = event.PubKey
    let mark = if valid then "!" else "???"
    Console.WriteLine "---------------------------------------------------------------"
    Console.WriteLine $"{mark} Kind: {event.Kind}  - Author: {pubKey.ToBytes() |> Utils.toHex}"
    Console.WriteLine (event.Content)

let Main =
    let secret = Key.createNewRandom ()
    let pubkey = Key.getPubKey secret |> XOnlyPubKey
    let unsignedEvent = createNoteEvent "Hello world!"
    let event = sign secret unsignedEvent 
    let v = Event.verify event
    
    let uri = Uri("wss://nostr-pub.wellorder.net")
    //let uri = Uri("ws://127.0.0.1:8080/websocket")
    let ws = new ClientWebSocket()
    async {
        do! ws.ConnectAsync (uri, CancellationToken.None) |> Async.AwaitTask
        let pushToRelay = run (ws : WebSocket) (sender ())
         
        let filter = Query.toFilter (Query.CommonClientFilter.AllNotes (DateTime.UtcNow.AddDays(-1)))
        Request.CMSubscribe ("all", [filter])
        |> pushToRelay

        Request.CMEvent event
        |> pushToRelay

        let receiveLoop = run (ws : WebSocket) (startReceiving printEvent)
        do! receiveLoop
       
    } |> Async.RunSynchronously

    #if false
                Console.WriteLine (event |> eventToJson |> Json.formatWith JsonFormattingOptions.Pretty )
    
    async {
        do! ws.ConnectAsync (Uri("wss://relay.damus.io"), CancellationToken.None) |> Async.AwaitTask

        let send = Client.sender(ws)
        let subscription = Client.CMSubscribe ("misubs", [Query.DirectMessageFilter (pubkey)])
        send.Post subscription

        let dm = createEncryptedDirectMessage pubkey secret "hola"
        let signedDm = sign secret dm 
        send.Post (Client.CMEvent signedDm) 
                
        do! Client.startReceiving ws writeConsole

    } |> Async.RunSynchronously
    
    // 
    // let send = Client.sender(ws)
    // send.Post (Client.Event event)
    
    // let serializedEvent = event |> eventToJson |> Json.formatWith JsonFormattingOptions.Pretty 
    // Console.WriteLine serializedEvent
    // Console.WriteLine (verify event)
    
    // let receivedEvent = serializedEvent |> Json.parse |> jsonToEvent 
    // Console.WriteLine (verify receivedEvent)
    #endif
