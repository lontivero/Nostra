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
    
    //let uri = Uri("wss://nostr-pub.wellorder.net")
    let uri = Uri("ws://127.0.0.1:8080/websocket")
    let ws = new ClientWebSocket()
    async {
        do! ws.ConnectAsync (uri, CancellationToken.None) |> Async.AwaitTask
        let pushToRelay = Monad.run (ws : WebSocket) (Client.sender ())
         
        let filter = Filter.toFilter (Filter.CommonClientFilter.AllNotes (DateTime.UtcNow.AddDays(-1)))
        Request.CMSubscribe ("all", [filter])
        |> pushToRelay

        Request.CMEvent event
        |> pushToRelay

        let receiveLoop = Monad.run (ws : WebSocket) (Client.startReceiving printEvent)
        do! receiveLoop
       
    } |> Async.RunSynchronously

