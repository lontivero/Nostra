open System
open System.Net.WebSockets
open System.Threading
open Microsoft.FSharp.Control
open Microsoft.VisualBasic.CompilerServices
open Nostra.Core
open Nostra.Core.Event
open Nostra.Core.Client
open Nostra.Core.Client.Request.Filter

let displayResponse = function
    | Ok (Response.RMEvent (subscriptionId, event)) ->
        let (XOnlyPubKey pubKey) = event.PubKey
        Console.ForegroundColor <- ConsoleColor.Cyan
        Console.WriteLine $"Kind: {event.Kind}  - Author: {pubKey.ToBytes() |> Utils.toHex}"
        Console.ForegroundColor <- enum<ConsoleColor> (-1)
        Console.WriteLine (event.Content.Trim())
        Console.ForegroundColor <- ConsoleColor.DarkGray
        Console.WriteLine (event.Tags |> List.map (fun (t, vs) -> $"{t}:{vs}"))
        Console.WriteLine ()

    | Ok (Response.RMACK(eventId, success, message)) ->
        Console.ForegroundColor <- ConsoleColor.Green
        let (EventId eid) = eventId 
        Console.WriteLine $"Event: {eid |> Utils.toHex} Success: {success} = {message}"
    | Ok (Response.RMNotice message) ->
        Console.ForegroundColor <- ConsoleColor.Yellow
        Console.WriteLine message
    | Ok (Response.RMEOSE subscriptionId) ->
        Console.ForegroundColor <- ConsoleColor.DarkGray
        Console.WriteLine $">>> {subscriptionId} Done"
    | Error e ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine (e.ToString())
    

open Nostra.Core.WebSocket;

let buildWebSocket (ws: ClientWebSocket) = {
    write =
        fun arr ->
            ws.SendAsync( ArraySegment(arr), WebSocketMessageType.Text, true, CancellationToken.None ) |> Async.AwaitTask
    read =
        fun buffer -> async {
            let! result = ws.ReceiveAsync(ArraySegment(buffer), CancellationToken.None) |> Async.AwaitTask
            return { Count = result.Count; EndOfMessage = result.EndOfMessage }
        }
     }
    
let Main =
    
    let secret = Key.createNewRandom ()
    
    let uri = Uri("wss://nostr-pub.wellorder.net")
    //let uri = Uri("ws://127.0.0.1:8080/")

    let ws = new ClientWebSocket()
    async {
        do! ws.ConnectAsync (uri, CancellationToken.None) |> Async.AwaitTask
        let ctx = buildWebSocket ws
        let pushToRelay = Reader.run ctx (Communication.sender ())
         
        let filter = FilterUtils.toFilter (FilterUtils.ClientFilter.AllNotes (DateTime.UtcNow.AddDays(-1)))

        Request.CMSubscribe ("all", [filter])
        |> pushToRelay

        let note =
            createNoteEvent "Hello world!"
            |> sign secret
            
        Request.CMEvent note
        |> pushToRelay

        let delete =
            createDeleteEvent [note.Id] "Because I can" 
            |> sign secret

        Request.CMEvent delete
        |> pushToRelay

        let receiveLoop = Reader.run ctx (Communication.startReceiving displayResponse)
        do! receiveLoop
       
    } |> Async.RunSynchronously

