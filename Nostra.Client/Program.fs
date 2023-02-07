open System
open System.Net.WebSockets
open System.Threading
open Microsoft.FSharp.Control
open Nostra
open Nostra.Event
open Nostra.Client
open Nostra.Client.Request.Filter

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
    

let Main =
    
    let secret = Key.createNewRandom ()
    
    let uri = Uri("wss://nostr-pub.wellorder.net")
    //let uri = Uri("ws://127.0.0.1:8080/")

    let ws = new ClientWebSocket()
    let ctx = Communication.buildContext ws Console.Out
    let pushToRelay = Monad.injectedWith ctx (Communication.sender ())
    let receiveLoop = Monad.injectedWith ctx (Communication.startReceiving displayResponse)
    let filter = FilterUtils.toFilter (FilterUtils.ClientFilter.AllNotes (DateTime.UtcNow.AddDays(-1)))

    let workflow = async {
        do! ws.ConnectAsync (uri, CancellationToken.None) |> Async.AwaitTask
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

        do! receiveLoop
    }
    
    use globalCancellationTokenSource = new CancellationTokenSource()
    Async.RunSynchronously (workflow, cancellationToken= globalCancellationTokenSource.Token)

