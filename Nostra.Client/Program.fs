open System
open System.Threading
open Microsoft.FSharp.Control
open Nostra
open Nostra.Client.Request
open Nostra.Client

let displayResponse = function
    | Ok (Response.RMEvent (subscriptionId, event)) ->
        let (XOnlyPubKey pubKey) = event.PubKey
        Console.ForegroundColor <- ConsoleColor.Cyan
        Console.WriteLine $"Kind: {event.Kind}  - Author: {pubKey.ToBytes() |> Utils.toHex} - {event.CreatedAt}"
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

    let uri = Uri "wss://offchain.pub"
    //let uri = Uri "wss://relay.primal.net"
    //let uri = Uri "wss://nostr.bitcoiner.social"
    //let uri = Uri("wss://nostr-pub.wellorder.net")
    //let uri = Uri "wss://relay.damus.io"
    //let uri = Uri("ws://127.0.0.1:8080/")

    let workflow = async {
        let! relay = connectToRelay uri
        let filter = Filter.notes
                    |> Filter.authors ["npub1qny3tkh0acurzla8x3zy4nhrjz5zd8l9sy9jys09umwng00manysew95gx"]
                    |> Filter.since (DateTime.Today.AddDays -5)
                    |> Filter.limit 100

        let note = Event.createNote "Hello world!" |> Event.sign secret
        let delete = Event.createDeleteEvent [note.Id] "Because I can" |> Event.sign secret

        relay.subscribe "all" [filter]
        relay.publish note
        relay.publish delete
        do! relay.startListening displayResponse
    }

    use globalCancellationTokenSource = new CancellationTokenSource()
    Async.RunSynchronously (workflow, cancellationToken= globalCancellationTokenSource.Token)

