open System
open System.Net.WebSockets
open System.Threading
open Microsoft.FSharp.Control
open Nostra.Client
open Nostra.Core
open Nostra.Core.Client
open Nostra.Client.ECashClient
open Nostra.Core.WebSocket

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

[<EntryPoint>]
let main argv =
    let uri = Uri("wss://nostr-pub.wellorder.net")

    let ws = new ClientWebSocket()
    let ctx = buildWebSocket ws
    let relay = Reader.run ctx (Communication.sender ())

    let connect = ws.ConnectAsync (uri, CancellationToken.None) |> Async.AwaitTask

    match argv[0] with
    | "wallet" ->
        let secret = Key.createNewRandom ()
        let pubkey = Key.getPubKey secret
        Console.WriteLine $"My public Key: {(pubkey |> fun x -> x.ToBytes() |> Utils.toHex)}"
        let protocolHandlerPush = protocolHandlerLoop secret relay
        let handleProtocolMessages = dispatchProtocolHandler secret protocolHandlerPush
        let protocolHandlingLoop = Reader.run ctx (Communication.startReceiving handleProtocolMessages)
        let readCommandFromUserLoop = processUserCommandLoop protocolHandlerPush
         
        async {
            do! connect
            subscribeToAnnouncements relay
            subscribeToPayments relay (XOnlyPubKey pubkey)
            subscribeToDirectMessages relay (XOnlyPubKey pubkey)
            
            do! (Async.Parallel [ protocolHandlingLoop; readCommandFromUserLoop ] |> Async.Ignore)
        } |> Async.RunSynchronously

    | "minter" ->
        let secret = Common.secretFromHex "1adafb0e2e40d3397688f8351acb92d09d5edfe532226b6920a4904123abfad9"
        let handleProtocolMessages = ECashMinter.dispatchProtocolHandler relay secret 
        let protocolHandlingLoop = Reader.run ctx (Communication.startReceiving handleProtocolMessages)

        async {
            do! connect
            ECashMinter.subscribeToDirectMessagesToMinter relay
            ECashMinter.announceParameters relay secret

            do! protocolHandlingLoop
        } |> Async.RunSynchronously
    | _ ->
        Console.WriteLine "Only 'wallet' and 'minter' are valid arguments."
    0