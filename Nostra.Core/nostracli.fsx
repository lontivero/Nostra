#r "nuget:NBitcoin.Secp256k1"
#r "nuget:Thoth.Json.Net"

#load "Utils.fs"
#load "Types.fs"
#load "Event.fs"
#load "Client.fs"

#nowarn "2303" // this bug is fixed now but not released yet


open System
open System.Net.WebSockets
open System.Threading
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open NBitcoin.Secp256k1
open Nostra.Core

module CommandLine =
    open System.Text.RegularExpressions
    
    let (|Command|_|) (s:string) =
        let r = new Regex(@"^(?:-{1,2}|\/)(?<command>\w+)[=:]*(?<value>.*)$",RegexOptions.IgnoreCase)
        let m = r.Match(s)
        if m.Success then
           Some(m.Groups.["command"].Value.ToLower(), m.Groups.["value"].Value)
        else
           None

    let parseArgs (args:string seq) =
       args 
       |> Seq.map (
             function 
             | Command (switch, value) -> (switch, value)
             | flag -> (flag, ""))
       |> Seq.skip 1
       |> List.ofSeq

open System.Threading.Tasks
open Nostra.Core.Client.Response

let buildWebSocket (ws: ClientWebSocket) : WebSocket.WebSocket = {
    write =
        fun arr ->
            ws.SendAsync( ArraySegment(arr), WebSocketMessageType.Text, true, CancellationToken.None ) |> Async.AwaitTask
    read =
        fun buffer -> async {
            let! result = ws.ReceiveAsync(ArraySegment(buffer), CancellationToken.None) |> Async.AwaitTask
            return { Count = result.Count; EndOfMessage = result.EndOfMessage }
        }
     }

let args = fsi.CommandLineArgs
let switchs = CommandLine.parseArgs args

match switchs with
| ("genkey", _)::rest ->
    let secret = Key.createNewRandom ()
    let secretBytes = Array.zeroCreate 32
    secret.WriteToSpan (Span secretBytes)
    Console.WriteLine ($"secret: {Utils.toHex secretBytes}")
    Console.WriteLine ($"pubkey: {secret |> Key.getPubKey |> (fun x -> x.ToBytes()) |> Utils.toHex}")
    0
| ("listen", _)::rest ->
    let args = Map.ofList rest
    
    let displayResponse = function
        | Ok (RMEvent (subscriptionId, event)) ->
            let (XOnlyPubKey pubKey) = event.PubKey
            Console.ForegroundColor <- ConsoleColor.Cyan
            Console.WriteLine $"Kind: {event.Kind}  - Author: {pubKey.ToBytes() |> Utils.toHex}"
            Console.ForegroundColor <- enum<ConsoleColor> (-1)
            Console.WriteLine (event.Content.Trim())
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.WriteLine (event.Tags |> List.map (fun (t, vs) -> $"{t}:{vs}"))
            Console.WriteLine ()

        | Ok (RMACK(eventId, success, message)) ->
            Console.ForegroundColor <- ConsoleColor.Green
            let (EventId eid) = eventId 
            Console.WriteLine $"Event: {eid |> Utils.toHex} Success: {success} = {message}"
        | Ok (RMNotice message) ->
            Console.ForegroundColor <- ConsoleColor.Yellow
            Console.WriteLine message
        | Ok (RMEOSE subscriptionId) ->
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.WriteLine $">>> {subscriptionId} Done"
        | Error e ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine (e.ToString())

    async {
        let ws = new ClientWebSocket()
        let io = buildWebSocket ws

        // "wss://nostr-pub.wellorder.net"
        do! ws.ConnectAsync (Uri (args["relay"]), CancellationToken.None) |> Async.AwaitTask
        let pushToRelay = Reader.run io (Client.Communication.sender ())
        let filter = Client.Request.Filter.FilterUtils.toFilter (Client.Request.Filter.FilterUtils.ClientFilter.AllNotes (DateTime.UtcNow.AddDays(-1)))      
        Client.Request.CMSubscribe ("all", [filter])
        |> pushToRelay

        let receiveLoop = Reader.run io (Client.Communication.startReceiving displayResponse)
        do! receiveLoop 
        
    } |> Async.RunSynchronously
    0
| ("sendmsg", _)::rest ->
    let args = Map.ofList rest
    let secret = args["secret"] |> Utils.fromHex |> ECPrivKey.Create 
    let recipient = args["to"] |> Utils.fromHex |> ECXOnlyPubKey.Create |> XOnlyPubKey 
    let msg = args["msg"]
    let dm = Event.createEncryptedDirectMessage recipient secret msg
    let signedDm = Event.sign secret dm 

    let ws = new ClientWebSocket()
    let io = buildWebSocket ws
    
    ws.ConnectAsync (Uri (args["relay"]), CancellationToken.None)
    |> Async.AwaitTask
    |> Async.RunSynchronously
   
    let (EventId id) = signedDm.Id
    let pushToRelay = Reader.run io (Client.Communication.sender ()) 
    pushToRelay (Client.Request.CMEvent signedDm)
    
    Console.WriteLine (id |> Utils.toHex)
    Task.Delay(1000) 
    |> Async.AwaitTask
    |> Async.RunSynchronously
    0
| _ ->
    """
    Usage: dotnet fsi nostrcli.fsx -- command
    
    Commands:
        genkey                      generates a new private/public key pair.
        listen                      listens for tex notes
            --relay                     from the relay (eg: wss://nostr-pub.wellorder.net)
        sendmsg                     sends encrypted message
            --to                        to the specified public key
            --secret                    using the specified secret key to encrypt
            --msg                       the message
            --relay                     relay to be used
    """
    |> Console.WriteLine
    0

//Library.fsx genkey
//secret: 65efca3c243e4132afbfc7e30fbc41d8d3698d26d11d816bc24a7787aa57f0dc
//pubkey: dc04a357c5ef17dd9ca245b7fa24842fc227a5b86a57f5d6a36a8d4443c21014
