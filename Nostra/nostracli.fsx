﻿#r "nuget:NBitcoin.Secp256k1"
#r "nuget:Thoth.Json.Net"

#load "Utils.fs"
#load "Bech32.fs"
#load "Types.fs"
#load "Event.fs"
#load "Client.fs"

open System
open System.Net.WebSockets
open System.Threading
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open NBitcoin.Secp256k1
open Nostra
open Nostra.Client
open Nostra.Client.Response

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
open Nostra.Monad
open Nostra.Event
open Nostra.Client.Request.Filter.FilterUtils

let args = fsi.CommandLineArgs
let switchs = CommandLine.parseArgs args

match switchs with
| ("genkey", _)::rest ->
    let secret = Key.createNewRandom ()
    let secretBytes = Array.zeroCreate 32
    let pubKeyBytes = secret |> Key.getPubKey |> (fun x -> x.ToBytes())
    secret.WriteToSpan (Span secretBytes)
    
    let nsec = "nsec"
    let npub = "npub"
    Console.WriteLine ($"secret: hex    {Utils.toHex secretBytes}")
    Console.WriteLine ($"        bech32 {Bech32.encode nsec (secretBytes |> Array.toList)}")
    Console.WriteLine ($"pubkey: hex    {Utils.toHex pubKeyBytes}")
    Console.WriteLine ($"        bech32 {Bech32.encode npub (pubKeyBytes |> Array.toList)}")
    0
| ("listen", _)::rest ->
    let args = Map.ofList rest
    
    let printEvent (eventResult) =
        match eventResult with
        | Ok relayMessage ->
            match relayMessage with
            | RMEvent (id, event) ->
                let (XOnlyPubKey pubKey) = event.PubKey
                Console.WriteLine "---------------------------------------------------------------"
                Console.WriteLine $"Kind: {event.Kind}  - Author: {pubKey.ToBytes() |> Utils.toHex}"
                Console.WriteLine (event.Content)
            | _ ->
                Console.WriteLine "- other -"
        | Error e -> Console.WriteLine (e.ToString())

    async {
        let ws = new ClientWebSocket()

        // "wss://nostr-pub.wellorder.net"
        do! ws.ConnectAsync (Uri (args["relay"]), CancellationToken.None) |> Async.AwaitTask
        let ctx = Client.Communication.buildContext ws Console.Out
        let pushToRelay = injectedWith ctx (Client.Communication.sender ())
        let filter = toFilter (AllNotes (DateTime.UtcNow.AddDays(-1)))
        
        Client.Request.CMSubscribe ("all", [filter])
        |> pushToRelay

        let receiveLoop = injectedWith ctx (Client.Communication.startReceiving printEvent)
        do! receiveLoop 
        
    } |> Async.RunSynchronously
    0
| ("sendmsg", _)::rest ->
    let args = Map.ofList rest
    let secret = args["secret"] |> Utils.fromHex |> ECPrivKey.Create 
    let recipient = args["to"] |> Utils.fromHex |> ECXOnlyPubKey.Create |> XOnlyPubKey 
    let msg = args["msg"]
    let dm = createEncryptedDirectMessage recipient secret msg
    let signedDm = sign secret dm 

    let ws = new ClientWebSocket()
    
    ws.ConnectAsync (Uri (args["relay"]), CancellationToken.None)
    |> Async.AwaitTask
    |> Async.RunSynchronously
   
    let (EventId id) = signedDm.Id
    let ctx = Client.Communication.buildContext ws Console.Out
    let pushToRelay = injectedWith ctx (Client.Communication.sender ()) 
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
