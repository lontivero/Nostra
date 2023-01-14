#r "nuget:NBitcoin.Secp256k1"
#r "nuget:Chiron"

#nowarn "3391"
#nowarn "0020"

open System
open System.Buffers
open System.IO
open System.Net.WebSockets
open System.Security.Cryptography
open System.Text
open System.Threading
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open NBitcoin.Secp256k1
open Chiron

module Utils =
    let toHex (bytes:byte[]) =
       bytes
       |> Array.map (fun x -> sprintf "%02x" x)
       |> String.concat ""
    
    let fromHex (str: string) =
        Convert.FromHexString str

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None
        
module Epoch =
    let fromDateTime (dt: DateTime) =
        dt - DateTime(1970, 1, 1) |> fun t -> t.TotalSeconds |> int64

module Key =
    let createNewRandom () =
        ECPrivKey.TryCreate( ReadOnlySpan(RandomNumberGenerator.GetBytes(32))) |> snd

    let getPubKey (secret: ECPrivKey) =
        secret.CreateXOnlyPubKey()

    let sharedKey (he: ECXOnlyPubKey) (mySecret: ECPrivKey) =
        let ecPubKey = ReadOnlySpan (Array.insertAt 0 2uy (he.ToBytes()))
        let hisPubKey = ECPubKey.Create ecPubKey
        let sharedPubKey = hisPubKey.GetSharedPubkey(mySecret).ToBytes()
        sharedPubKey[1..]

module Encryption =
    let encrypt (encryptionKey: byte[]) (plainText: string) =
        let iv = RandomNumberGenerator.GetBytes(16)
        let aes = Aes.Create(Key = encryptionKey, IV = iv)
        let plainTextBytes = ReadOnlySpan (Encoding.UTF8.GetBytes(plainText))
        let cipherTextBytes = aes.EncryptCbc (plainTextBytes, iv)
        iv, cipherTextBytes
        
    let decrypt (decryptionKey: byte[]) (iv: byte[]) (cipherTextBytes: byte[]) =
        let aes = Aes.Create(Key = decryptionKey, IV = iv)
        aes.DecryptCbc (cipherTextBytes, iv)
        |> Encoding.UTF8.GetString


[<AutoOpen>]
module Event =

    type EventId = byte[]
    type XOnlyPubKey = ECXOnlyPubKey
    type ProfileName = string
    type Uri_ = string
    type SchnorrSignature = SecpSchnorrSignature

    type Tag =
        | PubKeyFull of XOnlyPubKey * Uri_ * ProfileName
        | PubKeyWithUri of XOnlyPubKey * Uri_
        | PubKeyOnly of XOnlyPubKey
        | EventPositional of EventId
        | EventPositionalWithUri of EventId * Uri_
        | EventRoot of EventId * Uri_
        | EventReply of EventId * Uri_
        | MentionTag of EventId * Uri_
        | HashTag of string
        | GeoTag of string
        | ReferenceTag of Uri_
        | NonceTag of int64 * int
        | UnknownTag

    type Kind =
        | Metadata = 0
        | Text = 1
        | Recommend = 2
        | Contacts = 3
        | Encrypted = 4
        | Event = 5
        | Repost = 6
        | Reaction = 7

    type Event = {
        Id: EventId
        PubKey: XOnlyPubKey
        CreatedAt: DateTime
        Kind: Kind
        Tags: Tag list
        Content: string
        Signature: SchnorrSignature
    }

    type UnsignedEvent = {
        CreatedAt: DateTime
        Kind: Kind
        Tags: Tag list
        Content: string
    }

    let createEvent kind tags content = {
        CreatedAt = DateTime.UtcNow;
        Kind = kind;
        Tags = tags;
        Content = content
    }

    let createNoteEvent (pubkey: XOnlyPubKey) content =
        createEvent Kind.Text [] content

    let createReplyEvent (replyTo: EventId) (pubkey: XOnlyPubKey) content =
        createEvent Kind.Text [EventReply(replyTo, Uri_(""))] content

    let createEncryptedDirectMessage (recipient: XOnlyPubKey) (secret: ECPrivKey) content =
        let sharedPubKey = Key.sharedKey recipient secret
        let iv, encryptedContent = Encryption.encrypt (sharedPubKey) content 
        createEvent Kind.Encrypted [PubKeyOnly(recipient)] $"{Convert.ToBase64String(encryptedContent)}?iv={Convert.ToBase64String(iv)}"
    
    let decryptDirectMessage (secret: ECPrivKey) (event: Event) =
        let message = event.Content
        let parts =
            message.Split ("?iv=")
            |> Array.map Convert.FromBase64String
        let sharedPubKey = Key.sharedKey event.PubKey secret
        Encryption.decrypt sharedPubKey parts[1] parts[0]

    let toUnsignedEvent (event: Event) = {
        CreatedAt = event.CreatedAt
        Kind = event.Kind
        Tags = event.Tags
        Content = event.Content
    }

    let bytesToJson (bytes: byte[]) =
        bytes |> Utils.toHex |> String

    let jsonToBytes = function
        | String pubkey -> pubkey |> Utils.fromHex
        | _ -> failwith "Incorrect"
        
    let kindToJson (kind : Kind) =
        Number (int kind)
    
    let kindsToJson (kinds : Kind list) =
        Array <| (kinds |> List.map kindToJson)
        
    let pubKeyToJson (pubkey: XOnlyPubKey) =
        bytesToJson (pubkey.ToBytes())

    let schnorrSignatureToJson (signature: SchnorrSignature) =
        bytesToJson (signature.ToBytes())

    let eventIdToJson (eventId: EventId) =
        bytesToJson eventId
    
    let pubKeysToJson (pubkeys: XOnlyPubKey list) =
        Array <| (pubkeys |> List.map pubKeyToJson)
        
    let eventIdsToJson (eventIds: EventId list) =
        Array <| (eventIds |> List.map eventIdToJson)

    let jsonToEventId (json: Json) =
         jsonToBytes json 
        
    let dateTimeToJson (dt : DateTime) =
        Number <| (Epoch.fromDateTime dt)

    let jsonToDateTime = function
        | Number seconds -> TimeSpan.TicksPerSecond * (int64 seconds) + DateTime.UnixEpoch.Ticks |> DateTime
        | _ -> failwith "Incorrect date/time"
            
    let jsonToXOnlyPubKey (json: Json) =
        json |> jsonToBytes |> XOnlyPubKey.Create

    let jsonToSchnorrSignature (json: Json) =
        json |> jsonToBytes |> SchnorrSignature.TryCreate |> snd
    
    let tagToJson (tag : Tag) =
        match tag with
        | PubKeyFull (pubkey, uri, petname) -> Array [String "p"; pubKeyToJson pubkey; String (uri.ToString()); String petname]
        | PubKeyWithUri (pubkey, uri) -> Array [String "p"; pubKeyToJson pubkey; String (uri.ToString())]
        | PubKeyOnly (pubkey) -> Array [String "p"; pubKeyToJson pubkey]
        | EventPositional (eventId) -> Array [String "e"; eventIdToJson eventId]
        | EventPositionalWithUri (eventId, uri) -> Array [String "e"; eventIdToJson eventId; String (uri.ToString())]
        | EventRoot (eventId, uri) -> Array [String "e"; eventIdToJson eventId; String (uri.ToString()); String "root"]
        | EventReply (eventId, uri) -> Array [String "e"; eventIdToJson eventId; String (uri.ToString()); String "reply"]
        | MentionTag (eventId, uri) -> Array [String "e"; eventIdToJson eventId; String (uri.ToString()); String "mention"]
        | HashTag (hashtag) -> Array [String "t"; String hashtag]
        | GeoTag (geotag) -> Array [String "g"; String geotag]
        | ReferenceTag (reference) -> Array [String "r"; String reference]
        | NonceTag (nonce, target) -> Array [String "nonce"; Number nonce; Number target]
        | _ -> Array []

    let (|String64|_|) = function
        | String str ->
            let bytes = Utils.fromHex str
            if bytes.Length = 32 then
                Some (bytes)
            else
                None
        | _ -> None
            
    let jsonToTag (json: Json) =
        match json with
         | Array [String "p"; String64 pubkey] -> PubKeyOnly (XOnlyPubKey.Create pubkey)
         | Array [String "p"; String64 pubkey; String uri] -> PubKeyWithUri (XOnlyPubKey.Create pubkey, uri)
         | Array [String "p"; String64 pubkey; String uri; String profileName] -> PubKeyFull (XOnlyPubKey.Create pubkey, uri, profileName)
         | Array [String "e"; String64 eventId] -> EventPositional(eventId)
         | Array [String "e"; String64 eventId; String uri] -> EventPositionalWithUri(eventId, uri)
         | Array [String "e"; String64 eventId; String uri; String "root"] -> EventRoot(eventId, uri)
         | Array [String "e"; String64 eventId; String uri; String "reply"] -> EventReply(eventId, uri)
         | Array [String "e"; String64 eventId; String uri; String "mention"] -> MentionTag(eventId, uri)
         | Array ((String "e")::(String64 eventId)::rest) -> EventReply(eventId, "") 
         | Array [String "t"; String hashtag] -> HashTag(hashtag) 
         | Array [String "g"; String geotag] -> GeoTag(geotag) 
         | Array [String "r"; String referenceUri] -> ReferenceTag(referenceUri) 
         | Array [String "nonce"; Number nonce; Number target] -> NonceTag(int64 nonce, int target) 
         | _ -> UnknownTag

    let tagsToJson (tags: Tag list) =
        Array <| (tags |> List.map tagToJson)
    
    let toJsonArray (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        Array <| [
            Number 0
            pubKeyToJson pubkey
            dateTimeToJson event.CreatedAt
            kindToJson event.Kind
            tagsToJson event.Tags
            String event.Content
        ]

    let serializeEvent (event: Event) = json {
        do! Json.writeWith eventIdToJson  "id"     event.Id
        do! Json.writeWith pubKeyToJson   "pubkey" event.PubKey
        do! Json.writeWith dateTimeToJson "created_at" event.CreatedAt
        do! Json.writeWith kindToJson     "kind"   event.Kind
        do! Json.writeWith tagsToJson     "tags"   event.Tags
        do! Json.write "content" event.Content
        do! Json.writeWith schnorrSignatureToJson "sig"  event.Signature
    }
        
    let eventToJson (event: Event) =
        let xx = Json.writeWith eventIdToJson "id" event.Id
        Object <| Map.ofList [
            "id", eventIdToJson event.Id
            "pubkey", pubKeyToJson event.PubKey
            "created_at", dateTimeToJson event.CreatedAt
            "kind", kindToJson event.Kind
            "tags", tagsToJson event.Tags
            "content", String event.Content
            "sig", schnorrSignatureToJson event.Signature
        ]

    let jsonToTags = function
        | Array arr -> arr |> List.map jsonToTag
        | _ -> failwith "Incorrect tags"
        
    let jsonToContent = function
        | String content -> content
        | _ -> failwith "Incorrect content"
        
    let jsonToEvent = function
        | Object map -> {
              Id =  jsonToEventId (map |> Map.find "id") 
              PubKey = jsonToXOnlyPubKey (map |> Map.find "pubkey")
              CreatedAt = jsonToDateTime (map |> Map.find "created_at")
              Kind = (map |> Map.find "kind") |> function | Number n -> enum<Kind>(int n) | _ -> Kind.Repost 
              Tags = jsonToTags (map |> Map.find "tags") 
              Content = jsonToContent (map |> Map.find "content") 
              Signature = jsonToSchnorrSignature (map |> Map.find "sig")
            }
        | _ -> failwith "Incorrect event"
        
    let serializeForEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        event
        |> toJsonArray pubkey
        |> Json.format

    let getEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        event
        |> serializeForEventId pubkey
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData

    let sign (secret: ECPrivKey) (event: UnsignedEvent) : Event =
        let pubkey = secret |> Key.getPubKey 
        let eventId = event |> getEventId pubkey
        { 
            Id = eventId;
            PubKey = pubkey;
            CreatedAt = DateTime.UtcNow;
            Kind = event.Kind;
            Tags = event.Tags; 
            Content = event.Content; 
            Signature = secret.SignBIP340 eventId
        }

    let verify (event : Event) =
        let pubkey = event.PubKey
        let computedId = event |> toUnsignedEvent |> getEventId pubkey
        computedId = event.Id && event.PubKey.SigVerifyBIP340(event.Signature, event.Id)

module Query =
    type Filter =
         | MetadataFilter of XOnlyPubKey list * DateTime
         | ContactsFilter of XOnlyPubKey list * DateTime
         | TextNoteFilter of XOnlyPubKey list * DateTime
         | LinkedEvents of EventId list * DateTime
         | AllNotes of DateTime
         | AllMetadata of DateTime
         | DirectMessageFilter of XOnlyPubKey

    let filterToJson = function
        | MetadataFilter (pubKeys, dateTime) ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Metadata ]
                "authors", pubKeysToJson pubKeys
                "limit", Number 1
                "until", dateTimeToJson (dateTime.AddSeconds 60.0)
            ]
        | ContactsFilter (pubKeys, dateTime) ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Contacts ]
                "authors", pubKeysToJson pubKeys
                "limit", Number 500
                "until", dateTimeToJson (dateTime.AddSeconds 60.0)
            ]
        | DirectMessageFilter (pubKey) ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Encrypted ]
                "authors", pubKeysToJson [pubKey]
                "limit", Number 1
                "since", dateTimeToJson (DateTime.UtcNow.AddSeconds (-180.0))
            ]
        | TextNoteFilter (pubKeys, dateTime) ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Text; Kind.Event  ]
                "authors", pubKeysToJson pubKeys
                "limit", Number 100
                "until", dateTimeToJson (dateTime.AddSeconds 60.0)
            ]
        | LinkedEvents (eventIds, dateTime) ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Text ]
                "limit", Number 100
                "#e", eventIdsToJson eventIds
                "until", dateTimeToJson (dateTime.AddSeconds 60.0)
            ]
        | AllNotes dateTime ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Text ]
                "limit", Number 100
                "since", dateTimeToJson (dateTime.AddSeconds 60.0)
            ]
        | AllMetadata dateTime ->
            Object <| Map.ofList [
                "kinds", kindsToJson [ Kind.Metadata ]
                "limit", Number 100
                "until", dateTimeToJson (dateTime.AddSeconds 60.0)
            ]

    let filtersToJson (filters: Filter list) =
        Array <| (filters |> List.map filterToJson)
        
module Client =
    type SubscriptionId = string

    type ClientMessage =
        | CMEvent of Event
        | CMSubscribe of SubscriptionId * Query.Filter list
        | CMUnsubscribe of SubscriptionId

    type RelayMessage =
        | RMEvent of SubscriptionId * Event.Event
        | RMNotice of string
        | RMACK of EventId * bool * string
        | RMEOSE of string
    
    let serialize (msg: ClientMessage) =
        let json =
            match msg with
            | CMEvent event -> [ String "EVENT"; eventToJson event ]
            | CMSubscribe (subscriptionId, filters) ->[String "REQ"; String subscriptionId; Query.filterToJson (List.head filters)]
            | CMUnsubscribe subscriptionId -> [String "CLOSE"; String subscriptionId]
        Json.format (Array json)


    let deserialize (payload: string) =
        let json = Json.parse payload
        match json with
        | Array [String "EVENT"; String subscriptionId; event] -> Ok <| RMEvent (subscriptionId, jsonToEvent event)
        | Array [String "NOTICE"; String message] -> Ok <| RMNotice (message)
        | Array [String "OK";  String64 eventId; Bool success; String message] -> Ok <| RMACK (eventId, success, message)
        | Array [String "EOSE"; String subscriptionId] -> Ok <| RMEOSE (subscriptionId)
        | _ -> Result.Error "Unexpected message format from the relay"
        
    let sender (ws:WebSocket) = MailboxProcessor<ClientMessage>.Start (fun inbox ->
        let rec loop () = async { 
            let! msg = inbox.Receive()
            let payload = msg |> serialize |> Encoding.UTF8.GetBytes
            do! ws.SendAsync( ArraySegment(payload), WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
            return! loop() }
        loop () )
    
    let readWebSocketMessage (ws:WebSocket) =
        let rec readMessage (mem:MemoryStream) = async {
            let buffer = ArraySegment (ArrayPool.Shared.Rent(1024))
            let! result = ws.ReceiveAsync(buffer, CancellationToken.None) |> Async.AwaitTask
            mem.Write (buffer.Array, buffer.Offset, result.Count)
            ArrayPool.Shared.Return buffer.Array
            if result.EndOfMessage then
                return mem.ToArray()
            else
                return! readMessage mem
        }
        readMessage (new MemoryStream (4 * 1024))
        
        
    let startReceiving (ws:WebSocket) (fn: Event -> unit) = async {
        let rec loop () = async {
            let! payload = (readWebSocketMessage ws)
            let relayMsg = deserialize (Encoding.UTF8.GetString payload)
            match relayMsg with
            | Ok (RMEvent (subscriptionId, event)) ->
                fn event
                //Console.WriteLine (Encoding.UTF8.GetString payload)
                // (verify event) then
            | Ok (RMNotice notice) -> 1
            | Ok (RMACK (eid, s, ack)) -> 3
            | Ok (RMEOSE s) -> 4
            | Result.Error error -> 2
            return! loop ()
        }
        do! loop ()
    }

// --------------------------

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
    async {
        let ws = new ClientWebSocket()

        // "wss://nostr-pub.wellorder.net"
        do! ws.ConnectAsync (Uri (args["relay"]), CancellationToken.None) |> Async.AwaitTask
        let send = Client.sender(ws)
         
        Client.CMSubscribe ("all", [Query.AllNotes (DateTime.UtcNow.AddDays(-1))])
        |> send.Post
        
        do! Client.startReceiving ws (
            fun event ->
                Console.WriteLine "---------------------------------------------------------------"
                Console.WriteLine $"Kind: {event.Kind}  - Author: {event.PubKey.ToBytes() |> Utils.toHex}"
                Console.WriteLine (event.Content)
            )
    } |> Async.RunSynchronously
    0
| ("sendmsg", _)::rest ->
    let args = Map.ofList rest
    let secret = args["secret"] |> Utils.fromHex |> ECPrivKey.Create 
    let recipient = args["to"] |> Utils.fromHex |> XOnlyPubKey.Create
    let msg = args["msg"]
    let dm = createEncryptedDirectMessage recipient secret msg
    let signedDm = sign secret dm 

    let ws = new ClientWebSocket()
    ws.ConnectAsync (Uri (args["relay"]), CancellationToken.None)
    |> Async.AwaitTask
    |> Async.RunSynchronously
   
    let send = Client.sender(ws)
    send.Post (Client.CMEvent signedDm)
    Console.WriteLine (signedDm.Id |> Utils.toHex)
    Task.Delay(1000) 
    |> Async.AwaitTask
    |> Async.RunSynchronously
    0
| _ ->
    """
    Usage: dotnet fsi Library.fsx -- command
    
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
