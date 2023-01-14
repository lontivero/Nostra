﻿// #r "nuget:NBitcoin.Secp256k1"
#nowarn "3391"

namespace Nostra.Core

open System
open System.Buffers
open System.IO
open System.Net.WebSockets
open System.Security.Cryptography
open System.Text
open System.Threading
open Microsoft.FSharp.Control
open NBitcoin.Secp256k1
open Chiron

module Utils =
    let toHex (bytes:byte[]) =
       bytes |> Convert.ToHexString |> (fun s -> s.ToLower()) 

    let fromHex (str: string) =
        Convert.FromHexString str

module Key =
    let createNewRandom () =
        ECPrivKey.TryCreate( ReadOnlySpan(RandomNumberGenerator.GetBytes(32))) |> snd

    let getPubKey (secret: ECPrivKey) =
        secret.CreateXOnlyPubKey()

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
    open Chiron

    let (|String64|_|) = function
        | String str ->
            let bytes = Utils.fromHex str
            if bytes.Length = 32 then
                Some (bytes)
            else
                None
        | _ -> None

    let bytesToJson (bytes: byte[]) = 
        ToJsonDefaults.ToJson (bytes |> Utils.toHex )

    type EventId = | EventId of byte[] with
        static member ToJson (EventId eventId) =
            bytesToJson eventId
        static member FromJson (_ : EventId) = fun json ->
            match json with 
            | String64 hex -> Value (EventId hex), json
            | _ -> Error "Incorrect EventId format", json

    type XOnlyPubKey = | XOnlyPubKey of ECXOnlyPubKey with
        static member ToJson (XOnlyPubKey pubKey) =
            pubKey.ToBytes() |> bytesToJson 
        static member FromJson (_ : XOnlyPubKey) = fun json ->
            match json with 
            | String64 hex -> Value (XOnlyPubKey (ECXOnlyPubKey.Create hex)), json
            | _ -> Error "Incorrect pubkey format", json

    type ProfileName = string
    type Uri_ = string
    
    type SchnorrSignature = | SchnorrSignature of SecpSchnorrSignature with 
        static member ToJson (SchnorrSignature signature) =
            signature.ToBytes() |> bytesToJson 
        static member FromJson (_ : SchnorrSignature) = fun json ->
            match json with 
            | String hex -> Value (SchnorrSignature (snd <| SecpSchnorrSignature.TryCreate (Utils.fromHex hex))), json
            | _ -> Error "Incorrect EventId format", json
    
    let dateTimeToJson (dt : DateTime) =
        (dt - DateTime(1970, 1, 1))
        |> fun t -> t.TotalSeconds
        |> decimal
        |> Number
    
    let jsonToDateTime (json : Json)  = 
        match json with
        | Number seconds -> Value (TimeSpan.TicksPerSecond * (int64 seconds) + DateTime.UnixEpoch.Ticks |> DateTime)
        | _ -> Error "Incorrect date/time"
    

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
        with
        static member ToJson (tag: Tag) = 
            match tag with
            | PubKeyFull (pubkey, uri, petname) -> ToJsonDefaults.ToJson (("p", pubkey, uri, petname))
            | PubKeyWithUri (pubkey, uri) -> ToJsonDefaults.ToJson (("p", pubkey, uri))
            | PubKeyOnly (pubkey) -> ToJsonDefaults.ToJson (("p", pubkey))
            | EventPositional (eventId) -> ToJsonDefaults.ToJson (("e", eventId))
            | EventPositionalWithUri (eventId, uri) -> ToJsonDefaults.ToJson (("e", eventId, uri))
            | EventRoot (eventId, uri) -> ToJsonDefaults.ToJson (("e", eventId, uri, "root"))
            | EventReply (eventId, uri) -> ToJsonDefaults.ToJson (("e", eventId, uri, "reply"))
            | MentionTag (eventId, uri) -> ToJsonDefaults.ToJson (("e", eventId, uri, "mention"))
            | HashTag (hashtag) -> ToJsonDefaults.ToJson (("t", hashtag))
            | GeoTag (geotag) -> ToJsonDefaults.ToJson (("g", geotag))
            | ReferenceTag (reference) -> ToJsonDefaults.ToJson (("r", reference))
            | NonceTag (nonce, target) -> ToJsonDefaults.ToJson (("nonce", nonce, target))
            | _ -> ToJsonDefaults.ToJson (0)
            
        static member FromJson(_:Tag) = fun json ->
            let arr =
                match json with
                | Array [String "p"; String64 pubkey] -> PubKeyOnly (XOnlyPubKey (ECXOnlyPubKey.Create pubkey))
                | Array [String "p"; String64 pubkey; String uri] -> PubKeyWithUri (XOnlyPubKey(ECXOnlyPubKey.Create pubkey), uri)
                | Array [String "p"; String64 pubkey; String uri; String profileName] -> PubKeyFull (XOnlyPubKey(ECXOnlyPubKey.Create pubkey), uri, profileName)
                | Array [String "e"; String64 eventId] -> EventPositional(EventId eventId)
                | Array [String "e"; String64 eventId; String uri] -> EventPositionalWithUri(EventId eventId, uri)
                | Array [String "e"; String64 eventId; String uri; String "root"] -> EventRoot(EventId eventId, uri)
                | Array [String "e"; String64 eventId; String uri; String "reply"] -> EventReply(EventId eventId, uri)
                | Array [String "e"; String64 eventId; String uri; String "mention"] -> MentionTag(EventId eventId, uri)
                | Array ((String "e")::(String64 eventId)::rest) -> EventReply(EventId eventId, "") 
                | Array [String "t"; String hashtag] -> HashTag(hashtag) 
                | Array [String "g"; String geotag] -> GeoTag(geotag) 
                | Array [String "r"; String referenceUri] -> ReferenceTag(referenceUri) 
                | Array [String "nonce"; Number nonce; Number target] -> NonceTag(int64 nonce, int target) 
                | _ -> UnknownTag
            Value arr, json

    type Kind =
        | Metadata = 0
        | Text = 1
        | Recommend = 2
        | Contacts = 3
        | Encrypted = 4
        | Deleted = 5
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
        } with
        static member ToJson (event: Event) = json {
            do! Json.write "id" event.Id
            do! Json.write "pubkey" event.PubKey
            do! Json.writeWith dateTimeToJson "created_at" event.CreatedAt
            do! Json.write "kind" (int event.Kind)
            do! Json.write "tags" event.Tags
            do! Json.write "content" event.Content
            do! Json.write "sig" event.Signature
        }
        
        static member FromJson(_: Event) = json { 
            let! id = Json.read "id"
            let! pubkey = Json.read "pubkey"
            let! createdAt = Json.readWith jsonToDateTime "created_at"
            let! kind = Json.read "kind"
            let! tags = Json.read "tags"
            let! content = Json.read "content"
            let! signature = Json.read "sig"
            return {
                Id = id 
                PubKey = pubkey
                CreatedAt = createdAt
                Kind = enum<Kind>(kind)
                Tags = tags
                Content = content
                Signature = signature
            }
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

    let sharedKey (XOnlyPubKey he) (mySecret: ECPrivKey) =
        let ecPubKey = ReadOnlySpan (Array.insertAt 0 2uy (he.ToBytes()))
        let hisPubKey = ECPubKey.Create ecPubKey
        let sharedPubKey = hisPubKey.GetSharedPubkey(mySecret).ToBytes()
        sharedPubKey[1..]
    
    let createEncryptedDirectMessage (recipient: XOnlyPubKey) (secret: ECPrivKey) content =
        let sharedPubKey = sharedKey recipient secret
        let iv, encryptedContent = Encryption.encrypt (sharedPubKey) content 
        createEvent Kind.Encrypted [PubKeyOnly(recipient)] $"{Convert.ToBase64String(encryptedContent)}?iv={Convert.ToBase64String(iv)}"
    
    let decryptDirectMessage (secret: ECPrivKey) (event: Event) =
        let message = event.Content
        let parts =
            message.Split ("?iv=")
            |> Array.map Convert.FromBase64String
        let sharedPubKey = sharedKey event.PubKey secret
        Encryption.decrypt sharedPubKey parts[1] parts[0]

    let toUnsignedEvent (event: Event) = {
        CreatedAt = event.CreatedAt
        Kind = event.Kind
        Tags = event.Tags
        Content = event.Content
    }

    let serializeForEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) = 
        [
          Number 0
          pubkey |> Json.serialize
          dateTimeToJson event.CreatedAt
          Number (int event.Kind)
          Array <| (event.Tags |> List.map Json.serialize)
          String (event.Content)
        ]

    let getEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        event
        |> serializeForEventId pubkey
        |> Json.serialize
        |> Json.format
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData

    let sign (secret: ECPrivKey) (event: UnsignedEvent) : Event =
        let pubkey = secret |> Key.getPubKey |> XOnlyPubKey 
        let eventId = event |> getEventId pubkey
        { 
            Id = EventId eventId;
            PubKey = pubkey;
            CreatedAt = DateTime.UtcNow;
            Kind = event.Kind;
            Tags = event.Tags; 
            Content = event.Content; 
            Signature = secret.SignBIP340 eventId |> SchnorrSignature
        }

    let verify (event : Event) =
        let (EventId id, XOnlyPubKey pubkey, SchnorrSignature signature) = event.Id, event.PubKey, event.Signature
        let computedId = event |> toUnsignedEvent |> getEventId event.PubKey
        computedId = id && pubkey.SigVerifyBIP340(signature, id)

module Query =
    let kindsToJson (kinds : Kind list) =
        Array <| (kinds |> List.map (decimal >> Number))
        
    type Filter =
         | MetadataFilter of XOnlyPubKey list * DateTime
         | ContactsFilter of XOnlyPubKey list * DateTime
         | TextNoteFilter of XOnlyPubKey list * DateTime
         | LinkedEvents of EventId list * DateTime
         | AllNotes of DateTime
         | AllMetadata of DateTime
         | DirectMessageFilter of XOnlyPubKey
         with
         static member ToJson (filter: Filter) = json {
            match filter with
            | MetadataFilter (pubKeys, dateTime) ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Metadata ]
                do! Json.write "authors" pubKeys
                do! Json.write "limit" 1
                do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
            | ContactsFilter (pubKeys, dateTime) ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Contacts ]
                do! Json.write "authors" pubKeys
                do! Json.write "limit" 500
                do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
            | DirectMessageFilter (pubKey) ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Encrypted ]
                do! Json.write "authors" pubKey
                do! Json.write "limit" 1
                do! Json.writeWith dateTimeToJson "since" (DateTime.UtcNow.AddSeconds -180.0)
            | TextNoteFilter (pubKeys, dateTime) ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Text; Kind.Deleted ]
                do! Json.write "authors" pubKeys
                do! Json.write "limit" 500
                do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
            | LinkedEvents (eventIds, dateTime) ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Text ]
                do! Json.write "limit" 500
                do! Json.write "#e" (Array <| (List.map (Json.serialize) eventIds))
                do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
            | AllNotes dateTime ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Text ]
                do! Json.write "limit" 500
                do! Json.writeWith dateTimeToJson "since" (dateTime.AddSeconds -180.0)
            | AllMetadata dateTime ->
                do! Json.writeWith kindsToJson "kinds" [ Kind.Metadata ]
                do! Json.write "limit" 500
                do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
        }

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
            | CMEvent event -> [ String "EVENT"; event |> Json.serialize ]
            | CMSubscribe (subscriptionId, filters) -> [ String "REQ"; String subscriptionId ] @ [for filter in filters do Json.serialize filter]
            | CMUnsubscribe subscriptionId -> [ String "CLOSE"; subscriptionId |> Json.serialize ]
        json |> Json.serialize |> Json.format 


    let deserialize (payload: string) =
        let json = Json.parse payload
        match json with
        | Array [String "EVENT"; String subscriptionId; event] -> Ok <| RMEvent (subscriptionId, event |> Json.deserialize)
        | Array [String "NOTICE"; String message] -> Ok <| RMNotice (message)
        | Array [String "OK";  String64 eventId; Bool success; String message] -> Ok <| RMACK (EventId eventId, success, message)
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
        
        
    let startReceiving (ws:WebSocket) (fn: Event * bool -> unit) = async {
        let rec loop () = async {
            let! payload = (readWebSocketMessage ws)
            let relayMsg = deserialize (Encoding.UTF8.GetString payload)
            match relayMsg with
            | Ok (RMEvent (subscriptionId, event)) ->
                fn (event, (verify event))
                //Console.WriteLine (Encoding.UTF8.GetString payload)
            | Ok (RMNotice notice) -> 1 |> ignore
            | Ok (RMACK (eid, s, ack)) -> 1 |> ignore
            | Ok (RMEOSE s) -> 1 |> ignore
            | Result.Error error -> 1 |> ignore
            return! loop ()
        }
        do! loop ()
    }
