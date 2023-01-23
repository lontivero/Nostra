#if INTERACTIVE
#r "nuget:NBitcoin.Secp256k1"
#r "nuget:Thoth.json.Net"
#endif

#nowarn "3391"

namespace Nostra.Core

open System
open System.Security.Cryptography
open System.Text
open NBitcoin.Secp256k1
open Thoth.Json.Net

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
    open Utils
    let replyTag (EventId replyTo) uri =
         Tag("p", [toHex replyTo; uri])
             
    let encryptedTo (XOnlyPubKey pubkey) =
        Tag("p", [toHex (pubkey.ToBytes())])

    let eventRefTag (EventId eventId) =
        Tag("e", [toHex eventId])
        
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
        }
    type UnsignedEvent = {
        CreatedAt: DateTime
        Kind: Kind
        Tags: Tag list
        Content: string
    }
    
    module Decode =
        let event : Decoder<Event> =
            Decode.object (fun get -> {
                Id = get.Required.Field "id" Decode.eventId
                PubKey = get.Required.Field "pubkey" Decode.xOnlyPubkey
                CreatedAt = get.Required.Field "created_at" Decode.unixDateTime
                Kind = get.Required.Field "kind" Decode.Enum.int
                Tags = get.Required.Field "tags" (Decode.list Decode.tag)
                Content = get.Required.Field "content" Decode.string
                Signature = get.Required.Field "sig" Decode.schnorrSignature
            })

    module Encode =
        let event (event : Event) =
            Encode.object [
                "id", Encode.eventId event.Id
                "pubkey", Encode.xOnlyPubkey event.PubKey
                "created_at", Encode.unixDateTime event.CreatedAt
                "kind", Encode.Enum.int event.Kind
                "tags", event.Tags 
                    |> List.map Encode.tag
                    |> Encode.list
                "content", Encode.string event.Content
                "sig", Encode.schnorrSignature event.Signature
            ]
            
    let serializeForEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) = 
        Encode.toCompactString (
            Encode.list [
              Encode.int 0
              Encode.xOnlyPubkey pubkey
              Encode.unixDateTime event.CreatedAt
              Encode.Enum.int event.Kind
              Encode.list (event.Tags |> List.map Encode.tag)
              Encode.string event.Content
            ])
        
    let createEvent kind tags content = {
        CreatedAt = DateTime.UtcNow;
        Kind = kind;
        Tags = tags;
        Content = content
    }

    let createNoteEvent content =
        createEvent Kind.Text [] content
        
    let createReplyEvent (replyTo: EventId) (pubkey: XOnlyPubKey) content =
        createEvent Kind.Text [replyTag replyTo ""] content

    let createDeleteEvent (ids: EventId list) content =
        createEvent Kind.Deleted (ids |> List.map eventRefTag) content
    
    let sharedKey (XOnlyPubKey he) (mySecret: ECPrivKey) =
        let ecPubKey = ReadOnlySpan (Array.insertAt 0 2uy (he.ToBytes()))
        let hisPubKey = ECPubKey.Create ecPubKey
        let sharedPubKey = hisPubKey.GetSharedPubkey(mySecret).ToBytes()
        sharedPubKey[1..]
    
    let createEncryptedDirectMessage (recipient: XOnlyPubKey) (secret: ECPrivKey) content =
        let sharedPubKey = sharedKey recipient secret
        let iv, encryptedContent = Encryption.encrypt sharedPubKey content 
        createEvent Kind.Encrypted [encryptedTo recipient] $"{Convert.ToBase64String(encryptedContent)}?iv={Convert.ToBase64String(iv)}"
    
    let decryptDirectMessage (secret: ECPrivKey) (event: Event) =
        let message = event.Content
        let parts =
            message.Split "?iv="
            |> Array.map Convert.FromBase64String
        let sharedPubKey = sharedKey event.PubKey secret
        Encryption.decrypt sharedPubKey parts[1] parts[0]

    let toUnsignedEvent (event: Event) = {
        CreatedAt = event.CreatedAt
        Kind = event.Kind
        Tags = event.Tags
        Content = event.Content
    }

    let getEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        event
        |> serializeForEventId pubkey
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData

    let sign (secret: ECPrivKey) (event: UnsignedEvent) : Event =
        let pubkey = secret |> Key.getPubKey |> XOnlyPubKey 
        let eventId = event |> getEventId pubkey
        { 
            Id = EventId eventId;
            PubKey = pubkey;
            CreatedAt = event.CreatedAt;
            Kind = event.Kind;
            Tags = event.Tags; 
            Content = event.Content; 
            Signature = secret.SignBIP340 eventId |> SchnorrSignature
        }

    let verify (event : Event) =
        let EventId id, XOnlyPubKey pubkey, SchnorrSignature signature = event.Id, event.PubKey, event.Signature
        let computedId = event |> toUnsignedEvent |> getEventId event.PubKey
        computedId = id && pubkey.SigVerifyBIP340(signature, id)
