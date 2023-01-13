#r "nuget:NBitcoin.Secp256k1"

open System
open System.Security.Cryptography
open System.Text
open System.Text.Json
open NBitcoin.Secp256k1

module Epoch =
    let fromDateTime (dt: DateTime) =
        dt - DateTime(1970, 1, 1) |> fun t -> t.TotalSeconds |> int64

    let now () =
        fromDateTime DateTime.UtcNow

module Key =
    let createNewRandom () =
        ECPrivKey.TryCreate( ReadOnlySpan(RandomNumberGenerator.GetBytes(32))) |> snd

    let getPubKey (secret: ECPrivKey) =
        secret.CreateXOnlyPubKey()

[<AutoOpen>]
module Event =

    type TSignedEvent = {
        id: string 
        pubkey: string
        created_at: int64
        kind: int
        tags: string list
        content: string
        ``sig``: string
    }

    type TUnsignedEvent = {
        pubkey: string
        created_at: int64
        kind: int
        tags: string list
        content: string
    }

    let createEvent (pubkey: ECXOnlyPubKey) kind tags content =
        {
            pubkey = pubkey.ToBytes() |> toHex;
            created_at = Epoch.now();
            kind = kind;
            tags = tags;
            content = content
        }

    let serialize obj =
        JsonSerializer.Serialize(obj, JsonSerializerOptions(WriteIndented = false ))

    let toJSON (event: TSignedEvent) =
        event |> serialize

    let toJsonArray (event: TUnsignedEvent) =
        [ 0; event.pubkey; event.created_at; event.kind; event.tags; event.content ] : obj list

    let serializeForEventId (event: TUnsignedEvent) =
        event
        |> toJsonArray
        |> serialize

    let toHex (bytes:byte[]) =
        bytes
        |> Array.map (fun x -> sprintf "%02x" x)
        |> String.concat ""

    let getEventId (event: TUnsignedEvent) =
        event
        |> serializeForEventId
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData

    let sign (secret: ECPrivKey) (event: TUnsignedEvent) : TSignedEvent =
        let eventId = event |> getEventId
        let pubkey =
            secret
            |> Key.getPubKey
            |> fun pubkey -> pubkey.ToBytes() 
        let signature =
            eventId
            |> secret.SignBIP340
            |> fun signature -> signature.ToBytes()
        { 
            id = eventId |> toHex;
            pubkey = pubkey |> toHex;
            created_at = Epoch.now();
            kind = event.kind;
            tags = event.tags; 
            content = event.content; 
            ``sig`` = signature |> toHex 
        }

module Main =  
    let secret = Key.createNewRandom ()
    let pubkey = Key.getPubKey secret
    let unsignedEvent = createEvent pubkey 2 [] "Hello world!"
    let signedEvent = sign secret unsignedEvent 
    signedEvent |> toJSON |> Console.WriteLine 

