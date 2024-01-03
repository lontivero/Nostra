namespace Nostra

open System
open System.Security.Cryptography
open System.Text
open Microsoft.FSharp.Core
open NBitcoin.Secp256k1
open Thoth.Json.Net


type Kind =
    | Metadata = 0
    | Text = 1
    | Recommend = 2
    | Contacts = 3
    | Encrypted = 4
    | Delete = 5
    | Repost = 6
    | Reaction = 7
    | ReplaceableStart = 10_000
    | ReplaceableEnd = 20_000
    | EphemeralStart = 20_000
    | EphemeralEnd = 30_000
    | ParameterizableReplaceableStart = 30_000
    | ParameterizableReplaceableEnd = 40_000

type Event =
    { Id: EventId
      PubKey: XOnlyPubKey
      CreatedAt: DateTime
      Kind: Kind
      Tags: Tag list
      Content: string
      Signature: SchnorrSignature }

[<RequireQualifiedAccess>]
module Event =
    open Utils


    type UnsignedEvent =
        { CreatedAt: DateTime
          Kind: Kind
          Tags: Tag list
          Content: string }

    module Decode =

        let event: Decoder<Event> =
            Decode.object (fun get ->
                { Id = get.Required.Field "id" Decode.eventId
                  PubKey = get.Required.Field "pubkey" Decode.xOnlyPubkey
                  CreatedAt = get.Required.Field "created_at" Decode.unixDateTime
                  Kind = get.Required.Field "kind" Decode.Enum.int
                  Tags = get.Required.Field "tags" Tag.Decode.tagList
                  Content = get.Required.Field "content" Decode.string
                  Signature = get.Required.Field "sig" Decode.schnorrSignature })

    module Encode =
        let event (event: Event) =
            Encode.object
                [ "id", Encode.eventId event.Id
                  "pubkey", Encode.xOnlyPubkey event.PubKey
                  "created_at", Encode.unixDateTime event.CreatedAt
                  "kind", Encode.Enum.int event.Kind
                  "tags", Tag.Encode.tagList event.Tags
                  "content", Encode.string event.Content
                  "sig", Encode.schnorrSignature event.Signature ]

    let serializeForEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        Encode.toCanonicalForm (
            Encode.list
                [ Encode.int 0
                  Encode.xOnlyPubkey pubkey
                  Encode.unixDateTime event.CreatedAt
                  Encode.Enum.int event.Kind
                  Tag.Encode.tagList event.Tags
                  Encode.string event.Content ]
        )

    let serialize event =
        event |> Encode.event |> Encode.toCanonicalForm

    let create kind tags content =
        { CreatedAt = DateTime.UtcNow
          Kind = kind
          Tags = tags
          Content = content }

    let createNote content = create Kind.Text [] content

    let createReplyEvent (replyTo: EventId) content =
        create Kind.Text [ Tag.replyTag replyTo "" ] content

    let createDeleteEvent (ids: EventId list) content =
        create Kind.Delete (ids |> List.map  Tag.eventRefTag) content

    let createProfileEvent (profile : Profile) =
        create Kind.Metadata [] (profile |> Profile.Encode.profile |> Encode.toCanonicalForm)

    let sharedKey (XOnlyPubKey he) (mySecret: ECPrivKey) =
        let ecPubKey = ReadOnlySpan(Array.insertAt 0 2uy (he.ToBytes()))
        let hisPubKey = ECPubKey.Create ecPubKey
        let sharedPubKey = hisPubKey.GetSharedPubkey(mySecret).ToBytes()
        sharedPubKey[1..]

    let createEncryptedDirectMessage (recipient: XOnlyPubKey) (secret: ECPrivKey) content =
        let sharedPubKey = sharedKey recipient secret
        let iv, encryptedContent = Encryption.encrypt sharedPubKey content

        create
            Kind.Encrypted
            [ Tag.encryptedTo recipient ]
            $"{Convert.ToBase64String(encryptedContent)}?iv={Convert.ToBase64String(iv)}"

    let decryptDirectMessage (secret: ECPrivKey) (event: Event) =
        let message = event.Content
        let parts = message.Split "?iv=" |> Array.map Convert.FromBase64String
        let sharedPubKey = sharedKey event.PubKey secret
        Encryption.decrypt sharedPubKey parts[1] parts[0]

    let toUnsignedEvent (event: Event) =
        { CreatedAt = event.CreatedAt
          Kind = event.Kind
          Tags = event.Tags
          Content = event.Content }

    let getEventId (pubkey: XOnlyPubKey) (event: UnsignedEvent) =
        event |> serializeForEventId pubkey |> Encoding.UTF8.GetBytes |> SHA256.HashData

    let sign (secret: ECPrivKey) (event: UnsignedEvent) : Event =
        let pubkey = secret |> Key.getPubKey |> XOnlyPubKey
        let eventId = event |> getEventId pubkey

        { Id = EventId eventId
          PubKey = pubkey
          CreatedAt = event.CreatedAt
          Kind = event.Kind
          Tags = event.Tags
          Content = event.Content
          Signature = secret.SignBIP340 eventId |> SchnorrSignature }

    let verify (event: Event) =
        let EventId id, XOnlyPubKey pubkey, SchnorrSignature signature =
            event.Id, event.PubKey, event.Signature

        let computedId = event |> toUnsignedEvent |> getEventId event.PubKey
        computedId = id && pubkey.SigVerifyBIP340(signature, id)

    let isEphemeral (event: Event) =
        event.Kind >= Kind.EphemeralStart &&
        event.Kind < Kind.EphemeralEnd

    let isReplaceable (event: Event) =
        match event.Kind with
        | Kind.Metadata | Kind.Contacts -> true
        | k when (k >= Kind.ReplaceableStart && k < Kind.ReplaceableEnd) -> true
        | _ -> false

    let isParameterizableReplaceable (event: Event) =
        event.Kind >= Kind.ParameterizableReplaceableStart &&
        event.Kind < Kind.ParameterizableReplaceableEnd

    let expirationUnixDateTime (event: Event) =
        event.Tags
        |> List.ungroup
        |> List.tryFind (fun (tagName, value) -> tagName = "expiration")
        |> Option.bind (fun (_, expirationTagValue) -> expirationTagValue |> Int32.TryParse |> Option.ofTuple)

    let isExpired (event: Event) (datetime: DateTime) =
        event
        |> expirationUnixDateTime
        |> Option.map (fun expirationDate ->
            let unixDateTime = toUnixTime datetime
            expirationDate <= int unixDateTime)
        |> Option.defaultValue false