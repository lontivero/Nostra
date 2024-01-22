﻿namespace Nostra

open System
open System.Security.Cryptography
open System.Text
open Microsoft.FSharp.Core
open NBitcoin.Secp256k1
open Thoth.Json.Net

[<Struct>]
type Kind =
    | Metadata = 0
    | Text = 1
    | Recommend = 2
    | Contacts = 3
    | Encrypted = 4
    | Delete = 5
    | Repost = 6
    | Reaction = 7
    | ChannelCreate = 40
    | ChannelMetadata = 41
    | ChannelMessage = 42
    | HideMessage = 43
    | MuteUser = 44
    | ReplaceableStart = 10_000
    | RelayList = 10_002
    | ReplaceableEnd = 20_000
    | EphemeralStart = 20_000
    | EphemeralEnd = 30_000
    | ParameterizableReplaceableStart = 30_000
    | ParameterizableReplaceableEnd = 40_000

[<CompiledName("EventT")>]
type Event =
    { Id: EventId
      PubKey: AuthorId
      CreatedAt: DateTime
      Kind: Kind
      Tags: Tag list
      Content: string
      Signature: SchnorrSignature }

[<CompiledName("Event")>]
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
                  PubKey = get.Required.Field "pubkey" Decode.authorId
                  CreatedAt = get.Required.Field "created_at" Decode.unixDateTime
                  Kind = get.Required.Field "kind" Decode.Enum.int
                  Tags = get.Required.Field "tags" Tag.Decode.tagList
                  Content = get.Required.Field "content" Decode.string
                  Signature = get.Required.Field "sig" Decode.schnorrSignature })

    module Encode =
        let event (event: Event) =
            Encode.object
                [ "id", Encode.eventId event.Id
                  "pubkey", Encode.authorId event.PubKey
                  "created_at", Encode.unixDateTime event.CreatedAt
                  "kind", Encode.Enum.int event.Kind
                  "tags", Tag.Encode.tagList event.Tags
                  "content", Encode.string event.Content
                  "sig", Encode.schnorrSignature event.Signature ]

    let serializeForEventId (author: AuthorId) (event: UnsignedEvent) =
        Encode.toCanonicalForm (
            Encode.list
                [ Encode.int 0
                  Encode.authorId author
                  Encode.unixDateTime event.CreatedAt
                  Encode.Enum.int event.Kind
                  Tag.Encode.tagList event.Tags
                  Encode.string event.Content ]
        )

    [<CompiledName("Serialize")>]
    let serialize event =
        event |> Encode.event |> Encode.toCanonicalForm

    [<CompiledName("Create")>]
    let create kind tags content =
        { CreatedAt = DateTime.UtcNow
          Kind = kind
          Tags = tags |> Seq.toList
          Content = content }

    [<CompiledName("CreateNote")>]
    let createNote content = create Kind.Text [] content

    let createContactsEvent contacts =
        create Kind.Contacts (contacts |> List.map Tag.authorTag) ""

    [<CompiledName("CreateReply")>]
    let createReplyEvent (replyTo: EventId) content =
        create Kind.Text [ Tag.replyTag replyTo "" ] content

    [<CompiledName("CreateChannel")>]
    let createChannelMessage (replyTo: EventId) content =
        create Kind.ChannelMessage [ Tag.rootEventRefTag replyTo ] content

    [<CompiledName("CreateDelete")>]
    let createDeleteEvent (ids: EventId list) content =
        create Kind.Delete (ids |> List.map  Tag.eventRefTag) content

    [<CompiledName("CreateProfile")>]
    let createProfileEvent (profile : Profile) =
        create Kind.Metadata [] (profile |> Profile.Encode.profile |> Encode.toCanonicalForm)

    let createRelayListEvent relays =
        create Kind.RelayList (relays |> List.map Tag.relayTag)

    let sharedKey (AuthorId he) (SecretKey mySecret) =
        let ecPubKey = ReadOnlySpan(Array.insertAt 0 2uy (he.ToBytes()))
        let hisPubKey = ECPubKey.Create ecPubKey
        let sharedPubKey = hisPubKey.GetSharedPubkey(mySecret).ToBytes()
        sharedPubKey[1..]

    let createEncryptedDirectMessage (recipient: AuthorId) (secret: SecretKey) content =
        let sharedPubKey = sharedKey recipient secret
        let iv, encryptedContent = Encryption.encrypt sharedPubKey content

        create
            Kind.Encrypted
            [ Tag.encryptedTo recipient ]
            $"{Convert.ToBase64String(encryptedContent)}?iv={Convert.ToBase64String(iv)}"

    let decryptDirectMessage (secret: SecretKey) (event: Event) =
        let message = event.Content
        let parts = message.Split "?iv=" |> Array.map Convert.FromBase64String
        let sharedPubKey = sharedKey event.PubKey secret
        Encryption.decrypt sharedPubKey parts[1] parts[0]

    let toUnsignedEvent (event: Event) =
        { CreatedAt = event.CreatedAt
          Kind = event.Kind
          Tags = event.Tags
          Content = event.Content }

    let getEventId (author: AuthorId) (event: UnsignedEvent) =
        event |> serializeForEventId author |> Encoding.UTF8.GetBytes |> SHA256.HashData

    [<CompiledName("Sign")>]
    let sign (secret: SecretKey) (event: UnsignedEvent) : Event =
        let author = secret |> SecretKey.getPubKey
        let eventId = event |> getEventId author
        { Id = EventId eventId
          PubKey = author
          CreatedAt = event.CreatedAt
          Kind = event.Kind
          Tags = event.Tags
          Content = event.Content
          Signature = SecretKey.sign eventId secret |> SchnorrSignature }

    [<CompiledName("Verify")>]
    let verify (event: Event) =
        let EventId id, AuthorId author, SchnorrSignature signature =
            event.Id, event.PubKey, event.Signature

        let computedId = event |> toUnsignedEvent |> getEventId event.PubKey
        computedId = id && author.SigVerifyBIP340(signature, id)

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
        |> Tag.findByKey "expiration"
        |> List.tryHead
        |> Option.bind (Int32.TryParse >> Option.ofTuple)

    let isExpired (event: Event) (datetime: DateTime) =
        event
        |> expirationUnixDateTime
        |> Option.map (fun expirationDate ->
            let unixDateTime = toUnixTime datetime
            expirationDate <= int unixDateTime)
        |> Option.defaultValue false