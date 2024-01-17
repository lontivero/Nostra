namespace Nostra.Client

open System
open System.IO
open Microsoft.FSharp.Core
open Thoth.Json.Net
open NBitcoin.Secp256k1
open Nostra

module List =
    let notInBy predicate list2 list1 =
        list1
        |> List.filter (fun i1 -> not (List.exists (predicate i1) list2))


type Channel = EventId

type Metadata = {
    name : string
    displayName : string option
    about : string option
    picture : string option
    nip05 : string option
}
type Relay = {
    uri : Uri
    proxy : Uri Option
}
type ContactKey =
    | Author of Author
    | Channel of Channel

type Contact = {
    key : ContactKey
    metadata : Metadata
}
type User = {
    secret : ECPrivKey
    metadata : Metadata
    relays : Relay list
    contacts : Contact list
    subscribedAuthors : Author list
    subscribedChannels : Channel list
}

module Author =
    module Decode =
        let shareableAuthor : Decoder<Author> =
            Decode.string
            |> Decode.map Shareable.decodeNpub
            |> Decode.andThen (Decode.ofOption "Not a valid bech32 author")

    module Encode =
        let shareableAuthor author =
            author |> Shareable.encodeNpub |> Encode.string

module ContactKey =
    open Author
    module Encode =
        let contactKey = function
            | Author author -> Encode.shareableAuthor author
            | Channel channel -> Encode.eventId channel

    module Decode =
        let contactKey : Decoder<ContactKey> =
            Decode.oneOf [
                Decode.eventId |> Decode.map ContactKey.Channel
                Decode.shareableAuthor |> Decode.map ContactKey.Author
            ]

module Metadata =
    module Decode =
        let metadata : Decoder<Metadata> =
            Decode.object (fun get -> {
                name = get.Required.Field "name" Decode.string
                displayName = get.Optional.Field "display_name" Decode.string
                picture = get.Optional.Field "picture" Decode.string
                about = get.Optional.Field "about" Decode.string
                nip05 = get.Optional.Field "nip05" Decode.string
                })

    module Encode =
        let metadata metadata =
            let getOptionalField value key =
                value
                |> Option.map (fun picture -> [ key, Encode.string picture ])
                |> Option.defaultValue []

            let mandatoryFields = [
                "name", Encode.string metadata.name
            ]
            let picture = getOptionalField metadata.picture "picture"
            let about = getOptionalField metadata.about "about"
            let nip05 = getOptionalField metadata.nip05 "nip05"
            let displayName = getOptionalField metadata.displayName "display_name"
            Encode.object (mandatoryFields @ picture @ about @ displayName @ nip05)

module Contact =
    open Author
    open Metadata
    open ContactKey

    module Encode =
        let contact contact =
            Encode.object [
                "key", Encode.contactKey contact.key
                "metadata", Encode.metadata contact.metadata
            ]

    module Decode =
        let contact : Decoder<Contact> =
            Decode.object (fun get -> {
                key = get.Required.Field "key" Decode.contactKey
                metadata = get.Required.Field "metadata" Decode.metadata
            })

module User =
    open Metadata

    let createUser name displayName about picture nip05 =
        let secret = Key.createNewRandom ()
        {
            secret = secret
            metadata = {
                name = name
                displayName = displayName
                about = about
                picture = picture
                nip05 = nip05
            }
            relays = []
            contacts = []
            subscribedAuthors = []
            subscribedChannels = []
        }

    module Decode =
        open Author
        open Contact

        let relay : Decoder<Relay> =
            Decode.object (fun get -> {
                uri = get.Required.Field "uri" Decode.uri
                proxy = get.Optional.Field "proxy" Decode.uri
            })

        let secret : Decoder<ECPrivKey> =
            Decode.string
            |> Decode.map Shareable.decodeNsec
            |> Decode.andThen (Decode.ofOption "Not a valid bech32 secret")

        let channel : Decoder<Channel> =
            Decode.eventId

        let user : Decoder<User> =
            Decode.object (fun get -> {
                secret = get.Required.Field "secret" secret
                metadata = get.Required.Field "metadata" Decode.metadata
                relays = get.Required.Field "relays" (Decode.list relay)
                contacts = get.Required.Field "contacts" (Decode.list Decode.contact)
                subscribedAuthors = get.Required.Field "subscribed_authors" (Decode.list Decode.shareableAuthor)
                subscribedChannels = get.Required.Field "subscribed_channels" (Decode.list channel)
            })

    module Encode =
        open Author
        open Contact

        let relay relay =
            let proxy = match relay.proxy with | Some p -> [ "proxy", Encode.uri p ] | None -> []
            Encode.object ([
                "uri", Encode.uri relay.uri
            ] @ proxy)

        let secret secret =
            secret |> Shareable.encodeNsec |> Encode.string
        let channel =
            Encode.eventId

        let user user =
            Encode.object [
                "secret", secret user.secret
                "metadata", Encode.metadata user.metadata
                "relays", Encode.list (List.map relay user.relays)
                "contacts", Encode.list (List.map Encode.contact user.contacts)
                "subscribed_authors", Encode.list (List.map Encode.shareableAuthor user.subscribedAuthors)
                "subscribed_channels", Encode.list (List.map channel user.subscribedChannels)
            ]

    let addRelays relays proxy user =
        let addedRelays =
            relays
            |> List.map (fun r -> {
                uri = r
                proxy = proxy |> Option.map Uri
            })
        let existingRelays  =
            user.relays
            |> List.notInBy (fun relay1 relay2 -> relay1.uri = relay2.uri) addedRelays

        { user with relays = existingRelays @ addedRelays }

    let removeRelays relays user =
        let relays' =
            user.relays
            |> List.notInBy (fun relay uri -> uri = relay.uri) relays
        { user with relays = relays' }

    let addContact key metadata user =
        let contacts = { key = key; metadata = metadata }:: user.contacts
        { user with contacts = List.distinctBy (fun c -> c.key) contacts }

    let subscribeAuthors (authors : Author list) user =
        let authors' = List.distinctBy Author.toBytes (user.subscribedAuthors @ authors)
        { user with subscribedAuthors = authors' }

    let unsubscribeAuthors (authors : Author list) user =
        let authors' = user.subscribedAuthors
                       |> List.notInBy (fun a1 a2 -> Author.toBytes a1 = Author.toBytes a2) authors
        { user with subscribedAuthors = authors' }

    let subscribeChannels (channels : Channel list) user =
        let channels' = List.distinctBy EventId.toBytes (user.subscribedChannels @ channels)
        { user with subscribedChannels = channels' }

    let unsubscribeChannels (channels : Channel list) user =
        let channels' = user.subscribedChannels
                       |> List.notInBy (fun (EventId a1) (EventId a2) -> a1 = a2) channels
        { user with subscribedChannels = channels' }

    let save filePath user =
        let json = user |> Encode.user |> Encode.toString 2
        File.WriteAllText (filePath, json)

    let load filePath =
        File.ReadAllText filePath
        |> Decode.fromString Decode.user
        |> function
            | Ok user -> user
            | Error e -> failwith e //"user file is not a valid json"

    let apply filePath action =
        filePath
        |> load
        |> action
        |> save filePath