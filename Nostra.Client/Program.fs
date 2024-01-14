module Client

open System
open System.Collections.Generic
open Microsoft.FSharp.Control
open Nostra
open Nostra.Client.Request
open Nostra.Client
open Thoth.Json.Net

let receivedEvents = HashSet<byte[]> ()

let displayResponse (contacts : Map<byte[], Contact>) (addContact: ContactKey -> Metadata -> unit) = function
    | Ok (Response.RMEvent ("channelmetadata", event)) ->
        let contactKey = Channel event.Id
        let metadataResult = Decode.fromString Metadata.Decode.metadata event.Content
        match metadataResult with
        | Ok metadata -> addContact contactKey metadata
        | Error _ -> ()
    | Ok (Response.RMEvent ("metadata", event)) ->
        let contactKey = Author event.PubKey
        let metadataResult = Decode.fromString Metadata.Decode.metadata event.Content
        match metadataResult with
        | Ok metadata -> addContact contactKey metadata
        | Error _ -> ()
    | Ok (Response.RMEvent ("all", event)) ->
        let (EventId eventid) = event.Id

        if not (receivedEvents.Contains eventid) then
            let contactKey =
                if event.Kind = Kind.ChannelMessage then
                    let eventId =
                            event.Tags
                            |> List.choose (function
                                | "e", [channel; _; "root"] -> Some (EventId.parse channel)
                                | _ -> None)
                            |> List.head
                            |> Result.requiresOk

                    EventId.toBytes eventId
                else
                    XOnlyPubKey.toBytes event.PubKey
            let maybeContact = contacts |> Map.tryFind contactKey
            let author = maybeContact |> Option.map (_.metadata.name) |> Option.defaultValue (Utils.toHex contactKey)
            let nevent = Shareable.encodeNevent (event.Id, [], Some event.PubKey, Some event.Kind)
            let emoji = match event.Kind with
                        | Kind.Text -> "📄"
                        | Kind.ChannelMessage -> "📢"
                        | _ -> "🥑"
            Console.ForegroundColor <- ConsoleColor.Cyan
            Console.WriteLine $"{emoji} 👤{author} 📅{event.CreatedAt}"
            Console.WriteLine $"https://njump.me/{nevent}"
            Console.ForegroundColor <- enum<ConsoleColor> (-1)
            Console.WriteLine (event.Content.Trim())
            //Console.ForegroundColor <- ConsoleColor.DarkGray
            //Console.WriteLine (event.Tags |> List.map (fun (t, vs) -> $"{t}:{vs}"))
            Console.WriteLine ()
            receivedEvents.Add eventid |> ignore

    | Ok (Response.RMACK(eventId, success, message)) ->
        Console.ForegroundColor <- ConsoleColor.Green
        let (EventId eid) = eventId
        Console.WriteLine $"Event: {eid |> Utils.toHex} Success: {success} = {message}"
    | Ok (Response.RMNotice message) ->
        Console.ForegroundColor <- ConsoleColor.Yellow
        Console.WriteLine message
    | Ok (Response.RMEOSE subscriptionId) ->
        Console.ForegroundColor <- ConsoleColor.DarkGray
        Console.WriteLine $">>> {subscriptionId} Done"
    | Error e ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine (e.ToString())

let sync = obj
let display (contacts : Map<byte[], Contact>) response =
    lock sync (fun () ->
        displayResponse contacts response)

let publish event relays =
    let publishedSuccessfullyTo =
        relays
        |> List.map (fun userRelay -> async {
            let! relay = connectToRelay userRelay.uri
            relay.publish event
            return userRelay.uri
        })
        |> List.map Async.Catch
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.toList
        |> List.map Option.ofChoice
        |> List.choose id

    let shareableRelays = publishedSuccessfullyTo |> List.map _.ToString()
    let nevent = Shareable.encodeNevent (event.Id, shareableRelays, Some event.PubKey, Some event.Kind )
    Console.WriteLine nevent

[<EntryPoint>]
let Main args =
    let opts = CommandLineParser.parseArgs args

    let userFilePath = opts.getUserFilePath ()

    if opts.isCreateUser() then
        let user = User.createUser
                       (opts.getName())
                       (opts.getAbout())
                       (opts.getPicture())
                       (opts.getNip05())
        User.save userFilePath user

    if opts.isAddRelay() then
        let relays = opts.getRelaysToAdd() |> List.map Uri
        let proxy = opts.getProxy()
        User.apply userFilePath (User.addRelays relays proxy)

    if opts.isRemoveRelay() then
        let relays = opts.getRelaysToRemove() |> List.map Uri
        User.apply userFilePath (User.removeRelays relays)

    if opts.isSubscribeAuthor() then
        let authors = opts.getSubcribeAuthor() |> List.map Shareable.decodeNpub |> List.lift |> Option.get
        User.apply userFilePath (User.subscribeAuthors authors)

    if opts.isUnsubscribeAuthor() then
        let authors = opts.getSubcribeAuthor() |> List.map Shareable.decodeNpub |> List.lift |> Option.get
        User.apply userFilePath (User.unsubscribeAuthors authors)

    if opts.isSubscribeChannel() then
        let channels = opts.getSubcribeChannel() |> List.map Shareable.decodeNote |> List.lift |> Option.get
        User.apply userFilePath (User.subscribeChannels channels)

    if opts.isUnsubscribeChannel() then
        let channels = opts.getSubcribeChannel() |> List.map Shareable.decodeNote |> List.lift |> Option.get
        User.apply userFilePath (User.unsubscribeChannels channels)

    if opts.isPublish() then
        let message = opts.getMessageToPublish()
        let user = User.load userFilePath
        let event = Event.createNote message |> Event.sign user.secret
        publish event user.relays

    if opts.isPublishToChannel() then
        let channel', message =
            match opts.getMessageToChannel() with
            | None  | Some [] -> (StdIn.read "Channel"), StdIn.read "Message"
            | Some [c] -> c, StdIn.read "Message"
            | Some (c::msgs) -> c, msgs |> List.head

        let channel = Shareable.decodeNpub channel' |> Option.map (fun pubkey -> EventId (XOnlyPubKey.toBytes pubkey) ) |> Option.get
        let user = User.load userFilePath
        let event = Event.createChannelMessage channel message |> Event.sign user.secret
        publish event user.relays

    if opts.isListen() then
        let user = User.load userFilePath
        let filter =
            Filter.all
            |> Filter.since (DateTime.Today.AddDays -58)
            |> Filter.limit 100

        let filterAuthors =
            filter
            |> Filter.notes
            |> Filter.authors user.subscribedAuthors

        let filterChannels =
            filter
            |> Filter.channels user.subscribedChannels

        let knownChannels =
            user.contacts
            |> List.choose (fun c -> match c.key with
                                     | Channel channel -> Some channel
                                     | _ -> None )

        let unknownChannels =
            user.subscribedChannels
            |> List.notInBy (fun (EventId x) (EventId y) -> x = y ) knownChannels

        let filterChannelMetadata =
            match unknownChannels with
            | [] -> None
            | channels ->
                Filter.all
                |> Filter.channelCreation channels
                |> Some

        let knownAuthors =
            user.contacts
            |> List.choose (fun c -> match c.key with
                                     | Author author -> Some author
                                     | _ -> None )

        let unknownAuthors =
            user.subscribedAuthors
            |> List.notInBy XOnlyPubKey.equals knownAuthors

        let filterMetadata =
            match unknownAuthors with
            | [] -> None
            | authors ->
                Filter.all
                |> Filter.metadata
                |> Filter.authors authors
                |> Some

        let contactMap =
            user.contacts
            |> List.map (fun c ->
                (match c.key with
                 | Channel e -> EventId.toBytes e
                 | Author p -> XOnlyPubKey.toBytes p)  , c )
            |> Map.ofList

        let addContact contactKey metadata =
            User.apply userFilePath (User.addContact contactKey metadata)

        let display = display contactMap addContact
        let connectSubscribeAndListen uri = async {
            let! relay = connectToRelay uri
            relay.subscribe "all" [filterAuthors; filterChannels]
            filterMetadata
            |> Option.iter (fun filter -> relay.subscribe "metadata" [filter])

            filterChannelMetadata
            |> Option.iter (fun filter -> relay.subscribe "channelmetadata" [filter])
            do! relay.startListening display
        }

        user.relays
        |> List.map (fun relay -> connectSubscribeAndListen relay.uri)
        |> List.map Async.Catch
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
    0