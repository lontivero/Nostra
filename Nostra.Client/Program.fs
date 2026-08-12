module Client

open System
open System.Collections.Generic
open Microsoft.FSharp.Control
open Nostra
open Nostra.Client.Request
open Nostra.Client
open Thoth.Json.Net

let receivedEvents = HashSet<byte[]> ()
let link text url =
    $"\027]8;;{url}\a{text}\027]8;;\a"

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
        let (EventId eventId) = event.Id

        if not (receivedEvents.Contains eventId) then
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
                    AuthorId.toBytes event.PubKey
            let maybeContact = contacts |> Map.tryFind contactKey
            let author = maybeContact
                         |> Option.map (fun c -> c.metadata.displayName |> Option.defaultValue c.metadata.name)
                         |> Option.defaultValue (Utils.toHex contactKey)
            let authorNpub = Shareable.encodeNpub event.PubKey
            let nevent = Shareable.encodeNevent (event.Id, [], Some event.PubKey, Some event.Kind)
            let emoji = match event.Kind with
                        | Kind.Text -> "📄"
                        | Kind.ChannelMessage -> "📢"
                        | _ -> "🥑"
            Console.ForegroundColor <- ConsoleColor.Cyan
            let eventLink = link emoji $"https://njump.me/{nevent}"
            let authorLink= link $"👤 {author}" $"https://njump.me/{authorNpub}"
            Console.WriteLine $"{eventLink} {authorLink} 📅 {event.CreatedAt}"
            Console.ForegroundColor <- enum<ConsoleColor> -1
            Console.WriteLine (event.Content.Trim())
            Console.WriteLine ()
            receivedEvents.Add eventId |> ignore

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
    | _ -> ()

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

let executeCommand (globalOpts: CliArgsParser.GlobalOptions) userFilePath cmd =
    match cmd with
    | CliArgsParser.CreateUser (name, displayName, about, picture, nip05) ->
        let user = User.createUser name displayName about picture nip05
        User.save userFilePath user

    | CliArgsParser.AddRelay urls ->
        let relays = urls |> List.map Uri
        User.apply userFilePath (User.addRelays relays globalOpts.Proxy)

    | CliArgsParser.RemoveRelay urls ->
        let relays = urls |> List.map Uri
        User.apply userFilePath (User.removeRelays relays)

    | CliArgsParser.SubscribeAuthor npubs ->
        let authors = npubs |> List.map Shareable.decodeNpub |> List.lift |> Option.get
        User.apply userFilePath (User.subscribeAuthors authors)

    | CliArgsParser.UnsubscribeAuthor npubs ->
        let authors = npubs |> List.map Shareable.decodeNpub |> List.lift |> Option.get
        User.apply userFilePath (User.unsubscribeAuthors authors)

    | CliArgsParser.SubscribeChannel noteIds ->
        let channels = noteIds |> List.map Shareable.decodeNote |> List.lift |> Option.get
        User.apply userFilePath (User.subscribeChannels channels)

    | CliArgsParser.UnsubscribeChannel noteIds ->
        let channels = noteIds |> List.map Shareable.decodeNote |> List.lift |> Option.get
        User.apply userFilePath (User.unsubscribeChannels channels)

    | CliArgsParser.Create (text, shouldPublish) ->
        let user = User.load userFilePath
        let secret = globalOpts.Secret |> Option.bind Shareable.decodeNsec |> Option.defaultValue user.secret
        let referenceTags = Content.extractReferences text
        let event = Event.create Kind.Text referenceTags text |> Event.sign secret
        if shouldPublish then
            publish event user.relays
        Console.WriteLine (Event.serialize event)
        Console.WriteLine (Shareable.encodeNevent (event.Id, [], Some event.PubKey, Some event.Kind))

    | CliArgsParser.PublishToChannel (channelOpt, messageOpt) ->
        let channel', message =
            match channelOpt, messageOpt with
            | None, None -> StdIn.read "Channel", StdIn.read "Message"
            | Some c, None -> c, StdIn.read "Message"
            | Some c, Some m -> c, m
            | None, Some m -> StdIn.read "Channel", m

        let channel = Shareable.decodeNpub channel' |> Option.map (fun pubkey -> EventId (AuthorId.toBytes pubkey)) |> Option.get
        let user = User.load userFilePath
        let event = Event.createChannelMessage channel message |> Event.sign user.secret
        publish event user.relays

    | CliArgsParser.WhoAmI ->
        let user = User.load userFilePath
        Console.WriteLine $"name:\t{user.metadata.name}"

    | CliArgsParser.ShowMetadata ->
        let user = User.load userFilePath
        Console.WriteLine (user.metadata |> Metadata.Encode.metadata |> Encode.toString 2)

    | CliArgsParser.ShowContacts ->
        let user = User.load userFilePath
        Console.WriteLine (user.contacts |> List.map Contact.Encode.contact |> Encode.list |> Encode.toString 2)

    | CliArgsParser.ShowPublicKey ->
        let user = User.load userFilePath
        Console.WriteLine (user.secret |> SecretKey.getPubKey |> Shareable.encodeNpub)

    | CliArgsParser.ShowSecretKey ->
        let user = User.load userFilePath
        Console.WriteLine (user.secret |> Shareable.encodeNsec)

    | CliArgsParser.NpubToHex npubs ->
        npubs
        |> List.map Shareable.decodeNpub
        |> List.iter (function
            | Some authorId -> Console.WriteLine (Utils.toHex (AuthorId.toBytes authorId))
            | None -> Console.WriteLine "The entered npub is not well formed")

    | CliArgsParser.HexToNpub hexes ->
        hexes
        |> List.map AuthorId.parse
        |> List.iter (function
            | Ok authorId -> Console.WriteLine (Shareable.encodeNpub authorId)
            | Error _ -> Console.WriteLine "The entered hex is not a valid public key")

    | CliArgsParser.Listen (sinceHoursAgo, limitOpt) ->
        let user = User.load userFilePath

        let since =
            sinceHoursAgo
            |> Option.defaultValue 40
            |> fun hours -> DateTime.UtcNow.AddHours(-(float hours))

        let limit = limitOpt |> Option.defaultValue 1_000

        let filter =
            Filter.all
            |> Filter.since since
            |> Filter.limit limit

        let filterAuthors =
            match user.subscribedAuthors with
            | [] -> None
            | authors ->
                filter
                |> Filter.notes
                |> Filter.authors authors
                |> Some

        let filterChannels =
            match user.subscribedChannels with
            | [] -> None
            | channels ->
                    Some (filter |> Filter.channels channels)

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
            |> List.notInBy AuthorId.equals knownAuthors

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
                 | Author p -> AuthorId.toBytes p)  , c )
            |> Map.ofList

        let addContact contactKey metadata =
            User.apply userFilePath (User.addContact contactKey metadata)

        let display = display contactMap addContact
        let connectSubscribeAndListen uri = async {
            let! relay = connectToRelay uri
            relay.subscribe "all" [(Filter.all |> fun f -> { f with Kinds = [Kind.GitRepositoryAnnouncement; Kind.GitPatch; Kind.GitIssue]})]
            do! relay.startListening display
        }

        user.relays
        |> List.map (fun relay -> connectSubscribeAndListen relay.uri)
        |> List.map Async.Catch
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

[<EntryPoint>]
let Main args =
    let parsed = CliArgsParser.parse args

    let dataDir =
        let dir = parsed.Global.DataDir |> Option.defaultWith DataDirectory.getDefaultDataDirectory
        if not (IO.Directory.Exists dir) then IO.Directory.CreateDirectory dir |> ignore
        dir

    let userFilePath =
        let path = parsed.Global.UserFile
        if IO.Path.IsPathRooted path then path
        else IO.Path.Combine(dataDir, path)

    parsed.Commands
    |> List.iter (executeCommand parsed.Global userFilePath)

    0