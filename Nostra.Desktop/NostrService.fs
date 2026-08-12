namespace Nostra.Desktop

open System
open Nostra
open Nostra.Client
open Nostra.Client.Request
open Nostra.Client.Response
open Thoth.Json.Net

module NostrService =

    let parseAuthorId (query: string) : Result<AuthorId, string> =
        let query = query.Trim()
        if query.StartsWith("npub") then
            match Shareable.decodeNpub query with
            | Some author -> Ok author
            | None -> Error "Invalid npub format"
        else
            match AuthorId.parse query with
            | Ok author -> Ok author
            | Error e -> Error e

    let formatAuthorId (author: AuthorId) =
        Shareable.encodeNpub author

    let authorIdToBytes (author: AuthorId) =
        AuthorId.toBytes author

    type RelaySubscription = {
        Relay: RelayConnection
        SubscribedAuthors: AuthorId list
    }

    type NostrClient = {
        mutable Connection: RelaySubscription option
        OnEvent: FeedEvent -> unit
        OnProfile: AuthorId * Profile -> unit
        OnStatusChange: ConnectionStatus -> unit
        OnMessage: string -> unit
    }

    let createClient onEvent onProfile onStatusChange onMessage = {
        Connection = None
        OnEvent = onEvent
        OnProfile = onProfile
        OnStatusChange = onStatusChange
        OnMessage = onMessage
    }

    let extractReplyTo (tags: Tag list) : EventId option =
        // NIP-10 defines: ["e", <event-id>, <relay-url>, <marker>]
        // where marker can be "root", "reply", or empty
        // With markers: look for "reply" marker for direct parent
        // Without markers (deprecated): first = root, last = reply-to
        let eTags = tags |> List.filter (fun (key, _) -> key = "e")

        // First, look for a tag with "reply" marker
        let replyTag =
            eTags
            |> List.tryFind (fun (_, values) ->
                values |> List.exists (fun v -> v = "reply"))

        let eventIdHex =
            match replyTag with
            | Some (_, eventId :: _) -> Some eventId
            | Some (_, []) | None ->
                // Fallback: use the last "e" tag (deprecated format where last = reply-to)
                match eTags |> List.tryLast with
                | Some (_, eventId :: _) -> Some eventId
                | _ -> None

        eventIdHex
        |> Option.bind (fun hex ->
            match EventId.parse hex with
            | Ok eventId -> Some eventId
            | Error _ -> None)

    let handleRelayMessage (client: NostrClient) (message: Result<RelayMessage, string>) =
        match message with
        | Ok (RMEvent (subscriptionId, event)) ->
            match event.Kind with
            | Kind.Metadata ->
                match Decode.fromString Profile.Decode.profile event.Content with
                | Ok profile ->
                    client.OnProfile(event.PubKey, profile)
                | Error _ -> ()
            | Kind.Text | Kind.Repost ->
                let replyTo = extractReplyTo event.Tags
                let feedEvent = {
                    Id = event.Id
                    Author = event.PubKey
                    AuthorName = None
                    Content = event.Content
                    CreatedAt = event.CreatedAt
                    Kind = event.Kind
                    ReplyTo = replyTo
                }
                client.OnEvent feedEvent
            | _ -> ()
        | Ok (RMEOSE subscriptionId) ->
            client.OnMessage $"Subscription '{subscriptionId}' completed"
        | Ok (RMNotice notice) ->
            client.OnMessage $"Notice: {notice}"
        | Ok (RMACK (_, success, message)) ->
            if not success then
                client.OnMessage $"Error: {message}"
        | Error e ->
            client.OnMessage $"Error: {e}"

    let connectToRelay (client: NostrClient) (relayUrl: string) = async {
        try
            client.OnStatusChange Connecting
            let uri = Uri(relayUrl)
            let! relay = Client.connectToRelay uri
            client.Connection <- Some { Relay = relay; SubscribedAuthors = [] }
            client.OnStatusChange Connected
            client.OnMessage $"Connected to {relayUrl}"

            do! relay.startListening (handleRelayMessage client)
        with ex ->
            client.OnStatusChange Disconnected
            client.OnMessage $"Connection failed: {ex.Message}"
    }

    let subscribeToAuthor (client: NostrClient) (author: AuthorId) =
        match client.Connection with
        | Some conn ->
            let since = DateTime.UtcNow.AddDays(-7.0)

            let eventsFilter =
                Filter.all
                |> Filter.notes
                |> Filter.authors [author]
                // |> Filter.since since
                |> Filter.limit 50

            let mentionsFilter =
                Filter.all
                |> Filter.notes
                |> Filter.referenceAuthor [author]
                |> Filter.since since
                |> Filter.limit 20

            let metadataFilter =
                Filter.all
                |> Filter.metadata
                |> Filter.authors [author]

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"feed-{guidStr[..7]}"
            conn.Relay.subscribe subscriptionId [eventsFilter; mentionsFilter; metadataFilter]

            client.Connection <- Some { conn with SubscribedAuthors = author :: conn.SubscribedAuthors }
            client.OnMessage $"Subscribed to {formatAuthorId author}"
        | None ->
            client.OnMessage "Not connected to relay"

    let searchUser (client: NostrClient) (author: AuthorId) = async {
        match client.Connection with
        | Some conn ->
            let metadataFilter =
                Filter.all
                |> Filter.metadata
                |> Filter.authors [author]
                |> Filter.limit 1

            let searchGuidStr = Guid.NewGuid().ToString("N")
            let searchSubscriptionId = $"search-{searchGuidStr[..7]}"
            conn.Relay.subscribe searchSubscriptionId [metadataFilter]
            client.OnMessage $"Searching for {formatAuthorId author}..."
        | None ->
            client.OnMessage "Not connected to relay"
    }

    let fetchProfile (client: NostrClient) (author: AuthorId) =
        match client.Connection with
        | Some conn ->
            let metadataFilter =
                Filter.all
                |> Filter.metadata
                |> Filter.authors [author]
                |> Filter.limit 1

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"profile-{guidStr[..7]}"
            conn.Relay.subscribe subscriptionId [metadataFilter]
        | None -> ()

    let fetchEvents (client: NostrClient) (eventIds: EventId list) =
        match client.Connection with
        | Some conn when not eventIds.IsEmpty ->
            let eventsFilter =
                { Filter.all with Ids = eventIds }
                |> Filter.notes

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"events-{guidStr[..7]}"
            conn.Relay.subscribe subscriptionId [eventsFilter]
        | _ -> ()