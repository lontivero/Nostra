namespace Nostra.Desktop

open System
open System.Net.Http
open Nostra
open Nostra.Client
open Nostra.Client.Request
open Nostra.Client.Response
open Nostra.Relay.InfoDocument
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

    type NostrClient = {
        RelayUrl: string
        Relay: RelayConnection
        mutable SubscribedAuthors: AuthorId list
        OnEvent: FeedEvent -> unit
        OnProfile: AuthorId * Profile -> unit
        OnRelayStatusChange: string * RelayStatus -> unit
        OnMessage: string -> unit
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
        | Ok (RMEvent (_, event)) ->
            match event.Kind with
            | Kind.Metadata ->
                match Decode.fromString Profile.Decode.profile event.Content with
                | Ok profile ->
                    client.OnProfile(event.PubKey, profile)
                | Error _ -> ()
            | Kind.Text | Kind.Repost ->
                let replyTo = extractReplyTo event.Tags
                let rawJson = Event.Encode.event event |> Encode.toString 2
                let feedEvent = {
                    Id = event.Id
                    Author = event.PubKey
                    AuthorName = None
                    Content = event.Content
                    CreatedAt = event.CreatedAt
                    Kind = event.Kind
                    ReplyTo = replyTo
                    Relays = [client.RelayUrl]
                    RawJson = rawJson
                }
                client.OnEvent feedEvent
            | _ -> ()
        | Ok (RMEOSE subscriptionId) ->
            client.OnMessage $"[{client.RelayUrl}] Subscription '{subscriptionId}' completed"
        | Ok (RMNotice notice) ->
            client.OnMessage $"[{client.RelayUrl}] Notice: {notice}"
        | Ok (RMACK (_, success, msg)) ->
            if not success then
                client.OnMessage $"[{client.RelayUrl}] Error: {msg}"
        | Error e ->
            client.OnMessage $"[{client.RelayUrl}] Error: {e}"

    /// Subscribe to a list of authors on this client's relay
    let private subscribeAuthors (client: NostrClient) (authors: AuthorId list) (since: DateTime) =
        if authors.IsEmpty then ()
        else
            let eventsFilter =
                Filter.all
                |> Filter.notes
                |> Filter.authors authors
                |> Filter.since since
                |> Filter.limit 100

            let metadataFilter =
                Filter.all
                |> Filter.metadata
                |> Filter.authors authors

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"feed-{guidStr[..7]}"
            client.Relay.subscribe subscriptionId [eventsFilter; metadataFilter]

            client.SubscribedAuthors <- authors
            client.OnMessage $"Subscribed to {authors.Length} author(s) on {client.RelayUrl}"

    /// Connect to a relay and create a client, optionally subscribing to authors.
    /// Returns the client immediately and starts listening in the background.
    let connectToRelay (relayUrl: string) (authors: AuthorId list) (since: DateTime) onEvent onProfile onRelayStatusChange onMessage onClientReady = async {
        try
            onRelayStatusChange(relayUrl, RelayConnecting)
            let uri = Uri(relayUrl)
            let! relay = Client.connectToRelay uri
            let client = {
                RelayUrl = relayUrl
                Relay = relay
                SubscribedAuthors = []
                OnEvent = onEvent
                OnProfile = onProfile
                OnRelayStatusChange = onRelayStatusChange
                OnMessage = onMessage
            }
            onRelayStatusChange(relayUrl, RelayConnected)
            onMessage $"Connected to {relayUrl}"

            // Subscribe to provided authors on this relay
            if not authors.IsEmpty then
                subscribeAuthors client authors since

            // Notify that client is ready before starting the blocking listen loop
            onClientReady client

            // This blocks until the connection is closed
            do! relay.startListening (handleRelayMessage client)
        with ex ->
            onRelayStatusChange(relayUrl, RelayDisconnected)
            onMessage $"Connection to {relayUrl} failed: {ex.Message}"
    }

    /// Subscribe to an author on this client's relay
    let subscribeToAuthor (client: NostrClient) (author: AuthorId) (since: DateTime) =
        let alreadySubscribed =
            client.SubscribedAuthors
            |> List.exists (fun a -> AuthorId.toBytes a = AuthorId.toBytes author)
        if not alreadySubscribed then
            let eventsFilter =
                Filter.all
                |> Filter.notes
                |> Filter.authors [author]
                |> Filter.since since
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
            client.Relay.subscribe subscriptionId [eventsFilter; mentionsFilter; metadataFilter]

            client.SubscribedAuthors <- author :: client.SubscribedAuthors
            client.OnMessage $"Subscribed to {formatAuthorId author} on {client.RelayUrl}"

    let searchUser (client: NostrClient) (author: AuthorId) =
        let metadataFilter =
            Filter.all
            |> Filter.metadata
            |> Filter.authors [author]
            |> Filter.limit 1

        let searchGuidStr = Guid.NewGuid().ToString("N")
        let searchSubscriptionId = $"search-{searchGuidStr[..7]}"
        client.Relay.subscribe searchSubscriptionId [metadataFilter]
        client.OnMessage $"Searching for {formatAuthorId author} on {client.RelayUrl}..."

    let fetchProfile (client: NostrClient) (author: AuthorId) =
        let metadataFilter =
            Filter.all
            |> Filter.metadata
            |> Filter.authors [author]
            |> Filter.limit 1

        let guidStr = Guid.NewGuid().ToString("N")
        let subscriptionId = $"profile-{guidStr[..7]}"
        client.Relay.subscribe subscriptionId [metadataFilter]

    let fetchEvents (client: NostrClient) (eventIds: EventId list) =
        if not eventIds.IsEmpty then
            let eventsFilter =
                { Filter.all with Ids = eventIds }
                |> Filter.notes

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"events-{guidStr[..7]}"
            client.Relay.subscribe subscriptionId [eventsFilter]

    /// Fetch NIP-11 relay information document
    let fetchRelayInfo (relayUrl: string) = async {
        try
            // Convert wss:// to https:// or ws:// to http://
            let httpUrl =
                relayUrl
                    .Replace("wss://", "https://")
                    .Replace("ws://", "http://")

            use client = new HttpClient()
            client.DefaultRequestHeaders.Add("Accept", "application/nostr+json")

            let! response = client.GetAsync(httpUrl) |> Async.AwaitTask
            if response.IsSuccessStatusCode then
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                match Decode.fromString RelayInfo.decode content with
                | Ok info -> return Ok info
                | Error e -> return Error $"Failed to parse relay info: {e}"
            else
                return Error $"HTTP {int response.StatusCode}: {response.ReasonPhrase}"
        with ex ->
            return Error $"Failed to fetch relay info: {ex.Message}"
    }