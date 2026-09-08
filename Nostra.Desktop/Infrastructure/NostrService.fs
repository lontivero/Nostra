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
        Relay: RelayClient
        mutable SubscribedAuthors: AuthorId list
        mutable MetadataSubscriptions: Set<string>  // Track metadata-only subscriptions for EOSE handling
        OnEvent: FeedEvent -> unit
        OnProfile: AuthorId * Profile -> unit
        OnContacts: AuthorId * AuthorId list -> unit
        OnRelayList: AuthorId * string list -> unit
        OnRelayStatusChange: string * RelayStatus -> unit
        OnMetadataEose: string -> unit  // Called when metadata subscription completes (EOSE)
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

    /// Extract contacts (followed authors) from a Kind 3 event
    let private extractContacts (tags: Tag list) : AuthorId list =
        tags
        |> List.choose (fun (key, values) ->
            if key = "p" then
                match values with
                | pubkeyHex :: _ ->
                    match AuthorId.parse pubkeyHex with
                    | Ok author -> Some author
                    | Error _ -> None
                | [] -> None
            else None)

    /// Extract relay URLs from a Kind 10002 event
    let private extractRelayList (tags: Tag list) : string list =
        tags
        |> List.choose (fun (key, values) ->
            if key = "r" then
                match values with
                | relayUrl :: _ -> Some relayUrl
                | [] -> None
            else None)

    let handleRelayMessage (client: NostrClient) (message: Result<RelayMessage, string>) =
        match message with
        | Ok (RMEvent (sid, event)) ->
            match event.Kind with
            | Kind.Metadata ->
                match Decode.fromString Profile.Decode.profile event.Content with
                | Ok profile ->
                    client.OnProfile(event.PubKey, profile)
                | Error _ -> ()
            | Kind.Contacts ->
                let contacts = extractContacts event.Tags
                client.OnContacts(event.PubKey, contacts)
            | Kind.RelayList ->
                let relays = extractRelayList event.Tags
                client.OnRelayList(event.PubKey, relays)
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
            // Check if this is a metadata subscription we're tracking
            if client.MetadataSubscriptions.Contains subscriptionId then
                client.MetadataSubscriptions <- client.MetadataSubscriptions.Remove subscriptionId
                client.OnMetadataEose subscriptionId
            client.OnMessage $"[{client.RelayUrl}] Subscription '{subscriptionId}' completed"
        | Ok (RMNotice notice) ->
            client.OnMessage $"[{client.RelayUrl}] Notice: {notice}"
        | Ok (RMACK (_, success, msg)) ->
            if not success then
                client.OnMessage $"[{client.RelayUrl}] Error: {msg}"
        | Ok (RMCount _) -> ()
        | Error e ->
            client.OnMessage $"[{client.RelayUrl}] Error: {e}"

    /// Connect to a relay and create a client.
    /// Returns the client immediately and starts listening in the background.
    let connectToRelay (relayUrl: string) onEvent onProfile onContacts onRelayList onRelayStatusChange onMetadataEose onMessage onClientReady = async {
        try
            onRelayStatusChange(relayUrl, RelayConnecting)
            let uri = Uri(relayUrl)
            let! relay = Client.connectToRelay uri
            let client = {
                RelayUrl = relayUrl
                Relay = relay
                SubscribedAuthors = []
                MetadataSubscriptions = Set.empty
                OnEvent = onEvent
                OnProfile = onProfile
                OnContacts = onContacts
                OnRelayList = onRelayList
                OnRelayStatusChange = onRelayStatusChange
                OnMetadataEose = onMetadataEose
                OnMessage = onMessage
            }
            onRelayStatusChange(relayUrl, RelayConnected)
            onMessage $"Connected to {relayUrl}"

            // Notify that client is ready before starting the blocking listen loop
            onClientReady client

            // This blocks until the connection is closed
            do! relay.startListening (handleRelayMessage client)
        with ex ->
            onRelayStatusChange(relayUrl, RelayDisconnected)
            onMessage $"Connection to {relayUrl} failed: {ex.Message}"
    }

    /// Subscribe to a list of authors on this client's relay
    let subscribeAuthors (client: NostrClient) (authors: AuthorId list) (since: DateTime) =
        if not authors.IsEmpty then
            let eventsFilter =
                Filter.all
                |> Filter.notes
                |> Filter.authors authors
                |> Filter.since since
                |> Filter.limit 50

            let metadataFilter =
                Filter.all
                |> Filter.metadata
                |> Filter.authors authors

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"feed-{guidStr[..7]}"
            client.Relay.subscribe subscriptionId [eventsFilter; metadataFilter]

            client.SubscribedAuthors <- authors
            client.OnMessage $"Subscribed to {authors.Length} author(s) on {client.RelayUrl}"

    /// Subscribe to metadata only (profiles + relay lists) for authors.
    /// Returns the subscription ID for EOSE tracking.
    let subscribeMetadataOnly (client: NostrClient) (authors: AuthorId list) : string option =
        if authors.IsEmpty then
            None
        else
            let profileFilter =
                Filter.all
                |> Filter.metadata
                |> Filter.authors authors

            let relayListFilter =
                Filter.all
                |> Filter.relayList
                |> Filter.authors authors

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"metadata-{guidStr[..7]}"

            // Track this subscription for EOSE handling
            client.MetadataSubscriptions <- client.MetadataSubscriptions.Add subscriptionId

            client.Relay.subscribe subscriptionId [profileFilter; relayListFilter]
            client.OnMessage $"Fetching metadata for {authors.Length} author(s) on {client.RelayUrl}"
            Some subscriptionId

    /// Subscribe to events only (notes) for authors. Used after metadata is loaded.
    let subscribeEventsOnly (client: NostrClient) (authors: AuthorId list) (since: DateTime) =
        if not authors.IsEmpty then
            let eventsFilter =
                Filter.all
                |> Filter.notes
                |> Filter.authors authors
                |> Filter.since since
                |> Filter.limit 50

            let guidStr = Guid.NewGuid().ToString("N")
            let subscriptionId = $"events-{guidStr[..7]}"
            client.Relay.subscribe subscriptionId [eventsFilter]

            client.SubscribedAuthors <- authors
            client.OnMessage $"Subscribed to events from {authors.Length} author(s) on {client.RelayUrl}"

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

    /// Check if a string looks like a NIP-05 identifier (contains @)
    let isNip05Identifier (query: string) =
        query.Contains("@") && not (query.StartsWith("@"))

    /// Parse a NIP-05 identifier into (name, domain)
    let parseNip05 (identifier: string) =
        let parts = identifier.Split('@')
        if parts.Length = 2 then
            let name = if parts.[0] = "_" then "_" else parts.[0].ToLowerInvariant()
            let domain = parts.[1].ToLowerInvariant()
            Some (name, domain)
        else
            None

    /// Lookup a NIP-05 identifier and return the associated public key
    let lookupNip05 (identifier: string) = async {
        match parseNip05 identifier with
        | None -> return Error "Invalid NIP-05 identifier format"
        | Some (name, domain) ->
            try
                use client = new HttpClient()
                client.Timeout <- TimeSpan.FromSeconds(10.0)
                client.DefaultRequestHeaders.Add("User-Agent", "Nostra/1.0")

                let url = $"https://{domain}/.well-known/nostr.json?name={name}"
                let! response = client.GetAsync(url) |> Async.AwaitTask

                if response.IsSuccessStatusCode then
                    let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask

                    // Parse the JSON: {"names": {"username": "hex_pubkey"}}
                    let decoder =
                        Decode.field "names" (Decode.dict Decode.string)

                    match Decode.fromString decoder content with
                    | Ok names ->
                        match Map.tryFind name names with
                        | Some hexPubKey ->
                            match parseAuthorId hexPubKey with
                            | Ok author -> return Ok author
                            | Error e -> return Error $"Invalid public key in NIP-05 response: {e}"
                        | None ->
                            return Error $"Name '{name}' not found at {domain}"
                    | Error e ->
                        return Error $"Failed to parse NIP-05 response: {e}"
                else
                    return Error $"NIP-05 lookup failed: HTTP {int response.StatusCode}"
            with ex ->
                return Error $"NIP-05 lookup failed: {ex.Message}"
    }

    // URL Preview types and functions
    type UrlPreviewData = {
        Image: string option
        Title: string option
        Description: string option
        SiteName: string option
    }

    type TweetPreviewData = {
        AuthorName: string
        AuthorHandle: string
        Content: string
        ProfileImage: string option
    }

    type PreviewResult =
        | WebPreview of UrlPreviewData
        | TweetPreview of TweetPreviewData
        | NoPreview

    let private extractMetaContent (html: string) (property: string) =
        let patterns = [
            $"""<meta[^>]+property=["']{property}["'][^>]+content=["']([^"']+)["']"""
            $"""<meta[^>]+content=["']([^"']+)["'][^>]+property=["']{property}["']"""
            $"""<meta[^>]+name=["']{property}["'][^>]+content=["']([^"']+)["']"""
            $"""<meta[^>]+content=["']([^"']+)["'][^>]+name=["']{property}["']"""
        ]
        patterns
        |> List.tryPick (fun pattern ->
            let regex = System.Text.RegularExpressions.Regex(pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
            let m = regex.Match(html)
            if m.Success then Some (System.Net.WebUtility.HtmlDecode(m.Groups.[1].Value))
            else None)

    let private isTwitterUrl (url: string) =
        url.Contains("twitter.com") || url.Contains("x.com")

    /// Fetch URL preview data (og:image, og:title, og:description, og:site_name)
    let fetchUrlPreview (url: string) = async {
        try
            use client = new HttpClient()
            client.Timeout <- TimeSpan.FromSeconds(10.0)
            client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (compatible; Nostra/1.0)")

            let! response = client.GetAsync(url) |> Async.AwaitTask
            if response.IsSuccessStatusCode then
                let! html = response.Content.ReadAsStringAsync() |> Async.AwaitTask

                if isTwitterUrl url then
                    // Try to extract tweet info from meta tags
                    let authorName = extractMetaContent html "og:title" |> Option.defaultValue "Tweet"
                    let content = extractMetaContent html "og:description" |> Option.defaultValue ""
                    let profileImage = extractMetaContent html "og:image"

                    // Try to extract handle from URL
                    let handleRegex = System.Text.RegularExpressions.Regex(@"(?:twitter\.com|x\.com)/([^/]+)")
                    let handleMatch = handleRegex.Match(url)
                    let handle = if handleMatch.Success then $"@{handleMatch.Groups.[1].Value}" else "@unknown"

                    return TweetPreview {
                        AuthorName = authorName
                        AuthorHandle = handle
                        Content = content
                        ProfileImage = profileImage
                    }
                else
                    let image = extractMetaContent html "og:image"
                    let title = extractMetaContent html "og:title"
                    let description = extractMetaContent html "og:description"
                    let siteName = extractMetaContent html "og:site_name"

                    if image.IsSome || title.IsSome then
                        return WebPreview {
                            Image = image
                            Title = title
                            Description = description
                            SiteName = siteName
                        }
                    else
                        return NoPreview
            else
                return NoPreview
        with _ ->
            return NoPreview
    }

    /// Fetch account metadata (profile, contacts, relay list) for an author
    let fetchAccountMetadata (client: NostrClient) (author: AuthorId) =
        let metadataFilter =
            Filter.all
            |> Filter.metadata
            |> Filter.authors [author]
            |> Filter.limit 1

        let contactsFilter =
            Filter.all
            |> Filter.contacts
            |> Filter.authors [author]
            |> Filter.limit 1

        let relayListFilter =
            Filter.all
            |> Filter.relayList
            |> Filter.authors [author]
            |> Filter.limit 1

        let guidStr = Guid.NewGuid().ToString("N")
        let subscriptionId = $"account-metadata-{guidStr[..7]}"
        client.Relay.subscribe subscriptionId [metadataFilter; contactsFilter; relayListFilter]
        client.OnMessage $"Fetching account metadata for {formatAuthorId author}"

    /// Fetch a profile image and return the bytes
    let fetchProfileImage (imageUrl: string) = async {
        try
            use client = new HttpClient()
            client.Timeout <- TimeSpan.FromSeconds(15.0)
            client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (compatible; Nostra/1.0)")

            let! response = client.GetAsync(imageUrl) |> Async.AwaitTask
            if response.IsSuccessStatusCode then
                let! bytes = response.Content.ReadAsByteArrayAsync() |> Async.AwaitTask
                if bytes.Length > 0 && bytes.Length < 1_000_000 then
                    return Some bytes
                else
                    printfn "[TRACE] Image too large or empty: %d bytes" bytes.Length
                    return None
            else
                printfn "[TRACE] Image fetch HTTP error: %d" (int response.StatusCode)
                return None
        with ex ->
            printfn "[TRACE] Image fetch exception: %s" ex.Message
            return None
    }