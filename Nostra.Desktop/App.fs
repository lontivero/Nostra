namespace Nostra.Desktop

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Threading
open Nostra
open Nostra.Desktop.Domain
open Nostra.Desktop.Infrastructure
open Nostra.Desktop.Store
open Styles

module App =
    open NostrService
    open global.Elmish

    let mutable nostrClients: Map<string, NostrClient> = Map.empty

    // Composed model from all feature modules
    type Model = {
        Store: DomainStore                      // Centralized domain state
        Cache: CacheDb                          // Persistent cache
        Navigation: Navigation.Model
        Home: Home.Model
        Notifications: Notifications.Model
        Bookmarks: Bookmarks.Model
        Search: Search.Model
        Settings: Settings.Model
        Stats: ConnectionStats
        StatusMessage: string
        PendingMetadataSubscriptions: Map<string, AuthorId list>
    }

    // Wrap child messages
    type Msg =
        | NavigationMsg of Navigation.Msg
        | HomeMsg of Home.Msg
        | NotificationsMsg of Notifications.Msg
        | BookmarksMsg of Bookmarks.Msg
        | SearchMsg of Search.Msg
        | SettingsMsg of Settings.Msg
        | StoreMsg of StoreMsg                  // Domain store updates
        | SetStatusMessage of string
        // Events from NostrService
        | NostrEventReceived of FeedEvent
        | NostrProfileReceived of AuthorId * Nostra.Profile
        | ProfileImageFetched of AuthorId * byte[]
        | NostrContactsReceived of AuthorId * AuthorId list
        | NostrRelayListReceived of AuthorId * string list
        | RelayStatusChanged of string * RelayStatus
        | UpdateStats of (ConnectionStats -> ConnectionStats)
        // Metadata-first subscription flow
        | MetadataEoseReceived of subscriptionId: string
        // Account events
        | AccountMetadataLoaded of AuthorId * Nostra.Profile
        | InitializeAccount
        | ConnectToAllRelays

    let mutable dispatchRef: (Msg -> unit) option = None
    let mutable failedImageUrls: Set<string> = Set.empty
    let mutable pendingImageFetches: Set<string> = Set.empty

    let init () =
        printfn "[TRACE] App.init() starting"
        let cache = CacheDb.open' None
        printfn "[TRACE] Cache opened"
        { Store = DomainStore.empty
          Cache = cache
          Navigation = Navigation.init ()
          Home = Home.init ()
          Notifications = Notifications.init ()
          Bookmarks = Bookmarks.init ()
          Search = Search.init ()
          Settings = Settings.init ()
          Stats = { EventsReceived = 0; EventsStored = 0; RelaysConnected = 0; ActiveSubscriptions = 0 }
          StatusMessage = "Welcome to Nostra"
          PendingMetadataSubscriptions = Map.empty },
        Cmd.none

    /// Get the latest event timestamp from the feed, or default to 2 days ago
    let private getLatestEventTimestamp (model: Model) =
        model.Home.Feed.Events
        |> List.tryHead
        |> Option.map _.CreatedAt
        |> Option.defaultValue (DateTime.UtcNow.AddDays(-2.0))

    /// Get the list of followed author IDs from the model
    let private getFollowedAuthors (model: Model) =
        match model.Store.CurrentUser with
        | Some user -> DomainStore.getFollowing user model.Store
        | None -> []

    let private handleNavigationExternal (extMsg: Navigation.ExternalMsg) (model: Model) =
        match extMsg with
        | Navigation.NoOp -> Cmd.none
        | Navigation.PageChanged _ -> Cmd.none
        | Navigation.AccountSwitched account ->
            // When account is switched, reconnect to relays and fetch new data
            Cmd.ofMsg (SetStatusMessage $"Switched to account: {account.Name}")

    let private handleHomeExternal (model: Model) (extMsg: Home.ExternalMsg) =
        match extMsg with
        | Home.NoOp -> Cmd.none
        | Home.SubscribeToAuthor author ->
            let since = getLatestEventTimestamp model
            nostrClients |> Map.iter (fun _ client -> subscribeToAuthor client author since)
            // Update the followed users list (using AddToFollowed to avoid infinite loop)
            // and Feed's profile cache
            Cmd.batch [
                Cmd.ofMsg (HomeMsg (Home.AddToFollowed author))
                Cmd.ofMsg (HomeMsg (Home.FeedMsg (Feed.AuthorFollowed author)))
                Cmd.ofMsg (SetStatusMessage $"Subscribed to {formatAuthorId author}")
            ]
        | Home.RequestProfile author ->
            nostrClients |> Map.tryPick (fun _ client -> Some client)
            |> Option.iter (fun client -> fetchProfile client author)
            Cmd.none
        | Home.RequestEvents eventIds ->
            nostrClients |> Map.tryPick (fun _ client -> Some client)
            |> Option.iter (fun client -> fetchEvents client eventIds)
            Cmd.none
        | Home.PublishNote _ ->
            // TODO: Implement note publishing
            Cmd.ofMsg (SetStatusMessage "Note publishing not yet implemented")
        | Home.RequestUrlPreview url ->
            match dispatchRef with
            | Some dispatch ->
                async {
                    let! result = fetchUrlPreview url
                    let preview =
                        match result with
                        | WebPreview data ->
                            Feed.PreviewLoaded {
                                Image = data.Image
                                Title = data.Title
                                Description = data.Description
                                SiteName = data.SiteName
                            }
                        | TweetPreview data ->
                            Feed.TweetPreviewLoaded {
                                AuthorName = data.AuthorName
                                AuthorHandle = data.AuthorHandle
                                Content = data.Content
                                ProfileImage = data.ProfileImage
                            }
                        | NoPreview ->
                            Feed.PreviewNotAvailable
                    dispatch (HomeMsg (Home.FeedMsg (Feed.UrlPreviewReceivedFull (url, preview))))
                } |> Async.Start
            | None -> ()
            Cmd.none

    let private handleSearchExternal (extMsg: Search.ExternalMsg) _model =
        match extMsg with
        | Search.NoOp -> Cmd.none
        | Search.SearchRequested author ->
            if nostrClients.IsEmpty then
                Cmd.batch [
                    Cmd.ofMsg (SearchMsg (Search.SearchFailed "Not connected to any relay"))
                    Cmd.ofMsg (SetStatusMessage "Not connected to any relay")
                ]
            else
                nostrClients |> Map.iter (fun _ client -> searchUser client author)
                Cmd.ofMsg (SetStatusMessage "Searching...")
        | Search.SearchNip05 identifier ->
            // Perform async NIP-05 lookup
            match dispatchRef with
            | Some dispatch ->
                async {
                    let! result = lookupNip05 identifier
                    match result with
                    | Ok author ->
                        // NIP-05 resolved, now search for the profile
                        if nostrClients.IsEmpty then
                            dispatch (SearchMsg (Search.SearchFailed "Not connected to any relay"))
                        else
                            nostrClients |> Map.iter (fun _ client -> searchUser client author)
                    | Error e ->
                        dispatch (SearchMsg (Search.SearchFailed e))
                } |> Async.Start
            | None -> ()
            Cmd.ofMsg (SetStatusMessage $"Looking up {identifier}...")
        | Search.FollowRequested author ->
            Cmd.batch [
                Cmd.ofMsg (HomeMsg (Home.Follow author))
                Cmd.ofMsg (SetStatusMessage $"Following {formatAuthorId author}")
            ]
        | Search.UnfollowRequested author ->
            Cmd.batch [
                Cmd.ofMsg (HomeMsg (Home.Unfollow author))
                Cmd.ofMsg (SetStatusMessage $"Unfollowed {formatAuthorId author}")
            ]

    let private handleSettingsExternal (model: Model) (extMsg: Settings.ExternalMsg) =
        match extMsg with
        | Settings.NoOp -> Cmd.none
        | Settings.ConnectToRelay url ->
            match dispatchRef with
            | Some dispatch ->
                let authors = getFollowedAuthors model
                let since = getLatestEventTimestamp model

                let onEvent event = dispatch (NostrEventReceived event)
                let onProfile (author, profile) = dispatch (NostrProfileReceived (author, profile))
                let onContacts (author, contacts) = dispatch (NostrContactsReceived (author, contacts))
                let onRelayList (author, relays) = dispatch (NostrRelayListReceived (author, relays))
                let onRelayStatusChange (relayUrl, status) = dispatch (RelayStatusChanged (relayUrl, status))
                let onMetadataEose subscriptionId = dispatch (MetadataEoseReceived subscriptionId)
                let onMessage message = dispatch (SetStatusMessage message)
                let onClientReady client =
                    nostrClients <- nostrClients |> Map.add url client
                    // Fetch account metadata when connected
                    match model.Navigation.CurrentAccount with
                    | Some account -> fetchAccountMetadata client account.PublicKey
                    | None -> ()
                    subscribeAuthors client authors since

                Async.Start(connectToRelay url onEvent onProfile onContacts onRelayList onRelayStatusChange onMetadataEose onMessage onClientReady)
            | None -> ()
            Cmd.ofMsg (SetStatusMessage $"Connecting to {url}...")
        | Settings.DisconnectFromRelay _ ->
            // TODO: Implement disconnect
            Cmd.none
        | Settings.FetchRelayInfo url ->
            match dispatchRef with
            | Some dispatch ->
                async {
                    let! result = fetchRelayInfo url
                    dispatch (SettingsMsg (Settings.RelaysMsg (Relays.RelayInfoLoaded (url, result))))
                } |> Async.Start
            | None -> ()
            Cmd.none

    let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
        match msg with
        | NavigationMsg subMsg ->
            let newNav, extMsg = Navigation.update subMsg model.Navigation
            let cmd = handleNavigationExternal extMsg model
            { model with Navigation = newNav }, cmd

        | HomeMsg subMsg ->
            let newHome, extMsg = Home.update subMsg model.Home
            let cmd = handleHomeExternal model extMsg
            { model with Home = newHome }, cmd

        | NotificationsMsg subMsg ->
            let newNotifications, _ = Notifications.update subMsg model.Notifications
            { model with Notifications = newNotifications }, Cmd.none

        | BookmarksMsg subMsg ->
            let newBookmarks, _ = Bookmarks.update subMsg model.Bookmarks
            { model with Bookmarks = newBookmarks }, Cmd.none

        | SearchMsg subMsg ->
            let newSearch, extMsg = Search.update subMsg model.Search
            let cmd = handleSearchExternal extMsg model
            { model with Search = newSearch }, cmd

        | SettingsMsg subMsg ->
            let newSettings, extMsg = Settings.update subMsg model.Settings
            let cmd = handleSettingsExternal model extMsg
            { model with Settings = newSettings }, cmd

        | StoreMsg storeMsg ->
            let newStore = StoreUpdate.update storeMsg model.Store
            { model with Store = newStore }, Cmd.none

        | SetStatusMessage message ->
            { model with StatusMessage = message }, Cmd.none

        | NostrEventReceived event ->
            // Cache the event
            CacheDb.saveEvent event model.Cache

            // Forward to Home's Feed
            let newHome, extMsg = Home.update (Home.FeedMsg (Feed.EventReceived event)) model.Home
            let cmd = handleHomeExternal model extMsg

            // Update stats
            let statsCmd = Cmd.ofMsg (UpdateStats (fun s ->
                { s with EventsReceived = s.EventsReceived + 1; EventsStored = s.EventsStored + 1 }))

            { model with Home = newHome }, Cmd.batch [cmd; statsCmd]

        | NostrProfileReceived (author, profile) ->
            let authorBytes = authorIdToBytes author
            let authorHex = AuthorId.toHex author
            printfn "[TRACE] NostrProfileReceived: %s (name=%s, picture=%s)"
                (authorHex.[..7]) profile.Name (if String.IsNullOrEmpty profile.Picture then "none" else "yes")

            let isFollowed = DomainStore.isFollowedByCurrentUser author model.Store

            // Check if we already have cached image data for this profile
            let cachedImageData =
                CacheDb.getProfile author model.Cache
                |> Option.bind (fun p -> p.PictureData)

            printfn "[TRACE] Cached image data for %s: %s"
                (authorHex.[..7]) (if cachedImageData.IsSome then $"{cachedImageData.Value.Length} bytes" else "none")

            // Create domain profile
            let domainProfile = Domain.Profile.fromNostrProfile author profile

            // Update DomainStore
            let newStore = DomainStore.setProfile domainProfile model.Store

            // Create UserProfile for legacy code (will be removed when fully migrated)
            let userProfile: UserProfile = {
                AuthorId = author
                Name = Some profile.Name
                DisplayName = Nostra.Profile.displayName profile
                About = Some profile.About
                Picture = Some profile.Picture
                PictureData = cachedImageData
                Nip05 = Nostra.Profile.nip05 profile
            }

            // Cache the profile
            CacheDb.saveProfile userProfile model.Cache

            // Update Home's Feed profile cache
            let newHome, _ = Home.update (Home.FeedMsg (Feed.ProfileUpdated (authorBytes, userProfile))) model.Home

            // Update Home's followed users if following
            let newHome2, _ = Home.update (Home.ProfileUpdated (authorBytes, userProfile)) newHome

            // Update Search if this is the searched profile
            let searchCmd =
                match model.Search.Result with
                | Searching -> Cmd.ofMsg (SearchMsg (Search.ProfileReceived (userProfile, isFollowed)))
                | Found (existing, _) when authorIdToBytes existing.AuthorId = authorBytes ->
                    Cmd.ofMsg (SearchMsg (Search.ProfileReceived (userProfile, isFollowed)))
                | _ -> Cmd.none

            // Fetch profile image if not already cached and picture URL exists
            let imageCmd =
                match cachedImageData, profile.Picture with
                | None, pictureUrl when not (String.IsNullOrEmpty pictureUrl) ->
                    if failedImageUrls.Contains pictureUrl then
                        printfn "[TRACE] Skipping image fetch for %s (previously failed)" (authorHex.[..7])
                        Cmd.none
                    elif pendingImageFetches.Contains pictureUrl then
                        printfn "[TRACE] Skipping image fetch for %s (already pending)" (authorHex.[..7])
                        Cmd.none
                    else
                        printfn "[TRACE] Fetching image for %s from %s" (authorHex.[..7]) (pictureUrl.[..min 50 (pictureUrl.Length - 1)])
                        match dispatchRef with
                        | Some dispatch ->
                            pendingImageFetches <- pendingImageFetches.Add pictureUrl
                            async {
                                let! imageData = fetchProfileImage pictureUrl
                                pendingImageFetches <- pendingImageFetches.Remove pictureUrl
                                match imageData with
                                | Some bytes ->
                                    printfn "[TRACE] Image fetched for %s: %d bytes" (authorHex.[..7]) bytes.Length
                                    dispatch (ProfileImageFetched (author, bytes))
                                | None ->
                                    printfn "[TRACE] Image fetch FAILED for %s" (authorHex.[..7])
                                    failedImageUrls <- failedImageUrls.Add pictureUrl
                            } |> Async.Start
                            Cmd.none
                        | None -> Cmd.none
                | Some _, _ ->
                    printfn "[TRACE] Skipping image fetch for %s (already cached)" (authorHex.[..7])
                    Cmd.none
                | _ -> Cmd.none

            { model with Store = newStore; Home = newHome2 }, Cmd.batch [searchCmd; imageCmd]

        | ProfileImageFetched (author, imageData) ->
            let authorBytes = authorIdToBytes author
            let authorHex = AuthorId.toHex author
            printfn "[TRACE] ProfileImageFetched: %s (%d bytes)" (authorHex.[..7]) imageData.Length

            // Update DomainStore with picture data
            let newStore = DomainStore.setProfilePictureData author imageData model.Store

            // Profile is guaranteed to be in cache (we saved it before fetching the image)
            match CacheDb.getProfile author model.Cache with
            | Some profile ->
                printfn "[TRACE] Saving image to cache for %s (current PictureData=%s)"
                    (authorHex.[..7])
                    (if profile.PictureData.IsSome then $"{profile.PictureData.Value.Length} bytes" else "None")
                let updatedProfile = { profile with PictureData = Some imageData }
                printfn "[TRACE] Updated profile PictureData=%s"
                    (if updatedProfile.PictureData.IsSome then $"{updatedProfile.PictureData.Value.Length} bytes" else "None")

                // Save updated profile with image data to cache
                CacheDb.saveProfile updatedProfile model.Cache

                // Verify save worked by reading back
                match CacheDb.getProfile author model.Cache with
                | Some verifyProfile ->
                    printfn "[TRACE] VERIFY after save: PictureData=%s"
                        (if verifyProfile.PictureData.IsSome then $"{verifyProfile.PictureData.Value.Length} bytes" else "None")
                | None ->
                    printfn "[TRACE] VERIFY ERROR: profile not found after save!"

                // Update Home's Feed profile cache
                let newHome, _ = Home.update (Home.FeedMsg (Feed.ProfileUpdated (authorBytes, updatedProfile))) model.Home

                // Update Home's followed users if following
                let newHome2, _ = Home.update (Home.ProfileUpdated (authorBytes, updatedProfile)) newHome

                printfn "[TRACE] Profile updated in UI for %s" (authorHex.[..7])
                { model with Store = newStore; Home = newHome2 }, Cmd.none
            | None ->
                printfn "[TRACE] ERROR: Profile not found in cache for %s" (authorHex.[..7])
                { model with Store = newStore }, Cmd.none

        | RelayStatusChanged (url, status) ->
            let relayCmd = Cmd.ofMsg (SettingsMsg (Settings.RelaysMsg (Relays.RelayStatusChanged (url, status))))
            // Calculate what the count will be AFTER the update is applied
            let connectedCount =
                model.Settings.Relays.Relays
                |> List.map (fun r -> if r.Url = url then { r with Status = status } else r)
                |> List.filter (fun r -> r.Status = RelayConnected)
                |> List.length
            let statsCmd = Cmd.ofMsg (UpdateStats (fun s -> { s with RelaysConnected = connectedCount }))
            model, Cmd.batch [relayCmd; statsCmd]

        | UpdateStats updateFn ->
            { model with Stats = updateFn model.Stats }, Cmd.none

        | NostrContactsReceived (author, contacts) ->
            // Check if this is for the current account
            match model.Navigation.CurrentAccount with
            | Some account when AuthorId.toBytes author = AuthorId.toBytes account.PublicKey ->
                // Save contacts to cache
                CacheDb.saveContacts author contacts model.Cache

                // Update DomainStore with contacts
                let newStore = DomainStore.setContacts author contacts model.Store

                // Update the account with contacts and add them to followed users
                let updatedAccount = { account with Following = contacts }
                let updatedAccounts =
                    model.Navigation.Accounts
                    |> List.map (fun a ->
                        if AuthorId.toBytes a.PublicKey = AuthorId.toBytes author then updatedAccount else a)
                let newNav =
                    { model.Navigation with
                        CurrentAccount = Some updatedAccount
                        Accounts = updatedAccounts }

                // Follow each contact in the UI
                let followCmds =
                    contacts
                    |> List.map (fun contact -> Cmd.ofMsg (HomeMsg (Home.AddToFollowed contact)))

                // === Metadata-first approach ===
                // 1. Load cached profiles and relay lists
                let cachedProfiles = CacheDb.getProfiles contacts model.Cache
                printfn "[TRACE] NostrContactsReceived: %d contacts, %d cached profiles" contacts.Length cachedProfiles.Length
                for p in cachedProfiles do
                    let hasImageData = p.PictureData.IsSome
                    let imageSize = p.PictureData |> Option.map (fun d -> d.Length) |> Option.defaultValue 0
                    printfn "[TRACE] Cached profile %s: PictureData=%b (%d bytes), Picture=%s"
                        (AuthorId.toHex p.AuthorId).[..7]
                        hasImageData
                        imageSize
                        (p.Picture |> Option.map (fun u -> u.[..min 30 (u.Length - 1)]) |> Option.defaultValue "none")
                let cachedProfileIds = cachedProfiles |> List.map (fun p -> AuthorId.toBytes p.AuthorId) |> Set.ofList

                // 2. Find which profiles are missing from cache
                let missingProfileAuthors =
                    contacts
                    |> List.filter (fun c -> not (cachedProfileIds.Contains (AuthorId.toBytes c)))

                // 3. Find which relay lists are missing (check cache for each contact)
                let authorsWithRelayList =
                    contacts
                    |> List.filter (fun c ->
                        let relays = CacheDb.getRelayList c model.Cache
                        not relays.IsEmpty)
                    |> List.map AuthorId.toBytes
                    |> Set.ofList

                let missingRelayListAuthors =
                    contacts
                    |> List.filter (fun c -> not (authorsWithRelayList.Contains (AuthorId.toBytes c)))

                // Authors needing any metadata from network
                let authorsNeedingMetadata =
                    (missingProfileAuthors @ missingRelayListAuthors)
                    |> List.distinctBy AuthorId.toBytes

                // 4. Dispatch cached profiles as if from network (update UI caches)
                let cachedProfileCmds =
                    cachedProfiles
                    |> List.collect (fun profile ->
                        let authorBytes = AuthorId.toBytes profile.AuthorId
                        [ Cmd.ofMsg (HomeMsg (Home.FeedMsg (Feed.ProfileUpdated (authorBytes, profile))))
                          Cmd.ofMsg (HomeMsg (Home.ProfileUpdated (authorBytes, profile))) ])

                // 4b. Fetch images for cached profiles that have Picture URL but no PictureData
                match dispatchRef with
                | Some dispatch ->
                    for profile in cachedProfiles do
                        match profile.PictureData, profile.Picture with
                        | None, Some pictureUrl when not (String.IsNullOrEmpty pictureUrl) ->
                            if not (failedImageUrls.Contains pictureUrl) && not (pendingImageFetches.Contains pictureUrl) then
                                let authorHex = AuthorId.toHex profile.AuthorId
                                printfn "[TRACE-CACHE] Fetching image for cached profile %s" authorHex.[..7]
                                pendingImageFetches <- pendingImageFetches.Add pictureUrl
                                async {
                                    let! imageData = fetchProfileImage pictureUrl
                                    pendingImageFetches <- pendingImageFetches.Remove pictureUrl
                                    match imageData with
                                    | Some bytes ->
                                        printfn "[TRACE-CACHE] Image fetched for %s: %d bytes" authorHex.[..7] bytes.Length
                                        dispatch (ProfileImageFetched (profile.AuthorId, bytes))
                                    | None ->
                                        printfn "[TRACE-CACHE] Image fetch FAILED for %s" authorHex.[..7]
                                        failedImageUrls <- failedImageUrls.Add pictureUrl
                                } |> Async.Start
                        | _ -> ()
                | None -> ()

                // 5. Load cached events for contacts and dispatch to feed
                let cachedEvents =
                    contacts
                    |> List.collect (fun c -> CacheDb.getEventsByAuthor c model.Cache)
                    |> List.sortByDescending (fun e -> e.CreatedAt)
                    |> List.truncate 200  // Limit to avoid overwhelming the UI

                let cachedEventCmds =
                    cachedEvents
                    |> List.map (fun event -> Cmd.ofMsg (HomeMsg (Home.FeedMsg (Feed.EventReceived event))))

                // 6. Subscribe for missing metadata only, track subscription for EOSE
                let mutable pendingSubscriptions = model.PendingMetadataSubscriptions
                if not authorsNeedingMetadata.IsEmpty then
                    nostrClients |> Map.iter (fun _ client ->
                        match subscribeMetadataOnly client authorsNeedingMetadata with
                        | Some subId -> pendingSubscriptions <- pendingSubscriptions |> Map.add subId contacts
                        | None -> ())

                // If no metadata needed, subscribe to events immediately
                let cachedEventsCount = cachedEvents.Length
                let eventsCmd =
                    if authorsNeedingMetadata.IsEmpty then
                        let since = getLatestEventTimestamp model
                        nostrClients |> Map.iter (fun _ client -> subscribeEventsOnly client contacts since)
                        Cmd.ofMsg (SetStatusMessage $"Loaded {contacts.Length} contacts, {cachedEventsCount} cached events. Subscribing to new events...")
                    else
                        Cmd.ofMsg (SetStatusMessage $"Loaded {contacts.Length} contacts, {cachedEventsCount} cached events. Fetching metadata for {authorsNeedingMetadata.Length} authors...")

                { model with
                    Store = newStore
                    Navigation = newNav
                    PendingMetadataSubscriptions = pendingSubscriptions },
                Cmd.batch (followCmds @ cachedProfileCmds @ cachedEventCmds @ [eventsCmd])
            | _ -> model, Cmd.none

        | NostrRelayListReceived (author, relays) ->
            // Cache the relay list for this author
            if not relays.IsEmpty then
                CacheDb.saveRelayList author relays model.Cache

            // Update DomainStore with relay list
            let newStore =
                if not relays.IsEmpty then
                    { model.Store with RelayLists = model.Store.RelayLists |> Map.add (AuthorId.toBytes author) relays }
                else
                    model.Store

            // Check if this is for the current account
            match model.Navigation.CurrentAccount with
            | Some account when AuthorId.toBytes author = AuthorId.toBytes account.PublicKey ->
                // Update the account with relay list
                let updatedAccount = { account with Relays = relays }
                let updatedAccounts =
                    model.Navigation.Accounts
                    |> List.map (fun a ->
                        if AuthorId.toBytes a.PublicKey = AuthorId.toBytes author then updatedAccount else a)
                let newNav =
                    { model.Navigation with
                        CurrentAccount = Some updatedAccount
                        Accounts = updatedAccounts }
                { model with Store = newStore; Navigation = newNav },
                Cmd.ofMsg (SetStatusMessage $"Loaded {relays.Length} relays from account")
            | _ -> { model with Store = newStore }, Cmd.none

        | MetadataEoseReceived subscriptionId ->
            // Metadata subscription completed - now subscribe to events for those authors
            match model.PendingMetadataSubscriptions |> Map.tryFind subscriptionId with
            | Some contacts ->
                let since = getLatestEventTimestamp model
                nostrClients |> Map.iter (fun _ client -> subscribeEventsOnly client contacts since)

                { model with
                    PendingMetadataSubscriptions = model.PendingMetadataSubscriptions |> Map.remove subscriptionId },
                Cmd.ofMsg (SetStatusMessage $"Metadata loaded, subscribing to events from {contacts.Length} authors...")
            | None ->
                // Unknown subscription, ignore
                model, Cmd.none

        | AccountMetadataLoaded (author, profile) ->
            // Update account with profile info
            match model.Navigation.CurrentAccount with
            | Some account when AuthorId.toBytes author = AuthorId.toBytes account.PublicKey ->
                let updatedAccount =
                    { account with
                        Name = profile.Name
                        Picture = Some profile.Picture }
                let updatedAccounts =
                    model.Navigation.Accounts
                    |> List.map (fun a ->
                        if AuthorId.toBytes a.PublicKey = AuthorId.toBytes author then updatedAccount else a)
                let newNav =
                    { model.Navigation with
                        CurrentAccount = Some updatedAccount
                        Accounts = updatedAccounts }
                { model with Navigation = newNav }, Cmd.none
            | _ -> model, Cmd.none

        | InitializeAccount ->
            // Hardcoded account for now
            let accountPubKey =
                match Shareable.decodeNpub "npub1nccwjspr3nv7h67xx2qhdh2dzzvpyy55gte2dsu8yl7xd7n74y9qydz7mj" with
                | Some pk -> pk
                | None -> failwith "Invalid npub"
            let defaultAccount =
                { Name = "Loading..."
                  PublicKey = accountPubKey
                  SecretKey = None
                  Picture = None
                  Relays = []
                  Following = [] }
            let newNav =
                { model.Navigation with
                    CurrentAccount = Some defaultAccount
                    Accounts = [defaultAccount] }
            // Set current user in DomainStore
            let newStore = { model.Store with CurrentUser = Some accountPubKey }
            { model with Store = newStore; Navigation = newNav }, Cmd.ofMsg ConnectToAllRelays

        | ConnectToAllRelays ->
            // Connect to all enabled relays
            let connectCmds =
                model.Settings.Relays.Relays
                |> List.filter (fun r -> r.Enabled && r.Status = RelayDisconnected)
                |> List.map (fun r -> Cmd.ofMsg (SettingsMsg (Settings.RelaysMsg (Relays.Connect r.Url))))
            model, Cmd.batch connectCmds

    let private contentView (model: Model) dispatch =
        match model.Navigation.CurrentPage with
        | HomePage ->
            Home.view model.Store model.Home (HomeMsg >> dispatch)

        | NotificationsPage ->
            Notifications.view model.Notifications (NotificationsMsg >> dispatch)

        | BookmarksPage ->
            Bookmarks.view model.Bookmarks (BookmarksMsg >> dispatch)

        | SearchPage ->
            let isConnected =
                model.Settings.Relays.Relays
                |> List.exists (fun r -> r.Status = RelayConnected)
            Search.view model.Search isConnected (SearchMsg >> dispatch)

        | SettingsPage ->
            Settings.view model.Settings (SettingsMsg >> dispatch)

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.background Colors.contentBackground
            DockPanel.children [
                // Left sidebar with navigation
                Navigation.view model.Navigation model.Stats (NavigationMsg >> dispatch)

                // Main content area
                Border.create [
                    Border.child (contentView model dispatch)
                ]
            ]
        ]

    let initializeNostrClient dispatch =
        dispatchRef <- Some dispatch

type MainWindow() as this =
    inherit Avalonia.FuncUI.Hosts.HostWindow()

    do
        base.Title <- "Nostra - Nostr Client"
        base.Width <- 1200.0
        base.Height <- 800.0

        let init () = App.init()

        let nostrSubscription _model =
            let sub dispatch =
                let wrappedDispatch msg =
                    Dispatcher.UIThread.Post(fun () -> dispatch msg)
                App.initializeNostrClient wrappedDispatch
                wrappedDispatch App.InitializeAccount
                { new System.IDisposable with member _.Dispose() = () }
            [ ["nostr"], sub ]

        Elmish.Program.mkProgram init App.update App.view
        |> Elmish.Program.withSubscription nostrSubscription
        |> Avalonia.FuncUI.Elmish.Program.withHost this
        |> Elmish.Program.run
