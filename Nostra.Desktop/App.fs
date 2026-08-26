namespace Nostra.Desktop

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Threading
open Nostra
open Styles

module App =
    open NostrService
    open global.Elmish

    let mutable nostrClients: Map<string, NostrClient> = Map.empty

    // Composed model from all feature modules
    type Model = {
        Navigation: Navigation.Model
        Home: Home.Model
        Notifications: Notifications.Model
        Bookmarks: Bookmarks.Model
        Search: Search.Model
        Settings: Settings.Model
        Stats: ConnectionStats
        StatusMessage: string
    }

    // Wrap child messages
    type Msg =
        | NavigationMsg of Navigation.Msg
        | HomeMsg of Home.Msg
        | NotificationsMsg of Notifications.Msg
        | BookmarksMsg of Bookmarks.Msg
        | SearchMsg of Search.Msg
        | SettingsMsg of Settings.Msg
        | SetStatusMessage of string
        // Events from NostrService
        | NostrEventReceived of FeedEvent
        | NostrProfileReceived of AuthorId * Profile
        | RelayStatusChanged of string * RelayStatus
        | UpdateStats of (ConnectionStats -> ConnectionStats)

    let mutable dispatchRef: (Msg -> unit) option = None

    let init () =
        { Navigation = Navigation.init ()
          Home = Home.init ()
          Notifications = Notifications.init ()
          Bookmarks = Bookmarks.init ()
          Search = Search.init ()
          Settings = Settings.init ()
          Stats = { EventsReceived = 0; EventsStored = 0; RelaysConnected = 0; ActiveSubscriptions = 0 }
          StatusMessage = "Welcome to Nostra" },
        Cmd.none

    /// Get the latest event timestamp from the feed, or default to 7 days ago
    let private getLatestEventTimestamp (model: Model) =
        model.Home.Feed.Events
        |> List.tryHead
        |> Option.map (fun e -> e.CreatedAt)
        |> Option.defaultValue (DateTime.UtcNow.AddDays(-7.0))

    /// Get the list of followed author IDs from the model
    let private getFollowedAuthors (model: Model) =
        model.Home.FollowedUsers
        |> Map.toList
        |> List.filter (fun (_, p) -> p.IsFollowed)
        |> List.map (fun (_, p) -> p.AuthorId)

    let private handleNavigationExternal (extMsg: Navigation.ExternalMsg) =
        match extMsg with
        | Navigation.NoOp -> Cmd.none
        | Navigation.PageChanged _ -> Cmd.none

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
                let onRelayStatusChange (relayUrl, status) = dispatch (RelayStatusChanged (relayUrl, status))
                let onMessage message =
                    dispatch (SetStatusMessage message)
                    // Track errors from relays
                    if message.Contains("Error:") || message.Contains("Notice:") then
                        dispatch (SettingsMsg (Settings.RelaysMsg (Relays.AddRelayError (url, message))))
                let onClientReady client = nostrClients <- nostrClients |> Map.add url client

                Async.Start(connectToRelay url authors since onEvent onProfile onRelayStatusChange onMessage onClientReady)
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
            let cmd = handleNavigationExternal extMsg
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

        | SetStatusMessage message ->
            { model with StatusMessage = message }, Cmd.none

        | NostrEventReceived event ->
            // Forward to Home's Feed
            let newHome, extMsg = Home.update (Home.FeedMsg (Feed.EventReceived event)) model.Home
            let cmd = handleHomeExternal model extMsg

            // Update stats
            let statsCmd = Cmd.ofMsg (UpdateStats (fun s ->
                { s with EventsReceived = s.EventsReceived + 1; EventsStored = s.EventsStored + 1 }))

            { model with Home = newHome }, Cmd.batch [cmd; statsCmd]

        | NostrProfileReceived (author, profile) ->
            let authorBytes = authorIdToBytes author
            let isFollowed =
                model.Home.FollowedUsers
                |> Map.tryFind authorBytes
                |> Option.map (fun p -> p.IsFollowed)
                |> Option.defaultValue false

            let userProfile = {
                AuthorId = author
                Name = Some profile.Name
                DisplayName = Profile.displayName profile
                About = Some profile.About
                Picture = Some profile.Picture
                Nip05 = Profile.nip05 profile
                IsFollowed = isFollowed
            }

            // Update Home's Feed profile cache
            let newHome, _ = Home.update (Home.FeedMsg (Feed.ProfileUpdated (authorBytes, userProfile))) model.Home

            // Update Home's followed users if following
            let newHome2, _ = Home.update (Home.ProfileUpdated (authorBytes, userProfile)) newHome

            // Update Search if this is the searched profile
            let searchCmd =
                match model.Search.Result with
                | Searching -> Cmd.ofMsg (SearchMsg (Search.ProfileReceived userProfile))
                | Found existing when authorIdToBytes existing.AuthorId = authorBytes ->
                    Cmd.ofMsg (SearchMsg (Search.ProfileReceived userProfile))
                | _ -> Cmd.none

            { model with Home = newHome2 }, searchCmd

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

    let private contentView (model: Model) dispatch =
        match model.Navigation.CurrentPage with
        | HomePage ->
            Home.view model.Home (HomeMsg >> dispatch)

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
                { new System.IDisposable with member _.Dispose() = () }
            [ ["nostr"], sub ]

        Elmish.Program.mkProgram init App.update App.view
        |> Elmish.Program.withSubscription nostrSubscription
        |> Avalonia.FuncUI.Elmish.Program.withHost this
        |> Elmish.Program.run
