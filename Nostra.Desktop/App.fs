namespace Nostra.Desktop

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open Nostra

module UICmd =
    open global.Elmish

    /// Wraps dispatch to ensure it runs on the UI thread
    let private uiDispatch dispatch msg =
        if Dispatcher.UIThread.CheckAccess() then
            dispatch msg
        else
            Dispatcher.UIThread.Post(fun () -> dispatch msg)

    /// Cmd.OfAsync.either that dispatches on the UI thread
    let ofAsyncEither (task: 'a -> Async<'b>) (arg: 'a) (ofSuccess: 'b -> 'msg) (ofError: exn -> 'msg) : Cmd<'msg> =
        [ fun dispatch ->
            async {
                try
                    let! result = task arg
                    uiDispatch dispatch (ofSuccess result)
                with ex ->
                    uiDispatch dispatch (ofError ex)
            } |> Async.StartImmediate ]

    /// Cmd.OfAsync.perform that dispatches on the UI thread
    let ofAsyncPerform (task: 'a -> Async<'b>) (arg: 'a) (ofSuccess: 'b -> 'msg) : Cmd<'msg> =
        [ fun dispatch ->
            async {
                try
                    let! result = task arg
                    uiDispatch dispatch (ofSuccess result)
                with _ -> ()
            } |> Async.StartImmediate ]

module App =
    open NostrService
    open global.Elmish

    let mutable nostrClient: NostrClient option = None

    let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
        match msg with
        | UpdateSearchQuery query ->
            { model with SearchQuery = query }, Cmd.none

        | SearchUser ->
            match parseAuthorId model.SearchQuery with
            | Ok author ->
                let searchCmd =
                    match nostrClient with
                    | Some client ->
                        UICmd.ofAsyncPerform
                            (fun () -> searchUser client author)
                            ()
                            (fun () -> SetStatusMessage "Search request sent...")
                    | None ->
                        Cmd.ofMsg (SetStatusMessage "Not connected to relay")
                { model with
                    SearchResult = Searching
                    StatusMessage = "Searching..." }, searchCmd
            | Error e ->
                { model with
                    SearchResult = NotFound e
                    StatusMessage = e }, Cmd.none

        | SearchCompleted result ->
            match result with
            | Ok profile ->
                { model with SearchResult = Found profile }, Cmd.none
            | Error e ->
                { model with SearchResult = NotFound e }, Cmd.none

        | FollowUser author ->
            let bytes = authorIdToBytes author
            let existingProfile =
                model.FollowedUsers
                |> Map.tryFind bytes
                |> Option.defaultValue {
                    AuthorId = author
                    Name = None
                    DisplayName = None
                    About = None
                    Picture = None
                    Nip05 = None
                    IsFollowed = true
                }
            let updatedProfile = { existingProfile with IsFollowed = true }
            let updatedUsers = model.FollowedUsers |> Map.add bytes updatedProfile

            match nostrClient with
            | Some client -> subscribeToAuthor client author
            | None -> ()

            let updatedSearchResult =
                match model.SearchResult with
                | Found profile when authorIdToBytes profile.AuthorId = bytes ->
                    Found { profile with IsFollowed = true }
                | other -> other

            { model with
                FollowedUsers = updatedUsers
                SearchResult = updatedSearchResult
                StatusMessage = $"Following {formatAuthorId author}" }, Cmd.none

        | UnfollowUser author ->
            let bytes = authorIdToBytes author
            let updatedUsers =
                model.FollowedUsers
                |> Map.change bytes (Option.map (fun p -> { p with IsFollowed = false }))

            let updatedSearchResult =
                match model.SearchResult with
                | Found profile when authorIdToBytes profile.AuthorId = bytes ->
                    Found { profile with IsFollowed = false }
                | other -> other

            { model with
                FollowedUsers = updatedUsers
                SearchResult = updatedSearchResult
                StatusMessage = $"Unfollowed {formatAuthorId author}" }, Cmd.none

        | EventReceived event ->
            let authorBytes = authorIdToBytes event.Author
            let eventBytes = EventId.toBytes event.Id
            let cachedProfile =
                model.ProfileCache
                |> Map.tryFind authorBytes

            let authorName =
                cachedProfile
                |> Option.bind (fun p -> p.DisplayName |> Option.orElse p.Name)

            let eventWithAuthor = { event with AuthorName = authorName }
            let updatedFeed =
                eventWithAuthor :: model.Feed
                |> List.distinctBy (fun e -> EventId.toBytes e.Id)
                |> List.sortByDescending (fun e -> e.CreatedAt)
                |> List.truncate 200

            // Store event in cache
            let updatedEventCache = model.EventCache |> Map.add eventBytes eventWithAuthor

            // Build commands for missing data
            let profileCmd =
                match cachedProfile with
                | Some _ -> Cmd.none
                | None -> Cmd.ofMsg (RequestProfile event.Author)

            // Check if we need to fetch the referenced event
            let replyToCmd =
                match event.ReplyTo with
                | Some replyToId ->
                    let replyBytes = EventId.toBytes replyToId
                    let alreadyCached = model.EventCache |> Map.containsKey replyBytes
                    let alreadyPending = model.PendingEventRequests |> Set.contains replyBytes
                    if not alreadyCached && not alreadyPending then
                        Cmd.ofMsg (RequestEvents [replyToId])
                    else
                        Cmd.none
                | None -> Cmd.none

            { model with
                Feed = updatedFeed
                EventCache = updatedEventCache }, Cmd.batch [profileCmd; replyToCmd]

        | ProfileReceived (author, profile) ->
            let bytes = authorIdToBytes author
            let isFollowed =
                model.FollowedUsers
                |> Map.tryFind bytes
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

            // Always store in cache
            let updatedCache = model.ProfileCache |> Map.add bytes userProfile

            let updatedUsers =
                if isFollowed then
                    model.FollowedUsers |> Map.add bytes userProfile
                else
                    model.FollowedUsers

            let updatedSearchResult =
                match model.SearchResult with
                | Searching -> Found userProfile
                | Found existing when authorIdToBytes existing.AuthorId = bytes ->
                    Found userProfile
                | other -> other

            let updatedFeed =
                model.Feed
                |> List.map (fun e ->
                    if authorIdToBytes e.Author = bytes then
                        { e with AuthorName = userProfile.DisplayName |> Option.orElse userProfile.Name }
                    else e)

            { model with
                ProfileCache = updatedCache
                FollowedUsers = updatedUsers
                SearchResult = updatedSearchResult
                Feed = updatedFeed }, Cmd.none

        | UpdateRelayUrl url ->
            { model with RelayUrl = url }, Cmd.none

        | Connect ->
            let connectCmd =
                match nostrClient with
                | Some client ->
                    UICmd.ofAsyncEither
                        (fun () -> connectToRelay client model.RelayUrl)
                        ()
                        (fun () -> SetStatusMessage "Connection initiated...")
                        (fun ex -> SetStatusMessage $"Error: {ex.Message}")
                | None ->
                    Cmd.ofMsg (SetStatusMessage "Client not initialized")
            { model with
                ConnectionStatus = Connecting
                StatusMessage = "Connecting..." }, connectCmd

        | ConnectionStatusChanged status ->
            { model with ConnectionStatus = status }, Cmd.none

        | SetStatusMessage message ->
            { model with StatusMessage = message }, Cmd.none

        | ClearFeed ->
            { model with Feed = [] }, Cmd.none

        | RequestProfile author ->
            let authorBytes = authorIdToBytes author
            // Only request if not already in cache
            if model.ProfileCache |> Map.containsKey authorBytes then
                model, Cmd.none
            else
                match nostrClient with
                | Some client ->
                    fetchProfile client author
                | None -> ()
                model, Cmd.none

        | RequestEvents eventIds ->
            // Filter out events we already have or are pending
            let newEventIds =
                eventIds
                |> List.filter (fun eid ->
                    let bytes = EventId.toBytes eid
                    not (model.EventCache |> Map.containsKey bytes) &&
                    not (model.PendingEventRequests |> Set.contains bytes))

            if newEventIds.IsEmpty then
                model, Cmd.none
            else
                // Mark these as pending
                let updatedPending =
                    newEventIds
                    |> List.fold (fun acc eid -> Set.add (EventId.toBytes eid) acc) model.PendingEventRequests

                match nostrClient with
                | Some client ->
                    fetchEvents client newEventIds
                | None -> ()

                { model with PendingEventRequests = updatedPending }, Cmd.none

    let connectionStatusView (model: Model) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Top
            DockPanel.margin (Thickness 10.0)
            DockPanel.children [
                TextBox.create [
                    TextBox.dock Dock.Left
                    TextBox.width 300.0
                    TextBox.text model.RelayUrl
                    TextBox.onTextChanged (UpdateRelayUrl >> dispatch)
                ]
                Button.create [
                    Button.dock Dock.Left
                    Button.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                    Button.content (
                        match model.ConnectionStatus with
                        | Disconnected -> "Connect"
                        | Connecting -> "Connecting..."
                        | Connected -> "Connected"
                    )
                    Button.isEnabled (model.ConnectionStatus = Disconnected)
                    Button.onClick (fun _ -> dispatch Connect)
                ]
                TextBlock.create [
                    TextBlock.dock Dock.Right
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                    TextBlock.foreground (
                        match model.ConnectionStatus with
                        | Connected -> Brushes.Green
                        | Connecting -> Brushes.Orange
                        | Disconnected -> Brushes.Gray
                    )
                    TextBlock.text (
                        match model.ConnectionStatus with
                        | Connected -> "● Connected"
                        | Connecting -> "● Connecting"
                        | Disconnected -> "○ Disconnected"
                    )
                ]
            ]
        ]

    let searchView (model: Model) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Top
            DockPanel.margin (Thickness 10.0)
            DockPanel.children [
                TextBox.create [
                    TextBox.dock Dock.Left
                    TextBox.width 450.0
                    TextBox.text model.SearchQuery
                    TextBox.onTextChanged (UpdateSearchQuery >> dispatch)
                    TextBox.onKeyDown (fun e ->
                        if e.Key = Avalonia.Input.Key.Enter then
                            dispatch SearchUser
                    )
                ]
                Button.create [
                    Button.dock Dock.Left
                    Button.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                    Button.content "Search"
                    Button.isEnabled (model.ConnectionStatus = Connected && model.SearchQuery.Length > 0)
                    Button.onClick (fun _ -> dispatch SearchUser)
                ]
            ]
        ]

    let searchResultView (model: Model) dispatch =
        Border.create [
            Border.margin (Thickness 10.0)
            Border.child (
                match model.SearchResult with
                | NotSearched ->
                    TextBlock.create [
                        TextBlock.text "Enter an npub or hex public key to search for a user"
                        TextBlock.foreground Brushes.Gray
                    ] :> IView
                | Searching ->
                    TextBlock.create [
                        TextBlock.text "Searching..."
                        TextBlock.foreground Brushes.Orange
                    ] :> IView
                | NotFound error ->
                    TextBlock.create [
                        TextBlock.text $"Not found: {error}"
                        TextBlock.foreground Brushes.Red
                    ] :> IView
                | Found profile ->
                    Border.create [
                        Border.padding (Thickness 15.0)
                        Border.cornerRadius (CornerRadius 8.0)
                        Border.background (SolidColorBrush(Color.FromArgb(30uy, 100uy, 100uy, 100uy)))
                        Border.child (
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 8.0
                                StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.fontSize 18.0
                                        TextBlock.fontWeight FontWeight.Bold
                                        TextBlock.text (
                                            profile.DisplayName
                                            |> Option.orElse profile.Name
                                            |> Option.defaultValue "Unknown"
                                        )
                                    ]
                                    TextBlock.create [
                                        TextBlock.fontSize 12.0
                                        TextBlock.foreground Brushes.Gray
                                        TextBlock.text (formatAuthorId profile.AuthorId)
                                        TextBlock.textWrapping TextWrapping.Wrap
                                    ]
                                    if profile.Nip05.IsSome then
                                        TextBlock.create [
                                            TextBlock.foreground Brushes.Purple
                                            TextBlock.text $"NIP-05: {profile.Nip05.Value}"
                                        ]
                                    if profile.About.IsSome && not (String.IsNullOrWhiteSpace profile.About.Value) then
                                        TextBlock.create [
                                            TextBlock.text profile.About.Value
                                            TextBlock.textWrapping TextWrapping.Wrap
                                            TextBlock.maxWidth 500.0
                                        ]
                                    Button.create [
                                        Button.margin (Thickness(0.0, 10.0, 0.0, 0.0))
                                        Button.content (if profile.IsFollowed then "Unfollow" else "Follow")
                                        Button.background (if profile.IsFollowed then Brushes.Gray else Brushes.Blue)
                                        Button.foreground Brushes.White
                                        Button.onClick (fun _ ->
                                            if profile.IsFollowed then
                                                dispatch (UnfollowUser profile.AuthorId)
                                            else
                                                dispatch (FollowUser profile.AuthorId)
                                        )
                                    ]
                                ]
                            ]
                        )
                    ] :> IView
            )
        ]

    let feedEventView (model: Model) (depth: int) (event: FeedEvent) =
        let isReply = depth > 0
        let leftMargin = float depth * 25.0
        let bgAlpha = if isReply then 35uy else 20uy
        let bgBlue = if isReply then 140uy else 100uy

        // Get the parent event info if this is a reply
        let replyToInfo =
            event.ReplyTo
            |> Option.bind (fun replyToId ->
                let bytes = EventId.toBytes replyToId
                model.EventCache |> Map.tryFind bytes)

        // Get profile picture from cache
        let authorBytes = authorIdToBytes event.Author
        let profilePicture =
            model.ProfileCache
            |> Map.tryFind authorBytes
            |> Option.bind (fun p -> p.Picture)

        // Generate a color based on author's pubkey for avatar background
        let avatarColor =
            if authorBytes.Length >= 3 then
                Color.FromRgb(authorBytes.[0], authorBytes.[1], authorBytes.[2])
            else
                Color.FromRgb(100uy, 100uy, 200uy)

        Border.create [
            Border.margin (Thickness(leftMargin, 5.0, 0.0, 5.0))
            Border.padding (Thickness 10.0)
            Border.cornerRadius (CornerRadius 5.0)
            Border.background (SolidColorBrush(Color.FromArgb(bgAlpha, 80uy, 80uy, bgBlue)))
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        // Profile image on the left
                        Border.create [
                            Border.dock Dock.Left
                            Border.width 40.0
                            Border.height 40.0
                            Border.cornerRadius (CornerRadius 20.0)
                            Border.background (SolidColorBrush avatarColor)
                            Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
                            Border.verticalAlignment VerticalAlignment.Top
                            Border.child (
                                TextBlock.create [
                                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.foreground Brushes.White
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.fontSize 16.0
                                    TextBlock.text (
                                        event.AuthorName
                                        |> Option.map (fun n -> if n.Length > 0 then n.[0..0].ToUpper() else "?")
                                        |> Option.defaultValue "?"
                                    )
                                ]
                            )
                        ]
                        // Content area
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 5.0
                            StackPanel.children [
                                // Show "replying to" indicator if this is a reply
                                if isReply then
                                    StackPanel.create [
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 5.0
                                        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 5.0))
                                        StackPanel.children [
                                            TextBlock.create [
                                                TextBlock.fontSize 11.0
                                                TextBlock.foreground Brushes.Gray
                                                TextBlock.text "↳ replying to"
                                            ]
                                            TextBlock.create [
                                                TextBlock.fontSize 11.0
                                                TextBlock.foreground Brushes.CornflowerBlue
                                                TextBlock.text (
                                                    match replyToInfo with
                                                    | Some parentEvent ->
                                                        parentEvent.AuthorName
                                                        |> Option.defaultValue (
                                                            let npub = formatAuthorId parentEvent.Author
                                                            npub[..12] + "...")
                                                    | None -> "..."
                                                )
                                            ]
                                        ]
                                    ]
                                DockPanel.create [
                                    DockPanel.children [
                                        TextBlock.create [
                                            TextBlock.dock Dock.Left
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.text (
                                                event.AuthorName
                                                |> Option.defaultValue (
                                                    let npub = formatAuthorId event.Author
                                                    npub[..15] + "..."
                                                )
                                            )
                                        ]
                                        TextBlock.create [
                                            TextBlock.dock Dock.Right
                                            TextBlock.foreground Brushes.Gray
                                            TextBlock.fontSize 11.0
                                            TextBlock.text (event.CreatedAt.ToString("MMM dd, HH:mm"))
                                        ]
                                    ]
                                ]
                                TextBlock.create [
                                    TextBlock.text event.Content
                                    TextBlock.textWrapping TextWrapping.Wrap
                                ]
                            ]
                        ]
                    ]
                ]
            )
        ]

    let buildThreadedFeed (events: FeedEvent list) =
        // Build a map of parentId -> children
        let childrenMap =
            events
            |> List.choose (fun e ->
                e.ReplyTo |> Option.map (fun parentId -> EventId.toBytes parentId, e))
            |> List.groupBy fst
            |> List.map (fun (parentId, children) -> parentId, children |> List.map snd)
            |> Map.ofList

        // Set of all event IDs in our feed
        let eventIds = events |> List.map (fun e -> EventId.toBytes e.Id) |> Set.ofList

        // Root events: no ReplyTo, or ReplyTo not in our feed
        let rootEvents =
            events
            |> List.filter (fun e ->
                match e.ReplyTo with
                | None -> true
                | Some parentId -> not (Set.contains (EventId.toBytes parentId) eventIds))
            |> List.sortByDescending (fun e -> e.CreatedAt)

        // Recursively flatten tree into (depth, event) pairs
        let rec flatten depth (event: FeedEvent) : (int * FeedEvent) list =
            let eventBytes = EventId.toBytes event.Id
            let children =
                childrenMap
                |> Map.tryFind eventBytes
                |> Option.defaultValue []
                |> List.sortBy (fun e -> e.CreatedAt)  // Keep replies in chronological order
            (depth, event) :: (children |> List.collect (flatten (depth + 1)))

        rootEvents |> List.collect (flatten 0)

    let feedView (model: Model) dispatch =
        let threadedEvents = buildThreadedFeed model.Feed

        DockPanel.create [
            DockPanel.margin (Thickness 10.0)
            DockPanel.children [
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.margin (Thickness(0.0, 0.0, 0.0, 10.0))
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Left
                            TextBlock.fontSize 16.0
                            TextBlock.fontWeight FontWeight.Bold
                            TextBlock.text $"Feed ({model.Feed.Length} events)"
                        ]
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "Clear"
                            Button.onClick (fun _ -> dispatch ClearFeed)
                        ]
                    ]
                ]
                ScrollViewer.create [
                    ScrollViewer.verticalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                for (depth, event) in threadedEvents do
                                    feedEventView model depth event
                            ]
                        ]
                    )
                ]
            ]
        ]

    let followedUsersView (model: Model) dispatch =
        let followedUsers =
            model.FollowedUsers
            |> Map.toList
            |> List.filter (fun (_, p) -> p.IsFollowed)
            |> List.map snd

        StackPanel.create [
            StackPanel.margin (Thickness 10.0)
            StackPanel.orientation Orientation.Vertical
            StackPanel.width 200.0
            StackPanel.children [
                TextBlock.create [
                    TextBlock.fontSize 14.0
                    TextBlock.fontWeight FontWeight.Bold
                    TextBlock.margin (Thickness(0.0, 0.0, 0.0, 10.0))
                    TextBlock.text $"Following ({followedUsers.Length})"
                ]
                ScrollViewer.create [
                    ScrollViewer.verticalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 5.0
                            StackPanel.children [
                                for user in followedUsers do
                                    Border.create [
                                        Border.padding (Thickness 5.0)
                                        Border.cornerRadius (CornerRadius 3.0)
                                        Border.background (SolidColorBrush(Color.FromArgb(20uy, 100uy, 100uy, 100uy)))
                                        Border.child (
                                            DockPanel.create [
                                                DockPanel.children [
                                                    TextBlock.create [
                                                        TextBlock.dock Dock.Left
                                                        TextBlock.text (
                                                            user.DisplayName
                                                            |> Option.orElse user.Name
                                                            |> Option.defaultValue (
                                                                let npub = formatAuthorId user.AuthorId
                                                                npub[..10] + "..."
                                                            )
                                                        )
                                                        TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                                    ]
                                                    Button.create [
                                                        Button.dock Dock.Right
                                                        Button.content "x"
                                                        Button.padding (Thickness(5.0, 0.0))
                                                        Button.onClick (fun _ -> dispatch (UnfollowUser user.AuthorId))
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                            ]
                        ]
                    )
                ]
            ]
        ]

    let statusBarView (model: Model) =
        Border.create [
            Border.dock Dock.Bottom
            Border.padding (Thickness 10.0)
            Border.background (SolidColorBrush(Color.FromArgb(30uy, 0uy, 0uy, 0uy)))
            Border.child (
                TextBlock.create [
                    TextBlock.fontSize 12.0
                    TextBlock.foreground Brushes.Gray
                    TextBlock.text model.StatusMessage
                ]
            )
        ]

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.children [
                statusBarView model
                connectionStatusView model dispatch
                searchView model dispatch
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.children [
                        searchResultView model dispatch
                    ]
                ]
                DockPanel.create [
                    DockPanel.children [
                        followedUsersView model dispatch
                        feedView model dispatch
                    ]
                ]
            ]
        ]

    let initializeNostrClient dispatch =
        let onEvent event =
            dispatch (EventReceived event)

        let onProfile (author, profile) =
            dispatch (ProfileReceived (author, profile))

        let onStatusChange status =
            dispatch (ConnectionStatusChanged status)

        let onMessage message =
            dispatch (SetStatusMessage message)

        nostrClient <- Some (createClient onEvent onProfile onStatusChange onMessage)

type MainWindow() as this =
    inherit Avalonia.FuncUI.Hosts.HostWindow()

    do
        base.Title <- "Nostra - Nostr Client"
        base.Width <- 1000.0
        base.Height <- 700.0

        let init () =
            let model = Model.init()
            model, global.Elmish.Cmd.none

        let nostrSubscription _model =
            let sub dispatch =
                let wrappedDispatch msg =
                    Dispatcher.UIThread.Post(fun () -> dispatch msg)
                App.initializeNostrClient wrappedDispatch
                { new System.IDisposable with member _.Dispose() = () }
            [ ["nostr"], sub ]

        global.Elmish.Program.mkProgram init App.update App.view
        |> global.Elmish.Program.withSubscription nostrSubscription
        |> Avalonia.FuncUI.Elmish.Program.withHost this
        |> global.Elmish.Program.run