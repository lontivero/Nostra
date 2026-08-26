namespace Nostra.Desktop

open System.Text.RegularExpressions
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open AsyncImageLoader
open Nostra
open Styles

module Feed =

    // Image URL pattern for common image formats
    let private imageUrlPattern = Regex(@"https?://[^\s]+\.(?:jpg|jpeg|png|gif|webp)(?:\?[^\s]*)?", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    type ContentPart =
        | TextPart of string
        | ImagePart of string

    let private parseContent (content: string) : ContentPart list =
        let matches = imageUrlPattern.Matches(content)
        if matches.Count = 0 then
            [ TextPart content ]
        else
            let parts = ResizeArray<ContentPart>()
            let mutable lastIndex = 0

            for m in matches do
                // Add text before this match
                if m.Index > lastIndex then
                    let text = content.Substring(lastIndex, m.Index - lastIndex).Trim()
                    if text.Length > 0 then
                        parts.Add(TextPart text)

                // Add the image URL
                parts.Add(ImagePart m.Value)
                lastIndex <- m.Index + m.Length

            // Add remaining text after last match
            if lastIndex < content.Length then
                let text = content.Substring(lastIndex).Trim()
                if text.Length > 0 then
                    parts.Add(TextPart text)

            parts |> Seq.toList

    let private renderContentPart (part: ContentPart) : Types.IView =
        match part with
        | TextPart text ->
            TextBlock.create (Attrs.wrappedText @ [
                TextBlock.text text
            ])
        | ImagePart url ->
            Border.create [
                Border.margin (Thickness(0.0, 8.0))
                Border.cornerRadius (CornerRadius 8.0)
                Border.clipToBounds true
                Border.child (
                    Image.create [
                        Image.maxWidth 400.0
                        Image.maxHeight 300.0
                        Image.stretch Stretch.Uniform
                        Image.init (fun img -> ImageLoader.SetSource(img, url))
                    ]
                )
            ]

    let private renderContent (content: string) : Types.IView =
        let parts = parseContent content
        if parts.Length = 1 then
            match parts[0] with
            | TextPart text ->
                TextBlock.create (Attrs.wrappedText @ [
                    TextBlock.text text
                ])
            | ImagePart url ->
                renderContentPart (ImagePart url)
        else
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 4.0
                StackPanel.children [
                    for part in parts do
                        renderContentPart part
                ]
            ]

    // Dialog state for showing event or author info
    type DialogState =
        | NoDialog
        | ShowingEventInfo of FeedEvent
        | ShowingAuthorInfo of AuthorId * UserProfile option

    type Model = {
        Events: FeedEvent list
        EventCache: Map<byte[], FeedEvent>
        ProfileCache: Map<byte[], UserProfile>
        PendingEventRequests: Set<byte[]>
        CurrentDialog: DialogState
    }

    type Msg =
        | EventReceived of FeedEvent
        | ProfileUpdated of byte[] * UserProfile
        | EventsCached of FeedEvent list
        | Clear
        // Context menu actions
        | ViewThread of FeedEvent
        | ViewAuthor of FeedEvent
        | ViewEventInfo of FeedEvent
        | SubscribeToAuthor of AuthorId
        | CloseDialog
        // Profile follow status update
        | AuthorFollowed of AuthorId

    type ExternalMsg =
        | NoOp
        | RequestProfile of AuthorId
        | RequestEvents of EventId list
        | SubscribeAuthor of AuthorId

    let init () = {
        Events = []
        EventCache = Map.empty
        ProfileCache = Map.empty
        PendingEventRequests = Set.empty
        CurrentDialog = NoDialog
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | EventReceived event ->
            let authorBytes = NostrService.authorIdToBytes event.Author
            let eventBytes = EventId.toBytes event.Id

            let authorName =
                model.ProfileCache
                |> Map.tryFind authorBytes
                |> Option.bind (fun p -> p.DisplayName |> Option.orElse p.Name)

            // Check if we already have this event and merge relay sources
            let existingEvent = model.EventCache |> Map.tryFind eventBytes
            let mergedRelays =
                match existingEvent with
                | Some existing ->
                    (existing.Relays @ event.Relays) |> List.distinct
                | None ->
                    event.Relays

            let eventWithAuthor = { event with AuthorName = authorName; Relays = mergedRelays }

            // Update events list, merging relays for duplicates
            let updatedEvents =
                let withoutExisting = model.Events |> List.filter (fun e -> EventId.toBytes e.Id <> eventBytes)
                eventWithAuthor :: withoutExisting
                |> List.sortByDescending (fun e -> e.CreatedAt)

            let updatedEventCache = model.EventCache |> Map.add eventBytes eventWithAuthor

            let needsProfile =
                model.ProfileCache |> Map.containsKey authorBytes |> not

            let needsReplyEvent =
                match event.ReplyTo with
                | Some replyToId ->
                    let replyBytes = EventId.toBytes replyToId
                    not (model.EventCache |> Map.containsKey replyBytes) &&
                    not (model.PendingEventRequests |> Set.contains replyBytes)
                | None -> false

            let externalMsg =
                match needsProfile, needsReplyEvent, event.ReplyTo with
                | true, true, Some replyToId ->
                    RequestProfile event.Author // Prioritize profile, events will come
                | true, _, _ ->
                    RequestProfile event.Author
                | false, true, Some replyToId ->
                    RequestEvents [replyToId]
                | _ ->
                    NoOp

            let updatedPending =
                match event.ReplyTo with
                | Some replyToId when needsReplyEvent ->
                    model.PendingEventRequests |> Set.add (EventId.toBytes replyToId)
                | _ ->
                    model.PendingEventRequests

            { model with
                Events = updatedEvents
                EventCache = updatedEventCache
                PendingEventRequests = updatedPending }, externalMsg

        | ProfileUpdated (authorBytes, profile) ->
            let updatedCache = model.ProfileCache |> Map.add authorBytes profile
            let updatedEvents =
                model.Events
                |> List.map (fun e ->
                    if NostrService.authorIdToBytes e.Author = authorBytes then
                        { e with AuthorName = profile.DisplayName |> Option.orElse profile.Name }
                    else e)
            { model with
                ProfileCache = updatedCache
                Events = updatedEvents }, NoOp

        | EventsCached events ->
            let updatedCache =
                events
                |> List.fold (fun cache e ->
                    Map.add (EventId.toBytes e.Id) e cache
                ) model.EventCache
            { model with EventCache = updatedCache }, NoOp

        | Clear ->
            { model with Events = [] }, NoOp

        | ViewThread _event ->
            // TODO: Implement thread view navigation
            model, NoOp

        | ViewAuthor event ->
            let authorBytes = NostrService.authorIdToBytes event.Author
            let profile = model.ProfileCache |> Map.tryFind authorBytes
            { model with CurrentDialog = ShowingAuthorInfo (event.Author, profile) }, NoOp

        | ViewEventInfo event ->
            { model with CurrentDialog = ShowingEventInfo event }, NoOp

        | SubscribeToAuthor author ->
            { model with CurrentDialog = NoDialog }, SubscribeAuthor author

        | CloseDialog ->
            { model with CurrentDialog = NoDialog }, NoOp

        | AuthorFollowed author ->
            let authorBytes = NostrService.authorIdToBytes author
            // Update ProfileCache to set IsFollowed = true
            let updatedProfileCache =
                model.ProfileCache
                |> Map.change authorBytes (Option.map (fun p -> { p with IsFollowed = true }))
            // Also update the dialog if it's showing this author
            let updatedDialog =
                match model.CurrentDialog with
                | ShowingAuthorInfo (dialogAuthor, Some profile) when NostrService.authorIdToBytes dialogAuthor = authorBytes ->
                    ShowingAuthorInfo (dialogAuthor, Some { profile with IsFollowed = true })
                | other -> other
            { model with ProfileCache = updatedProfileCache; CurrentDialog = updatedDialog }, NoOp

    // Context menu for each event
    let private eventContextMenu (event: FeedEvent) dispatch =
        ContextMenu.create [
            ContextMenu.viewItems [
                MenuItem.create [
                    MenuItem.header "View Thread"
                    MenuItem.onClick (fun _ -> dispatch (ViewThread event))
                ]
                MenuItem.create [
                    MenuItem.header "View Author"
                    MenuItem.onClick (fun _ -> dispatch (ViewAuthor event))
                ]
                MenuItem.create [
                    MenuItem.header "Info"
                    MenuItem.onClick (fun _ -> dispatch (ViewEventInfo event))
                ]
            ]
        ]

    // Dialog for showing event info
    let private eventInfoDialog (event: FeedEvent) dispatch =
        let noteId = Shareable.encodeNote event.Id
        let neventId = Shareable.encodeNevent (event.Id, event.Relays, Some event.Author, Some event.Kind)
        let npub = NostrService.formatAuthorId event.Author
        let kindValue = int event.Kind
        let kindName = event.Kind.ToString()

        Border.create [
            Border.background (SolidColorBrush(Color.FromArgb(220uy, 30uy, 30uy, 35uy)))
            Border.padding (Thickness 20.0)
            Border.cornerRadius (CornerRadius 10.0)
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 15.0
                    StackPanel.width 500.0
                    StackPanel.children [
                        // Header
                        DockPanel.create [
                            DockPanel.children [
                                TextBlock.create (Attrs.subheading @ [
                                    TextBlock.dock Dock.Left
                                    TextBlock.text "Event Info"
                                ])
                                Button.create [
                                    Button.dock Dock.Right
                                    Button.content "X"
                                    Button.padding (Thickness(8.0, 4.0))
                                    Button.onClick (fun _ -> dispatch CloseDialog)
                                ]
                            ]
                        ]

                        // Kind
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "Kind"
                                ])
                                TextBlock.create [
                                    TextBlock.text $"{kindName} ({kindValue})"
                                    TextBlock.textWrapping TextWrapping.Wrap
                                ]
                            ]
                        ]

                        // Event ID (note1...)
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "Event ID (note)"
                                ])
                                SelectableTextBlock.create [
                                    SelectableTextBlock.text noteId
                                    SelectableTextBlock.textWrapping TextWrapping.Wrap
                                    SelectableTextBlock.fontSize FontSizes.small
                                    SelectableTextBlock.fontFamily (FontFamily("Consolas, monospace"))
                                ]
                            ]
                        ]

                        // NIP-19 Pointer (nevent1...)
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "NIP-19 Pointer (nevent)"
                                ])
                                SelectableTextBlock.create [
                                    SelectableTextBlock.text neventId
                                    SelectableTextBlock.textWrapping TextWrapping.Wrap
                                    SelectableTextBlock.fontSize FontSizes.small
                                    SelectableTextBlock.fontFamily (FontFamily("Consolas, monospace"))
                                ]
                            ]
                        ]

                        // Author npub with button
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "Author"
                                ])
                                DockPanel.create [
                                    DockPanel.children [
                                        Button.create [
                                            Button.dock Dock.Right
                                            Button.content "View"
                                            Button.padding (Thickness(8.0, 4.0))
                                            Button.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                                            Button.onClick (fun _ -> dispatch (ViewAuthor event))
                                        ]
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text npub
                                            SelectableTextBlock.textWrapping TextWrapping.Wrap
                                            SelectableTextBlock.fontSize FontSizes.small
                                            SelectableTextBlock.fontFamily (FontFamily("Consolas, monospace"))
                                            SelectableTextBlock.verticalAlignment VerticalAlignment.Center
                                        ]
                                    ]
                                ]
                            ]
                        ]

                        // Created At
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "Created At"
                                ])
                                TextBlock.create [
                                    TextBlock.text (event.CreatedAt.ToString("yyyy-MM-dd HH:mm:ss UTC"))
                                ]
                            ]
                        ]

                        // Relays
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text $"Relays ({event.Relays.Length})"
                                ])
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.spacing 2.0
                                    StackPanel.children [
                                        for relay in event.Relays do
                                            TextBlock.create [
                                                TextBlock.text relay
                                                TextBlock.fontSize FontSizes.small
                                                TextBlock.foreground Colors.link
                                            ]
                                    ]
                                ]
                            ]
                        ]

                        // Content
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "Content"
                                ])
                                ScrollViewer.create [
                                    ScrollViewer.maxHeight 100.0
                                    ScrollViewer.content (
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text event.Content
                                            SelectableTextBlock.textWrapping TextWrapping.Wrap
                                            SelectableTextBlock.fontSize FontSizes.small
                                        ]
                                    )
                                ]
                            ]
                        ]

                        // Raw JSON
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "JSON"
                                ])
                                ScrollViewer.create [
                                    ScrollViewer.maxHeight 150.0
                                    ScrollViewer.content (
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text event.RawJson
                                            SelectableTextBlock.textWrapping TextWrapping.NoWrap
                                            SelectableTextBlock.fontSize FontSizes.small
                                            SelectableTextBlock.fontFamily (FontFamily("Consolas, monospace"))
                                        ]
                                    )
                                ]
                            ]
                        ]
                    ]
                ]
            )
        ]

    // Dialog for showing author info
    let private authorInfoDialog (author: AuthorId) (profile: UserProfile option) dispatch =
        let npub = NostrService.formatAuthorId author

        Border.create [
            Border.background (SolidColorBrush(Color.FromArgb(220uy, 30uy, 30uy, 35uy)))
            Border.padding (Thickness 20.0)
            Border.cornerRadius (CornerRadius 10.0)
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 15.0
                    StackPanel.width 450.0
                    StackPanel.children [
                        // Header
                        DockPanel.create [
                            DockPanel.children [
                                TextBlock.create (Attrs.subheading @ [
                                    TextBlock.dock Dock.Left
                                    TextBlock.text "Author Info"
                                ])
                                Button.create [
                                    Button.dock Dock.Right
                                    Button.content "X"
                                    Button.padding (Thickness(8.0, 4.0))
                                    Button.onClick (fun _ -> dispatch CloseDialog)
                                ]
                            ]
                        ]

                        // Display Name
                        match profile |> Option.bind (fun p -> p.DisplayName |> Option.orElse p.Name) with
                        | Some name ->
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 3.0
                                StackPanel.children [
                                    TextBlock.create (Attrs.caption @ [
                                        TextBlock.text "Name"
                                    ])
                                    TextBlock.create [
                                        TextBlock.text name
                                        TextBlock.fontSize FontSizes.medium
                                        TextBlock.fontWeight FontWeight.SemiBold
                                    ]
                                ]
                            ]
                        | None -> ()

                        // NIP-05
                        match profile |> Option.bind (fun p -> p.Nip05) with
                        | Some nip05 ->
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 3.0
                                StackPanel.children [
                                    TextBlock.create (Attrs.caption @ [
                                        TextBlock.text "NIP-05"
                                    ])
                                    TextBlock.create [
                                        TextBlock.text nip05
                                        TextBlock.foreground Colors.link
                                    ]
                                ]
                            ]
                        | None -> ()

                        // NPub
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.caption @ [
                                    TextBlock.text "Public Key (npub)"
                                ])
                                SelectableTextBlock.create [
                                    SelectableTextBlock.text npub
                                    SelectableTextBlock.textWrapping TextWrapping.Wrap
                                    SelectableTextBlock.fontSize FontSizes.small
                                    SelectableTextBlock.fontFamily (FontFamily("Consolas, monospace"))
                                ]
                            ]
                        ]

                        // About
                        match profile |> Option.bind (fun p -> p.About) with
                        | Some about when not (System.String.IsNullOrWhiteSpace about) ->
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 3.0
                                StackPanel.children [
                                    TextBlock.create (Attrs.caption @ [
                                        TextBlock.text "About"
                                    ])
                                    ScrollViewer.create [
                                        ScrollViewer.maxHeight 100.0
                                        ScrollViewer.content (
                                            TextBlock.create [
                                                TextBlock.text about
                                                TextBlock.textWrapping TextWrapping.Wrap
                                            ]
                                        )
                                    ]
                                ]
                            ]
                        | _ -> ()

                        // Subscribe button
                        Button.create (Attrs.primaryButton @ [
                            Button.content (
                                match profile with
                                | Some p when p.IsFollowed -> "Already Following"
                                | _ -> "Subscribe"
                            )
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.padding (Thickness(12.0, 8.0))
                            Button.isEnabled (
                                match profile with
                                | Some p -> not p.IsFollowed
                                | None -> true
                            )
                            Button.onClick (fun _ -> dispatch (SubscribeToAuthor author))
                        ])
                    ]
                ]
            )
        ]

    let private buildThreadedFeed (events: FeedEvent list) =
        let childrenMap =
            events
            |> List.choose (fun e ->
                e.ReplyTo |> Option.map (fun parentId -> EventId.toBytes parentId, e))
            |> List.groupBy fst
            |> List.map (fun (parentId, children) -> parentId, children |> List.map snd)
            |> Map.ofList

        let eventIds = events |> List.map (fun e -> EventId.toBytes e.Id) |> Set.ofList

        let rootEvents =
            events
            |> List.filter (fun e ->
                match e.ReplyTo with
                | None -> true
                | Some parentId -> not (Set.contains (EventId.toBytes parentId) eventIds))
            |> List.sortByDescending (fun e -> e.CreatedAt)

        let rec flatten depth (event: FeedEvent) : (int * FeedEvent) list =
            let eventBytes = EventId.toBytes event.Id
            let children =
                childrenMap
                |> Map.tryFind eventBytes
                |> Option.defaultValue []
                |> List.sortBy (fun e -> e.CreatedAt)
            (depth, event) :: (children |> List.collect (flatten (depth + 1)))

        rootEvents |> List.collect (flatten 0)

    let private feedEventView (model: Model) (depth: int) (event: FeedEvent) dispatch =
        let isReply = depth > 0
        let leftMargin = float depth * Dimensions.replyIndent

        let replyToInfo =
            event.ReplyTo
            |> Option.bind (fun replyToId ->
                let bytes = EventId.toBytes replyToId
                model.EventCache |> Map.tryFind bytes)

        let authorBytes = NostrService.authorIdToBytes event.Author

        Border.create (Attrs.feedEvent isReply @ [
            Border.margin (Spacing.leftOnly leftMargin)
            Border.contextMenu (eventContextMenu event dispatch)
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        Border.create (Attrs.avatar @ [
                            Border.dock Dock.Left
                            Border.background (Colors.avatarFromBytes authorBytes)
                            Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
                            Border.child (
                                TextBlock.create (Attrs.avatarText @ [
                                    TextBlock.text (
                                        event.AuthorName
                                        |> Option.map (fun n -> if n.Length > 0 then n.[0..0].ToUpper() else "?")
                                        |> Option.defaultValue "?"
                                    )
                                ])
                            )
                        ])
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 5.0
                            StackPanel.children [
                                if isReply then
                                    StackPanel.create [
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 5.0
                                        StackPanel.margin (Spacing.bottom 5.0)
                                        StackPanel.children [
                                            TextBlock.create (Attrs.tinyText @ [
                                                TextBlock.text "↳ replying to"
                                            ])
                                            TextBlock.create [
                                                TextBlock.fontSize FontSizes.tiny
                                                TextBlock.foreground Colors.link
                                                TextBlock.text (
                                                    match replyToInfo with
                                                    | Some parentEvent ->
                                                        parentEvent.AuthorName
                                                        |> Option.defaultValue (
                                                            let npub = NostrService.formatAuthorId parentEvent.Author
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
                                                    let npub = NostrService.formatAuthorId event.Author
                                                    npub[..15] + "..."
                                                )
                                            )
                                        ]
                                        TextBlock.create (Attrs.tinyText @ [
                                            TextBlock.dock Dock.Right
                                            TextBlock.text (event.CreatedAt.ToString("MMM dd, HH:mm"))
                                        ])
                                    ]
                                ]
                                renderContent event.Content
                            ]
                        ]
                    ]
                ]
            )
        ])

    let view (model: Model) dispatch =
        let threadedEvents = buildThreadedFeed model.Events

        let feedContent =
            ScrollViewer.create [
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.children [
                            for (depth, event) in threadedEvents do
                                feedEventView model depth event dispatch
                        ]
                    ]
                )
            ]

        // Wrap in a Panel to support dialog overlay
        Panel.create [
            Panel.children [
                feedContent

                // Dialog overlay
                match model.CurrentDialog with
                | NoDialog -> ()
                | ShowingEventInfo event ->
                    // Semi-transparent backdrop
                    Border.create [
                        Border.background (SolidColorBrush(Color.FromArgb(150uy, 0uy, 0uy, 0uy)))
                        Border.child (
                            Grid.create [
                                Grid.horizontalAlignment HorizontalAlignment.Center
                                Grid.verticalAlignment VerticalAlignment.Center
                                Grid.children [
                                    eventInfoDialog event dispatch
                                ]
                            ]
                        )
                    ]
                | ShowingAuthorInfo (author, profile) ->
                    Border.create [
                        Border.background (SolidColorBrush(Color.FromArgb(150uy, 0uy, 0uy, 0uy)))
                        Border.child (
                            Grid.create [
                                Grid.horizontalAlignment HorizontalAlignment.Center
                                Grid.verticalAlignment VerticalAlignment.Center
                                Grid.children [
                                    authorInfoDialog author profile dispatch
                                ]
                            ]
                        )
                    ]
            ]
        ]
