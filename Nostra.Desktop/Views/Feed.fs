namespace Nostra.Desktop

open System.Diagnostics
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open AsyncImageLoader
open Nostra
open Nostra.Desktop.Infrastructure
open Nostra.Desktop.Store
open Styles

module Feed =

    /// Create an image from a byte array (cached image data)
    let private imageFromBytes (bytes: byte[]) (width: float) (height: float) (stretch: Stretch) =
        Image.create [
            Image.width width
            Image.height height
            Image.stretch stretch
            Image.init (fun img ->
                use stream = new MemoryStream(bytes)
                img.Source <- new Bitmap(stream))
        ]

    /// Create an image from URL (async loaded)
    let private imageFromUrl (url: string) (width: float) (height: float) (stretch: Stretch) =
        Image.create [
            Image.width width
            Image.height height
            Image.stretch stretch
            Image.init (fun img -> ImageLoader.SetSource(img, url))
        ]

    /// Create a profile image - uses cached bytes if available, falls back to URL
    let private profileImage (profile: UserProfile option) (width: float) (height: float) (stretch: Stretch) =
        match profile with
        | Some p ->
            match p.PictureData, p.Picture with
            | Some bytes, _ ->
                imageFromBytes bytes width height stretch
            | None, Some url ->
                imageFromUrl url width height stretch
            | None, None ->
                Image.create []
        | None ->
            Image.create []

    let private cachedImage (url: string) (width: float) (height: float) (stretch: Stretch) =
        imageFromUrl url width height stretch

    let private cachedImageConstrained (url: string) (maxWidth: float) (maxHeight: float) (stretch: Stretch) =
        Image.create [
            Image.maxWidth maxWidth
            Image.maxHeight maxHeight
            Image.stretch stretch
            Image.init (fun img -> ImageLoader.SetSource(img, url))
        ]

    // URL preview data
    type PreviewData = {
        Image: string option
        Title: string option
        Description: string option
        SiteName: string option
    }

    // Twitter/X tweet preview data
    type TweetData = {
        AuthorName: string
        AuthorHandle: string
        Content: string
        ProfileImage: string option
    }

    // URL preview state - must be defined before renderContentPart
    type UrlPreview =
        | PreviewPending
        | PreviewLoaded of PreviewData
        | TweetPreviewLoaded of TweetData
        | PreviewNotAvailable

    let private openUrlInBrowser (url: string) =
        try
            let psi = ProcessStartInfo(url, UseShellExecute = true)
            Process.Start(psi) |> ignore
        with _ -> ()

    let private renderContentPart
        (profileCache: Map<byte[], UserProfile>)
        (eventCache: Map<byte[], FeedEvent>)
        (urlPreviewCache: Map<string, UrlPreview>)
        (requestUrlPreview: string -> unit)
        (part: Content.ContentPart) : Types.IView =
        match part with
        | Content.TextPart text ->
            TextBlock.create (Attrs.wrappedText @ [
                TextBlock.text text
            ])
        | Content.ImagePart url ->
            Border.create [
                Border.margin (Thickness(0.0, 8.0))
                Border.cornerRadius (CornerRadius 8.0)
                Border.clipToBounds true
                Border.child (cachedImageConstrained url 400.0 300.0 Stretch.Uniform)
            ]
        | Content.UrlPart url ->
            let previewState = urlPreviewCache |> Map.tryFind url
            // Request preview if not yet requested
            if previewState.IsNone then
                requestUrlPreview url

            match previewState with
            | Some (PreviewLoaded data) ->
                // Full preview card with image, title, description, URL
                Border.create [
                    Border.margin (Thickness(0.0, 8.0))
                    Border.cornerRadius (CornerRadius 8.0)
                    Border.background (SolidColorBrush(Color.FromArgb(40uy, 100uy, 100uy, 120uy)))
                    Border.borderBrush (SolidColorBrush(Color.FromArgb(60uy, 150uy, 150uy, 150uy)))
                    Border.borderThickness (Thickness 1.0)
                    Border.cursor (new Cursor(StandardCursorType.Hand))
                    Border.onPointerPressed (fun _ -> openUrlInBrowser url)
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                // Preview image (if available)
                                match data.Image with
                                | Some imgUrl ->
                                    Border.create [
                                        Border.cornerRadius (CornerRadius(8.0, 8.0, 0.0, 0.0))
                                        Border.clipToBounds true
                                        Border.child (cachedImageConstrained imgUrl 400.0 200.0 Stretch.UniformToFill)
                                    ]
                                | None -> ()

                                // Text content
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.margin (Thickness 12.0)
                                    StackPanel.spacing 4.0
                                    StackPanel.children [
                                        // Site name
                                        match data.SiteName with
                                        | Some siteName ->
                                            TextBlock.create [
                                                TextBlock.text siteName
                                                TextBlock.foreground Colors.muted
                                                TextBlock.fontSize FontSizes.tiny
                                            ]
                                        | None -> ()

                                        // Title
                                        match data.Title with
                                        | Some title ->
                                            TextBlock.create [
                                                TextBlock.text title
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                TextBlock.maxLines 2
                                            ]
                                        | None -> ()

                                        // Description
                                        match data.Description with
                                        | Some desc ->
                                            let shortDesc = if desc.Length > 150 then desc.Substring(0, 147) + "..." else desc
                                            TextBlock.create [
                                                TextBlock.text shortDesc
                                                TextBlock.foreground Colors.muted
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                TextBlock.fontSize FontSizes.small
                                                TextBlock.maxLines 3
                                            ]
                                        | None -> ()

                                        // URL
                                        TextBlock.create [
                                            TextBlock.text url
                                            TextBlock.foreground Colors.link
                                            TextBlock.fontSize FontSizes.tiny
                                            TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    )
                ]

            | Some (TweetPreviewLoaded tweet) ->
                // Twitter/X tweet card
                Border.create [
                    Border.margin (Thickness(0.0, 8.0))
                    Border.cornerRadius (CornerRadius 12.0)
                    Border.background (SolidColorBrush(Color.FromArgb(50uy, 29uy, 155uy, 240uy)))
                    Border.borderBrush (SolidColorBrush(Color.FromArgb(80uy, 29uy, 155uy, 240uy)))
                    Border.borderThickness (Thickness 1.0)
                    Border.padding (Thickness 12.0)
                    Border.cursor (new Cursor(StandardCursorType.Hand))
                    Border.onPointerPressed (fun _ -> openUrlInBrowser url)
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 8.0
                            StackPanel.children [
                                // Header with X logo and author info
                                DockPanel.create [
                                    DockPanel.children [
                                        // X logo on the right
                                        TextBlock.create [
                                            TextBlock.dock Dock.Right
                                            TextBlock.text "𝕏"
                                            TextBlock.fontSize 18.0
                                            TextBlock.foreground (SolidColorBrush(Color.FromRgb(29uy, 155uy, 240uy)))
                                        ]
                                        // Author info
                                        StackPanel.create [
                                            StackPanel.orientation Orientation.Horizontal
                                            StackPanel.spacing 8.0
                                            StackPanel.children [
                                                // Profile image
                                                match tweet.ProfileImage with
                                                | Some imgUrl ->
                                                    Border.create [
                                                        Border.width 40.0
                                                        Border.height 40.0
                                                        Border.cornerRadius (CornerRadius 20.0)
                                                        Border.clipToBounds true
                                                        Border.child (cachedImage imgUrl 40.0 40.0 Stretch.UniformToFill)
                                                    ]
                                                | None ->
                                                    Border.create [
                                                        Border.width 40.0
                                                        Border.height 40.0
                                                        Border.cornerRadius (CornerRadius 20.0)
                                                        Border.background (SolidColorBrush(Color.FromRgb(29uy, 155uy, 240uy)))
                                                        Border.child (
                                                            TextBlock.create [
                                                                TextBlock.text (tweet.AuthorName.Substring(0, 1).ToUpper())
                                                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                                TextBlock.verticalAlignment VerticalAlignment.Center
                                                                TextBlock.fontWeight FontWeight.Bold
                                                                TextBlock.foreground Brushes.White
                                                            ]
                                                        )
                                                    ]
                                                // Name and handle
                                                StackPanel.create [
                                                    StackPanel.orientation Orientation.Vertical
                                                    StackPanel.verticalAlignment VerticalAlignment.Center
                                                    StackPanel.children [
                                                        TextBlock.create [
                                                            TextBlock.text tweet.AuthorName
                                                            TextBlock.fontWeight FontWeight.SemiBold
                                                        ]
                                                        TextBlock.create [
                                                            TextBlock.text tweet.AuthorHandle
                                                            TextBlock.foreground Colors.muted
                                                            TextBlock.fontSize FontSizes.small
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]

                                // Tweet content
                                TextBlock.create [
                                    TextBlock.text tweet.Content
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.maxLines 6
                                ]
                            ]
                        ]
                    )
                ]

            | Some PreviewPending ->
                // Show loading indicator
                Border.create [
                    Border.margin (Thickness(0.0, 8.0))
                    Border.padding (Thickness 12.0)
                    Border.cornerRadius (CornerRadius 8.0)
                    Border.background (SolidColorBrush(Color.FromArgb(30uy, 100uy, 100uy, 100uy)))
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 4.0
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.text "Loading preview..."
                                    TextBlock.foreground Colors.muted
                                    TextBlock.fontSize FontSizes.small
                                ]
                                TextBlock.create [
                                    TextBlock.text url
                                    TextBlock.foreground Colors.link
                                    TextBlock.fontSize FontSizes.tiny
                                    TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                    TextBlock.cursor (new Cursor(StandardCursorType.Hand))
                                    TextBlock.onPointerPressed (fun _ -> openUrlInBrowser url)
                                ]
                            ]
                        ]
                    )
                ]

            | Some PreviewNotAvailable | None ->
                // Simple clickable URL
                TextBlock.create [
                    TextBlock.text url
                    TextBlock.foreground Colors.link
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.fontSize FontSizes.small
                    TextBlock.cursor (new Cursor(StandardCursorType.Hand))
                    TextBlock.onPointerPressed (fun _ -> openUrlInBrowser url)
                ]
        | Content.HashtagPart tag ->
            TextBlock.create [
                TextBlock.text $"#{tag}"
                TextBlock.foreground Colors.link
            ]
        | Content.MentionPart (authorId, _relays) ->
            let authorBytes = NostrService.authorIdToBytes authorId
            let displayName =
                profileCache
                |> Map.tryFind authorBytes
                |> Option.bind (fun p -> p.DisplayName |> Option.orElse p.Name)
                |> Option.defaultValue (
                    let npub = NostrService.formatAuthorId authorId
                    "@" + npub[..12] + "...")
            TextBlock.create [
                TextBlock.text (if displayName.StartsWith("@") then displayName else "@" + displayName)
                TextBlock.foreground Colors.link
                TextBlock.fontWeight FontWeight.SemiBold
            ]
        | Content.EventPart (eventId, _relays, _author) ->
            let eventBytes = EventId.toBytes eventId
            let displayText =
                eventCache
                |> Map.tryFind eventBytes
                |> Option.map (fun e ->
                    let preview = if e.Content.Length > 50 then e.Content.Substring(0, 47) + "..." else e.Content
                    $"📝 {preview}")
                |> Option.defaultValue (
                    let noteId = Shareable.encodeNote eventId
                    "📝 " + noteId[..15] + "...")
            Border.create [
                Border.background (SolidColorBrush(Color.FromArgb(30uy, 100uy, 100uy, 255uy)))
                Border.cornerRadius (CornerRadius 4.0)
                Border.padding (Thickness(6.0, 3.0))
                Border.margin (Thickness(0.0, 2.0))
                Border.child (
                    TextBlock.create [
                        TextBlock.text displayText
                        TextBlock.foreground Colors.link
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.fontSize FontSizes.small
                    ]
                )
            ]

    let private renderContent
        (profileCache: Map<byte[], UserProfile>)
        (eventCache: Map<byte[], FeedEvent>)
        (urlPreviewCache: Map<string, UrlPreview>)
        (requestUrlPreview: string -> unit)
        (content: string) : Types.IView =
        let parts = Content.parseContent content
        if parts.Length = 1 then
            match parts[0] with
            | Content.TextPart text ->
                TextBlock.create (Attrs.wrappedText @ [
                    TextBlock.text text
                ])
            | part ->
                renderContentPart profileCache eventCache urlPreviewCache requestUrlPreview part
        else
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 4.0
                StackPanel.children [
                    for part in parts do
                        renderContentPart profileCache eventCache urlPreviewCache requestUrlPreview part
                ]
            ]

    // Dialog state for showing event or author info
    type DialogState =
        | NoDialog
        | ShowingEventInfo of FeedEvent
        | ShowingAuthorInfo of AuthorId * UserProfile option * IsFollowed: bool

    type Model = {
        Events: FeedEvent list
        EventCache: Map<byte[], FeedEvent>
        ProfileCache: Map<byte[], UserProfile>
        UrlPreviewCache: Map<string, UrlPreview>
        FollowedAuthors: Set<byte[]>  // Track followed status separately
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
        | AuthorUnfollowed of AuthorId
        // URL preview
        | UrlPreviewRequested of url: string
        | UrlPreviewReceivedFull of url: string * preview: UrlPreview

    type ExternalMsg =
        | NoOp
        | RequestProfile of AuthorId
        | RequestEvents of EventId list
        | SubscribeAuthor of AuthorId
        | RequestUrlPreview of string

    let init () = {
        Events = []
        EventCache = Map.empty
        ProfileCache = Map.empty
        UrlPreviewCache = Map.empty
        FollowedAuthors = Set.empty
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

            let externalMsg =
                if needsProfile then RequestProfile event.Author
                else NoOp

            { model with
                Events = updatedEvents
                EventCache = updatedEventCache }, externalMsg

        | ProfileUpdated (authorBytes, profile) ->
            let authorHex = NostrService.formatAuthorId profile.AuthorId
            // Preserve existing PictureData if the incoming profile doesn't have any
            let existingPictureData =
                model.ProfileCache
                |> Map.tryFind authorBytes
                |> Option.bind (fun p -> p.PictureData)
            let profileWithData =
                match profile.PictureData, existingPictureData with
                | None, Some existingBytes -> { profile with PictureData = Some existingBytes }
                | _ -> profile
            printfn "[TRACE-FEED] ProfileUpdated %s: PictureData=%s, Picture=%s"
                (authorHex.[..15])
                (if profileWithData.PictureData.IsSome then $"{profileWithData.PictureData.Value.Length} bytes" else "None")
                (if profileWithData.Picture.IsSome then "yes" else "no")
            let updatedCache = model.ProfileCache |> Map.add authorBytes profileWithData
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
            let isFollowed = model.FollowedAuthors |> Set.contains authorBytes
            { model with CurrentDialog = ShowingAuthorInfo (event.Author, profile, isFollowed) }, NoOp

        | ViewEventInfo event ->
            { model with CurrentDialog = ShowingEventInfo event }, NoOp

        | SubscribeToAuthor author ->
            { model with CurrentDialog = NoDialog }, SubscribeAuthor author

        | CloseDialog ->
            { model with CurrentDialog = NoDialog }, NoOp

        | AuthorFollowed author ->
            let authorBytes = NostrService.authorIdToBytes author
            // Add to FollowedAuthors set
            let updatedFollowed = model.FollowedAuthors |> Set.add authorBytes
            // Also update the dialog if it's showing this author
            let updatedDialog =
                match model.CurrentDialog with
                | ShowingAuthorInfo (dialogAuthor, profile, _) when NostrService.authorIdToBytes dialogAuthor = authorBytes ->
                    ShowingAuthorInfo (dialogAuthor, profile, true)
                | other -> other
            { model with FollowedAuthors = updatedFollowed; CurrentDialog = updatedDialog }, NoOp

        | AuthorUnfollowed author ->
            let authorBytes = NostrService.authorIdToBytes author
            // Remove from FollowedAuthors set
            let updatedFollowed = model.FollowedAuthors |> Set.remove authorBytes
            // Also update the dialog if it's showing this author
            let updatedDialog =
                match model.CurrentDialog with
                | ShowingAuthorInfo (dialogAuthor, profile, _) when NostrService.authorIdToBytes dialogAuthor = authorBytes ->
                    ShowingAuthorInfo (dialogAuthor, profile, false)
                | other -> other
            { model with FollowedAuthors = updatedFollowed; CurrentDialog = updatedDialog }, NoOp

        | UrlPreviewRequested url ->
            // Mark as pending to prevent duplicate requests
            let updatedCache = model.UrlPreviewCache |> Map.add url PreviewPending
            { model with UrlPreviewCache = updatedCache }, RequestUrlPreview url

        | UrlPreviewReceivedFull (url, preview) ->
            let updatedCache = model.UrlPreviewCache |> Map.add url preview
            { model with UrlPreviewCache = updatedCache }, NoOp

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
    let private authorInfoDialog (author: AuthorId) (profile: UserProfile option) (isFollowed: bool) dispatch =
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

                        // Profile Picture
                        let hasPicture = profile |> Option.map (fun p -> p.PictureData.IsSome || p.Picture.IsSome) |> Option.defaultValue false
                        if hasPicture then
                            Border.create [
                                Border.width 80.0
                                Border.height 80.0
                                Border.cornerRadius (CornerRadius 40.0)
                                Border.clipToBounds true
                                Border.horizontalAlignment HorizontalAlignment.Center
                                Border.child (profileImage profile 80.0 80.0 Stretch.UniformToFill)
                            ]
                        else
                            let authorBytes = NostrService.authorIdToBytes author
                            Border.create [
                                Border.width 80.0
                                Border.height 80.0
                                Border.cornerRadius (CornerRadius 40.0)
                                Border.background (Colors.avatarFromBytes authorBytes)
                                Border.horizontalAlignment HorizontalAlignment.Center
                                Border.child (
                                    TextBlock.create [
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.foreground Colors.buttonText
                                        TextBlock.fontWeight FontWeight.Bold
                                        TextBlock.fontSize 32.0
                                        TextBlock.text (
                                            profile
                                            |> Option.bind (fun p -> p.DisplayName |> Option.orElse p.Name)
                                            |> Option.map (fun n -> if n.Length > 0 then n.[0..0].ToUpper() else "?")
                                            |> Option.defaultValue "?"
                                        )
                                    ]
                                )
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
                        match profile |> Option.bind _.Nip05 with
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
                            Button.content (if isFollowed then "Already Following" else "Subscribe")
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.padding (Thickness(12.0, 8.0))
                            Button.isEnabled (not isFollowed)
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

    let private feedEventView (store: DomainStore) (model: Model) (depth: int) (event: FeedEvent) dispatch =
        let isReply = depth > 0
        let leftMargin = float depth * Dimensions.replyIndent

        let replyToInfo =
            event.ReplyTo
            |> Option.bind (fun replyToId ->
                let bytes = EventId.toBytes replyToId
                model.EventCache |> Map.tryFind bytes)

        let authorBytes = NostrService.authorIdToBytes event.Author

        // Get profile from DomainStore, falling back to legacy cache
        let authorProfile =
            DomainStore.getCachedProfile event.Author store
            |> Option.map (fun cached ->
                // Convert CachedProfile to UserProfile for legacy view code
                { UserProfile.AuthorId = cached.Profile.AuthorId
                  Name = cached.Profile.Name
                  DisplayName = cached.Profile.DisplayName
                  About = cached.Profile.About
                  Picture = cached.Profile.Picture
                  PictureData = cached.PictureData
                  Nip05 = cached.Profile.Nip05 })
            |> Option.orElse (model.ProfileCache |> Map.tryFind authorBytes)

        Border.create (Attrs.feedEvent isReply @ [
            Border.margin (Spacing.leftOnly leftMargin)
            Border.contextMenu (eventContextMenu event dispatch)
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        Border.create (Attrs.avatar @ [
                            Border.dock Dock.Left
                            Border.background (
                                let hasPicture = authorProfile |> Option.map (fun p -> p.PictureData.IsSome || p.Picture.IsSome) |> Option.defaultValue false
                                if hasPicture then Brushes.Transparent :> IBrush
                                else Colors.avatarFromBytes authorBytes
                            )
                            Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
                            Border.clipToBounds true
                            Border.child (
                                let hasPicture = authorProfile |> Option.map (fun p -> p.PictureData.IsSome || p.Picture.IsSome) |> Option.defaultValue false
                                if hasPicture then
                                    profileImage authorProfile Dimensions.avatarSize Dimensions.avatarSize Stretch.UniformToFill
                                    :> Types.IView
                                else
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
                                let requestUrlPreview url = dispatch (UrlPreviewRequested url)
                                renderContent model.ProfileCache model.EventCache model.UrlPreviewCache requestUrlPreview event.Content
                            ]
                        ]
                    ]
                ]
            )
        ])

    let view (store: DomainStore) (model: Model) dispatch =
        let threadedEvents = buildThreadedFeed model.Events

        let feedContent =
            ScrollViewer.create [
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.children [
                            for (depth, event) in threadedEvents do
                                feedEventView store model depth event dispatch
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
                | ShowingAuthorInfo (author, profile, isFollowed) ->
                    Border.create [
                        Border.background (SolidColorBrush(Color.FromArgb(150uy, 0uy, 0uy, 0uy)))
                        Border.child (
                            Grid.create [
                                Grid.horizontalAlignment HorizontalAlignment.Center
                                Grid.verticalAlignment VerticalAlignment.Center
                                Grid.children [
                                    authorInfoDialog author profile isFollowed dispatch
                                ]
                            ]
                        )
                    ]
            ]
        ]
