namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open AsyncImageLoader
open Nostra
open Styles

module Navigation =

    type Model = {
        CurrentPage: AppPage
        CurrentAccount: Account option
        Accounts: Account list
        ShowAccountSwitcher: bool
    }

    type Msg =
        | NavigateTo of AppPage
        | ToggleAccountSwitcher
        | SwitchAccount of Account
        | CloseAccountSwitcher

    type ExternalMsg =
        | NoOp
        | PageChanged of AppPage
        | AccountSwitched of Account

    let init () = {
        CurrentPage = HomePage
        CurrentAccount = None
        Accounts = []
        ShowAccountSwitcher = false
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | NavigateTo page ->
            { model with CurrentPage = page; ShowAccountSwitcher = false }, PageChanged page
        | ToggleAccountSwitcher ->
            { model with ShowAccountSwitcher = not model.ShowAccountSwitcher }, NoOp
        | SwitchAccount account ->
            { model with CurrentAccount = Some account; ShowAccountSwitcher = false }, AccountSwitched account
        | CloseAccountSwitcher ->
            { model with ShowAccountSwitcher = false }, NoOp

    let private menuItem (currentPage: AppPage) (page: AppPage) (icon: string) (label: string) dispatch =
        let isSelected = currentPage = page
        Button.create [
            Button.classes (if isSelected then ["nav-item"; "selected"] else ["nav-item"])
            Button.horizontalAlignment HorizontalAlignment.Stretch
            Button.horizontalContentAlignment HorizontalAlignment.Left
            Button.padding (Thickness(15.0, 10.0))
            Button.background (if isSelected then Colors.selectedBackground else Brushes.Transparent)
            Button.onClick (fun _ -> dispatch (NavigateTo page))
            Button.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 12.0
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.text icon
                            TextBlock.fontSize FontSizes.medium
                            TextBlock.width 20.0
                        ]
                        TextBlock.create [
                            TextBlock.text label
                            TextBlock.fontSize FontSizes.normal
                            TextBlock.foreground (if isSelected then Colors.selectedText else Colors.normalText)
                        ]
                    ]
                ]
            )
        ]

    let private avatarImage (url: string) (size: float) =
        let imageView =
            Image.create [
                Image.width size
                Image.height size
                Image.stretch Stretch.UniformToFill
                Image.init (fun img -> ImageLoader.SetSource(img, url))
            ]
        Border.create [
            Border.width size
            Border.height size
            Border.cornerRadius (CornerRadius(size / 2.0))
            Border.clipToBounds true
            Border.child imageView
        ]

    let private avatarPlaceholder (name: string) (size: float) =
        let initial = if name.Length > 0 then name.[0..0].ToUpper() else "?"
        Border.create [
            Border.width size
            Border.height size
            Border.cornerRadius (CornerRadius(size / 2.0))
            Border.background Colors.avatarPlaceholder
            Border.child (
                TextBlock.create [
                    TextBlock.text initial
                    TextBlock.foreground Brushes.White
                    TextBlock.fontSize (size / 2.5)
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ]
            )
        ]

    let private accountItem (account: Account) (isCurrentAccount: bool) dispatch =
        Button.create [
            Button.classes ["account-item"]
            Button.horizontalAlignment HorizontalAlignment.Stretch
            Button.horizontalContentAlignment HorizontalAlignment.Left
            Button.padding (Thickness(10.0, 8.0))
            Button.background (if isCurrentAccount then Colors.selectedBackground else Brushes.Transparent)
            Button.onClick (fun _ -> dispatch (SwitchAccount account))
            Button.content (
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 10.0
                    StackPanel.children [
                        match account.Picture with
                        | Some url -> avatarImage url 32.0
                        | None -> avatarPlaceholder account.Name 32.0
                        TextBlock.create [
                            TextBlock.text account.Name
                            TextBlock.fontSize FontSizes.normal
                            TextBlock.foreground Colors.normalText
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ]
                    ]
                ]
            )
        ]

    let private accountSwitcherPopup (model: Model) dispatch =
        Border.create [
            Border.background Colors.popupBackground
            Border.cornerRadius (CornerRadius(8.0))
            Border.padding (Thickness(5.0))
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 2.0
                    StackPanel.children (
                        model.Accounts
                        |> List.map (fun acc ->
                            let isCurrent =
                                match model.CurrentAccount with
                                | Some current -> AuthorId.toBytes current.PublicKey = AuthorId.toBytes acc.PublicKey
                                | None -> false
                            accountItem acc isCurrent dispatch
                        )
                    )
                ]
            )
        ]

    let private currentAccountView (model: Model) dispatch : IView =
        match model.CurrentAccount with
        | Some account ->
            Button.create [
                Button.classes ["current-account"]
                Button.horizontalAlignment HorizontalAlignment.Stretch
                Button.horizontalContentAlignment HorizontalAlignment.Left
                Button.padding (Thickness(10.0, 12.0))
                Button.background Brushes.Transparent
                Button.onClick (fun _ -> dispatch ToggleAccountSwitcher)
                Button.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 10.0
                        StackPanel.children [
                            match account.Picture with
                            | Some url -> avatarImage url 40.0
                            | None -> avatarPlaceholder account.Name 40.0
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.verticalAlignment VerticalAlignment.Center
                                StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.text account.Name
                                        TextBlock.fontSize FontSizes.normal
                                        TextBlock.foreground Colors.normalText
                                        TextBlock.fontWeight FontWeight.SemiBold
                                    ]
                                    TextBlock.create [
                                        TextBlock.text (
                                            let npub = Shareable.encodeNpub account.PublicKey
                                            $"{npub.[0..12]}..."
                                        )
                                        TextBlock.fontSize FontSizes.small
                                        TextBlock.foreground Colors.mutedText
                                    ]
                                ]
                            ]
                        ]
                    ]
                )
            ]
        | None ->
            Border.create [
                Border.padding (Thickness(10.0))
                Border.child (
                    TextBlock.create [
                        TextBlock.text "No account"
                        TextBlock.fontSize FontSizes.small
                        TextBlock.foreground Colors.mutedText
                    ]
                )
            ]

    let view (model: Model) (stats: ConnectionStats) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Left
            DockPanel.width Dimensions.sidebarWidth
            DockPanel.background Colors.sidebarBackground
            DockPanel.children [
                // Current account at bottom
                Border.create [
                    Border.dock Dock.Bottom
                    Border.borderThickness (Thickness(0.0, 1.0, 0.0, 0.0))
                    Border.borderBrush Colors.borderColor
                    Border.child (currentAccountView model dispatch)
                ]

                // Stats panel above account
                Border.create [
                    Border.dock Dock.Bottom
                    Border.padding Spacing.small
                    Border.background Colors.statusPanelBackground
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 3.0
                            StackPanel.children [
                                TextBlock.create (Attrs.statsText @ [
                                    TextBlock.text $"EVENTS RECV {stats.EventsReceived}"
                                ])
                                TextBlock.create (Attrs.statsText @ [
                                    TextBlock.text $"EVENTS STOR {stats.EventsStored}"
                                ])
                                TextBlock.create (Attrs.statsText @ [
                                    TextBlock.text $"RELAYS CONN {stats.RelaysConnected}"
                                ])
                                TextBlock.create (Attrs.statsText @ [
                                    TextBlock.text $"SUBSCRIPTIONS {stats.ActiveSubscriptions}"
                                ])
                            ]
                        ]
                    )
                ]

                // Navigation menu
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.margin (Thickness(0.0, 10.0, 0.0, 0.0))
                    StackPanel.children [
                        menuItem model.CurrentPage HomePage "🏠" "Home" dispatch
                        menuItem model.CurrentPage NotificationsPage "🔔" "Notifications" dispatch
                        menuItem model.CurrentPage BookmarksPage "🔖" "Bookmarks" dispatch
                        menuItem model.CurrentPage SearchPage "🔍" "Search" dispatch
                        menuItem model.CurrentPage SettingsPage "⚙" "Settings" dispatch
                    ]
                ]
            ]
        ]
