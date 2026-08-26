namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

module Styles =
    // Colors
    module Colors =
        let connected = Brushes.Green :> IBrush
        let connecting = Brushes.Orange :> IBrush
        let disconnected = Brushes.Gray :> IBrush
        let error = Brushes.Red :> IBrush
        let muted = Brushes.Gray :> IBrush
        let accent = Brushes.Purple :> IBrush
        let link = Brushes.CornflowerBlue :> IBrush
        let buttonText = Brushes.White :> IBrush
        let primaryButton = SolidColorBrush(Color.FromRgb(88uy, 101uy, 242uy)) :> IBrush // Discord-like blue
        let secondaryButton = Brushes.Gray :> IBrush
        let dangerButton = SolidColorBrush(Color.FromRgb(237uy, 66uy, 69uy)) :> IBrush

        // Sidebar colors (Gossip-like dark theme)
        let sidebarBackground = SolidColorBrush(Color.FromRgb(32uy, 34uy, 37uy)) :> IBrush
        let statusPanelBackground = SolidColorBrush(Color.FromRgb(24uy, 25uy, 28uy)) :> IBrush
        let selectedBackground = SolidColorBrush(Color.FromArgb(60uy, 88uy, 101uy, 242uy)) :> IBrush
        let selectedText = Brushes.White :> IBrush
        let normalText = SolidColorBrush(Color.FromRgb(185uy, 187uy, 190uy)) :> IBrush
        let statsText = SolidColorBrush(Color.FromRgb(114uy, 118uy, 125uy)) :> IBrush

        // Content area
        let contentBackground = SolidColorBrush(Color.FromRgb(54uy, 57uy, 63uy)) :> IBrush
        let cardBackground = SolidColorBrush(Color.FromArgb(30uy, 100uy, 100uy, 100uy)) :> IBrush
        let cardBackgroundAlt = SolidColorBrush(Color.FromArgb(20uy, 100uy, 100uy, 100uy)) :> IBrush
        let statusBarBackground = SolidColorBrush(Color.FromArgb(30uy, 0uy, 0uy, 0uy)) :> IBrush
        let feedEventBackground = SolidColorBrush(Color.FromArgb(20uy, 80uy, 80uy, 100uy)) :> IBrush
        let replyEventBackground = SolidColorBrush(Color.FromArgb(35uy, 80uy, 80uy, 140uy)) :> IBrush
        let inputBackground = SolidColorBrush(Color.FromRgb(64uy, 68uy, 75uy)) :> IBrush

        // Account and popup colors
        let mutedText = SolidColorBrush(Color.FromRgb(114uy, 118uy, 125uy)) :> IBrush
        let avatarPlaceholder = SolidColorBrush(Color.FromRgb(88uy, 101uy, 242uy)) :> IBrush
        let popupBackground = SolidColorBrush(Color.FromRgb(47uy, 49uy, 54uy)) :> IBrush
        let borderColor = SolidColorBrush(Color.FromRgb(60uy, 63uy, 68uy)) :> IBrush

        let avatarFromBytes (bytes: byte array) =
            if bytes.Length >= 3 then
                SolidColorBrush(Color.FromRgb(bytes.[0], bytes.[1], bytes.[2])) :> IBrush
            else
                SolidColorBrush(Color.FromRgb(100uy, 100uy, 200uy)) :> IBrush

    // Font sizes
    module FontSizes =
        let tiny = 11.0
        let small = 12.0
        let normal = 14.0
        let medium = 16.0
        let large = 18.0

    // Dimensions
    module Dimensions =
        let avatarSize = 40.0
        let avatarRadius = 20.0
        let sidebarWidth = 220.0
        let followedPanelWidth = 200.0
        let relayInputWidth = 300.0
        let searchInputWidth = 450.0
        let maxContentWidth = 500.0
        let replyIndent = 25.0

    // Spacing and margins
    module Spacing =
        let none = Thickness 0.0
        let tiny = Thickness 5.0
        let small = Thickness 10.0
        let medium = Thickness 15.0

        let horizontal s = Thickness(s, 0.0, 0.0, 0.0)
        let vertical s = Thickness(0.0, s, 0.0, 0.0)
        let bottom s = Thickness(0.0, 0.0, 0.0, s)
        let top s = Thickness(0.0, s, 0.0, 0.0)
        let leftOnly left = Thickness(left, 5.0, 0.0, 5.0)

    // Corner radius
    module Radius =
        let small = CornerRadius 3.0
        let medium = CornerRadius 5.0
        let large = CornerRadius 8.0
        let circular = CornerRadius 20.0

    // Reusable attribute builders
    module Attrs =
        // Text styles
        let heading: IAttr<TextBlock> list = [
            TextBlock.fontSize FontSizes.large
            TextBlock.fontWeight FontWeight.Bold
        ]

        let subheading: IAttr<TextBlock> list = [
            TextBlock.fontSize FontSizes.medium
            TextBlock.fontWeight FontWeight.Bold
        ]

        let caption: IAttr<TextBlock> list = [
            TextBlock.fontSize FontSizes.small
            TextBlock.foreground Colors.muted
        ]

        let tinyText: IAttr<TextBlock> list = [
            TextBlock.fontSize FontSizes.tiny
            TextBlock.foreground Colors.muted
        ]

        let wrappedText: IAttr<TextBlock> list = [
            TextBlock.textWrapping TextWrapping.Wrap
        ]

        let ellipsisText: IAttr<TextBlock> list = [
            TextBlock.textTrimming TextTrimming.CharacterEllipsis
        ]

        // Container styles
        let card: IAttr<Border> list = [
            Border.padding Spacing.medium
            Border.cornerRadius Radius.large
            Border.background Colors.cardBackground
        ]

        let cardSmall: IAttr<Border> list = [
            Border.padding Spacing.tiny
            Border.cornerRadius Radius.small
            Border.background Colors.cardBackgroundAlt
        ]

        let feedEvent (isReply: bool): IAttr<Border> list = [
            Border.padding Spacing.small
            Border.cornerRadius Radius.medium
            Border.background (if isReply then Colors.replyEventBackground else Colors.feedEventBackground)
        ]

        let statusBar: IAttr<Border> list = [
            Border.dock Dock.Bottom
            Border.padding Spacing.small
            Border.background Colors.statusBarBackground
        ]

        // Avatar style
        let avatar: IAttr<Border> list = [
            Border.width Dimensions.avatarSize
            Border.height Dimensions.avatarSize
            Border.cornerRadius Radius.circular
            Border.verticalAlignment VerticalAlignment.Top
        ]

        let avatarText: IAttr<TextBlock> list = [
            TextBlock.horizontalAlignment HorizontalAlignment.Center
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.foreground Colors.buttonText
            TextBlock.fontWeight FontWeight.Bold
            TextBlock.fontSize FontSizes.medium
        ]

        // Button styles
        let primaryButton: IAttr<Button> list = [
            Button.background Colors.primaryButton
            Button.foreground Colors.buttonText
        ]

        let secondaryButton: IAttr<Button> list = [
            Button.background Colors.secondaryButton
            Button.foreground Colors.buttonText
        ]

        let dangerButton: IAttr<Button> list = [
            Button.background Colors.dangerButton
            Button.foreground Colors.buttonText
        ]

        // Stats panel text
        let statsText: IAttr<TextBlock> list = [
            TextBlock.fontSize FontSizes.tiny
            TextBlock.foreground Colors.statsText
            TextBlock.fontFamily (FontFamily("Consolas, monospace"))
        ]

        // Relay status indicator
        let relayStatusDot (status: RelayStatus): IAttr<TextBlock> list = [
            TextBlock.fontSize FontSizes.small
            TextBlock.foreground (
                match status with
                | RelayConnected -> Colors.connected
                | RelayConnecting -> Colors.connecting
                | RelayDisconnected -> Colors.disconnected
                | RelayError _ -> Colors.error
            )
        ]