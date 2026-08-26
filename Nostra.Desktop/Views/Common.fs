namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Nostra.Desktop.Styles

module Common =
    let header headerContent =
        TextBlock.create (Attrs.heading @ [
            TextBlock.dock Dock.Top
            TextBlock.text headerContent
            TextBlock.margin (Spacing.bottom 15.0)
        ])
