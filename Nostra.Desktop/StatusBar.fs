namespace Nostra.Desktop

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Styles

module StatusBar =

    let view (message: string) =
        Border.create (Attrs.statusBar @ [
            Border.child (
                TextBlock.create (Attrs.caption @ [
                    TextBlock.text message
                ])
            )
        ])
