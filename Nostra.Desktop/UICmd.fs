namespace Nostra.Desktop

module UICmd =
    open Avalonia.Threading
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


