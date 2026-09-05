namespace Nostra

open System
open System.Diagnostics
open System.Text
open System.Threading
open Nostra
open Thoth.Json.Net


module Relay =

    module Plugin =
        type EventSourceType =
            | IP4
            | IP6
            | Import
            | Stream
            | Sync

        type Action =
            | Accept
            | Reject of string

        type Response = {
            Id: string
            Action: Action
        }

        module EventSourceType =
            let toString = function
                | IP4 -> "IP4"
                | IP6 -> "IP6"
                | Import -> "Import"
                | Stream -> "Stream"
                | Sync -> "Sync"

        module Response =
            let decode : Decoder<Response> =
                Decode.object (fun get ->
                    get.Required.Field "id" Decode.string,
                    get.Required.Field "action" Decode.string,
                    get.Optional.Field "msg" Decode.string)
                |> Decode.andThen (fun (id, action, msg) ->
                    match action with
                    | "accept" -> Decode.succeed { Id = id; Action = Accept }
                    | "reject" -> Decode.succeed { Id = id; Action = Reject (msg |> Option.defaultValue "") }
                    | unknown -> Decode.fail $"unknown action: {unknown}")

        module RequestJson =
            let build (nostrEvent: Nostra.Event) (sourceType: EventSourceType) (sourceInfo: string) =
                Encode.object [
                    "type", Encode.string "new"
                    "event", Nostra.Event.Encode.event nostrEvent
                    "receivedAt", Encode.int64 (DateTimeOffset.UtcNow.ToUnixTimeSeconds())
                    "sourceType", Encode.string (EventSourceType.toString sourceType)
                    "sourceInfo", Encode.string sourceInfo
                ]
                |> Encode.toString 0

        type EventRequest = {
            NostrEvent: Nostra.Event
            SourceType: EventSourceType
            SourceInfo: string
            TimeoutSeconds: int
            ReplyChannel: AsyncReplyChannel<Result<Action, string>>
        }

        type ManagerMessage =
            | ProcessEvent of EventRequest
            | Shutdown of AsyncReplyChannel<unit>

        type PluginState =
            | Idle
            | Running of Process

        module PluginState =
            let dispose = function
                | Running proc ->
                    try
                        if not proc.HasExited then proc.Kill()
                        proc.Dispose()
                    with _ -> ()
                | Idle -> ()

            let spawnPlugin (pluginCmd: string) =
                let splitCommandLine (cmd: string) : string list =
                    let step (tokens, sb : StringBuilder, inQuotes) c =
                        match c with
                        | '"' ->
                            (tokens, sb, not inQuotes)
                        | ' ' when not inQuotes && sb.Length > 0 ->
                            (sb.ToString() :: tokens, StringBuilder(), false)
                        | c ->
                            sb.Append(c) |> ignore
                            (tokens, sb, inQuotes)

                    let tokens, sb, _ = Seq.fold step ([], StringBuilder(), false) cmd

                    if sb.Length > 0
                    then List.rev (sb.ToString() :: tokens)
                    else List.rev tokens

                match splitCommandLine pluginCmd with
                | exe :: args ->
                    let psi =
                        ProcessStartInfo(
                            FileName = exe,
                            UseShellExecute = false,
                            RedirectStandardInput = true,
                            RedirectStandardOutput = true,
                            RedirectStandardError = true,
                            CreateNoWindow = true)
                    args |> List.iter psi.ArgumentList.Add
                    Running (Process.Start(psi))
                | [] -> failwith "Plugin command is empty"

            let getProcess = function
                | Running proc when not proc.HasExited -> Some proc
                | _ -> None

        let private executePluginRequest
                (timeoutSeconds: int)
                (nostrEvent: Nostra.Event)
                (sourceType: EventSourceType)
                (sourceInfo: string)
                (proc: PluginState) =
            let (EventId eventIdBytes) = nostrEvent.Id
            let eventId = Utils.toHex eventIdBytes
            let request = RequestJson.build nostrEvent sourceType sourceInfo

            try
                proc |> PluginState.getProcess |> function
                | None -> Error "Plugin not running", Idle
                | Some plugin ->
                    plugin.StandardInput.WriteLine(request)
                    plugin.StandardInput.Flush()

                    use cts = new CancellationTokenSource(TimeSpan.FromSeconds(float timeoutSeconds))

                    let response =
                        try
                            plugin.StandardOutput.ReadLineAsync(cts.Token).AsTask()
                            |> Async.AwaitTask
                            |> Async.RunSynchronously
                            |> Some
                        with
                        | :? OperationCanceledException -> None

                    match response with
                    | None ->
                        PluginState.dispose proc
                        Error "Plugin timeout", Idle
                    | Some line when isNull line ->
                        PluginState.dispose proc
                        Error "Plugin closed unexpectedly", Idle
                    | Some line ->
                        match Decode.fromString Response.decode line with
                        | Ok pluginResponse when pluginResponse.Id = eventId ->
                            Ok pluginResponse.Action, proc
                        | Ok pluginResponse ->
                            PluginState.dispose proc
                            Error $"Plugin returned wrong event id: expected {eventId}, got {pluginResponse.Id}", Idle
                        | Result.Error err ->
                            Error $"Failed to parse plugin response: {err}", proc
            with ex ->
                PluginState.dispose proc
                Error $"Plugin error: {ex.Message}", Idle

        [<TailCall>]
        let rec private processPluginRequestLoop
                (pluginCmd: string)
                (state: PluginState)
                (inbox: MailboxProcessor<ManagerMessage>) = async {

            let! msg = inbox.Receive()

            match msg with
            | Shutdown replyChannel ->
                PluginState.dispose state
                replyChannel.Reply()

            | ProcessEvent request ->
                let currentState =
                    match PluginState.getProcess state with
                    | Some _ -> state
                    | None ->
                        PluginState.dispose state
                        PluginState.spawnPlugin pluginCmd

                let result, newState =
                    executePluginRequest
                        request.TimeoutSeconds
                        request.NostrEvent
                        request.SourceType
                        request.SourceInfo
                        currentState

                request.ReplyChannel.Reply(result)
                return! processPluginRequestLoop pluginCmd newState inbox
        }

        type PluginManager = {
            acceptEvent: int -> Event -> EventSourceType -> string -> Result<Action, string>
            shutdown: unit -> unit
        }

        let createPluginManager (pluginCmd: string) =
            let worker =
                MailboxProcessor<ManagerMessage>.Start(
                    processPluginRequestLoop pluginCmd Idle)

            {
                acceptEvent = fun timeoutSeconds event sourceType sourceInfo ->
                    worker.PostAndReply(fun replyChannel ->
                        ProcessEvent {
                            NostrEvent = event
                            SourceType = sourceType
                            SourceInfo = sourceInfo
                            TimeoutSeconds = timeoutSeconds
                            ReplyChannel = replyChannel
                        })

                shutdown = fun () ->
                    worker.PostAndReply Shutdown
            }

        let createAcceptAllPluginManager () =
            {
                acceptEvent = fun _ _ _ _ -> Ok Accept
                shutdown = fun () -> ()
            }
