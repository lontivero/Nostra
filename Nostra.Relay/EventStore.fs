module EventStore

open System
open Nostra
open Nostra.Event
open Nostra.Relay
type EventProcessorCommand =
    | StoreEvent of Event
    | GetEvents of Request.Filter list * AsyncReplyChannel<Async<Result<SerializedEvent list, exn>>>

type EventSaver = Event -> Result<StoredEvent, exn>
type EventsDeleter = XOnlyPubKey -> String list -> Result<int list, exn>
type EventsFetcher = Request.Filter -> Async<Result<SerializedEvent list, exn>>

type EventStore (saveEvent: EventSaver, deleteEvents: EventsDeleter, fetchEvents: EventsFetcher)  =
    let afterEventStoredEvent = Event<StoredEvent>()
    
    let worker =
        MailboxProcessor<EventProcessorCommand>.Start (fun inbox ->
            let rec loop () = async { 
                let! msg = inbox.Receive()
                match msg with
                | StoreEvent event ->
                    saveEvent event
                    |> Result.bind (
                        fun storedEvent ->
                            match event.Kind with
                            | Kind.Delete ->
                                storedEvent.RefEvents
                                |> deleteEvents event.PubKey
                                |> Result.map (fun _ -> storedEvent)
                            | _ -> Ok storedEvent)
                    |> function
                    | Ok storedEvent -> afterEventStoredEvent.Trigger storedEvent
                    | Error e ->  ignore e // TODO log the error
 
                | GetEvents (filters, reply) ->
                    filters
                    |> Seq.map fetchEvents
                    |> Seq.toList
                    |> Seq.iter reply.Reply
                return! loop() }
            loop () )
    member this.storeEvent event = worker.Post (StoreEvent event)
    member this.getEvents filters = worker.PostAndReply (fun replyChannel -> GetEvents (filters, replyChannel)) 
    [<CLIEvent>]
    member this.afterEventStored = afterEventStoredEvent.Publish


