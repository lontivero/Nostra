module EventStore

open Nostra
open Nostra.Event
open Nostra.Relay

type EventSaver = StoredEvent -> Async<Result<unit, exn>>
type EventsDeleter = XOnlyPubKey -> string list -> Async<Result<int list, exn>>
type EventsFetcher = Request.Filter list -> System.DateTime -> Async<Result<SerializedEvent list, exn>>

type EventStore = {
    saveEvent : EventSaver
    deleteEvents : EventsDeleter
    fetchEvents : EventsFetcher
}

let filterEvents (fetchEvents : EventsFetcher) filters = 
    filters
    |> fetchEvents
    

let storeEvent (saveEvent : EventSaver) (deleteEvents : EventsDeleter) preprocessed = async {
    if not (isEphemeral preprocessed.Event) then
        let! _ = saveEvent preprocessed 
        match preprocessed.Event.Kind with
        | Kind.Delete ->
            let! _ = deleteEvents preprocessed.Event.PubKey preprocessed.RefEvents
            ()
        | _ -> ()
}