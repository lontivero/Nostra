module EventStore

open System
open FsToolkit.ErrorHandling
open Nostra
open Nostra.Event
open Nostra.Relay

type EventSaver = StoredEvent -> Async<Result<int list, exn>>
type EventsDeleter = XOnlyPubKey -> String list -> Async<Result<int list, exn>>
type EventsFetcher = Request.Filter -> Async<Result<SerializedEvent list, exn>>

type EventStore = {
    saveEvent : EventSaver
    deleteEvents : EventsDeleter
    fetchEvents : EventsFetcher
}

let filterEvents (fetchEvents : EventsFetcher) filters = asyncResult {
    let! matchingEventsResult =
        filters
        |> List.map fetchEvents
        |> List.sequenceAsyncA
        
    let matchingEvents = 
        matchingEventsResult
        |> List.sequenceResultM
        |> Result.bind (fun eventsList ->
            eventsList
            |> List.concat
            |> Ok)
        
    return! matchingEvents
    }
    

let storeEvent (saveEvent : EventSaver) (deleteEvents : EventsDeleter) preprocessed = async {
    if not (isEphemeral preprocessed.Event) then
        let! _ = saveEvent preprocessed 
        match preprocessed.Event.Kind with
        | Kind.Delete ->
            let! _ = deleteEvents preprocessed.Event.PubKey preprocessed.RefEvents
            ()
        | _ -> ()
}