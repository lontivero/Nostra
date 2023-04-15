module EventStore

open System
open FsToolkit.ErrorHandling
open Nostra
open Nostra.Event
open Nostra.Relay

type EventSaver = Event -> SerializedEvent -> Async<Result<StoredEvent, exn>>
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
    
let storeEvent (saveEvent : EventSaver) (deleteEvents : EventsDeleter) event serializedEvent=
    saveEvent event serializedEvent
    |> AsyncResult.bind(
        fun storedEvent ->
            match event.Kind with
            | Kind.Delete ->
                storedEvent.RefEvents
                |> deleteEvents event.PubKey
                |> AsyncResult.map (fun _ -> storedEvent)
            | _ -> AsyncResult.ok storedEvent)
