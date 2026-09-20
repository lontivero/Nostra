module EventStore

open FsToolkit.ErrorHandling
open Nostra
open Nostra.Relay

type EventSaver = StoredEvent -> Async<Result<unit, exn>>
type EventsDeleter = AuthorId -> string list -> Async<Result<int list, exn>>
type DeletableChecker = AuthorId -> string list -> Async<Result<bool, exn>>
type EventsFetcher = Request.Filter list -> System.DateTime -> Async<Result<SerializedEvent list, exn>>
type EventsCounter = Request.Filter list -> System.DateTime -> Async<Result<int, exn>>

type EventStore = {
    saveEvent : EventSaver
    deleteEvents : EventsDeleter
    hasDeletableEvents : DeletableChecker
    fetchEvents : EventsFetcher
    countEvents : EventsCounter
}

let filterEvents (fetchEvents : EventsFetcher) filters =
    filters
    |> fetchEvents


type StoreResult = Stored | DeletionHasNoEffect

let storeEvent (saveEvent : EventSaver) (deleteEvents : EventsDeleter) (hasDeletableEvents : DeletableChecker) preprocessed = asyncResult {
    if Event.isEphemeral preprocessed.Event then
        return Stored
    else
        match preprocessed.Event.Kind with
        | Kind.Delete ->
            let eRef = Tag.findByKey "e" preprocessed.Event.Tags
            let! hasDeletable = hasDeletableEvents preprocessed.Event.PubKey eRef
            if hasDeletable then
                let! _ = saveEvent preprocessed
                let! _ = deleteEvents preprocessed.Event.PubKey eRef
                return Stored
            else
                return DeletionHasNoEffect
        | _ ->
            let! _ = saveEvent preprocessed
            return Stored
}