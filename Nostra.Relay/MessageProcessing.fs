module MessageProcessing

open System.Collections.Generic
open ClientRegistry
open EventStore
open FsToolkit.ErrorHandling
open System
open Nostra
open Nostra.ClientContext
open Nostra.Event
open Nostra.Relay
open Relay.Request
open Relay.Response
open YoLo

type SubscriptionStore = Dictionary<SubscriptionId, Filter list>

type Context = {
    eventStore : EventStore
    clientRegistry : ClientRegistry
    logger: IOLogger
}

type EventProcessingError =
    | UnexpectedError of Exception
    | BusinessError of RelayMessage

let preprocessEvent (event : Event) serializedEvent =
    let (EventId eventId) = event.Id
    let (XOnlyPubKey xOnlyPubkey) = event.PubKey
    let pubkey = xOnlyPubkey.ToBytes()

    let tagsByKey key tags =
        tags
        |> List.filter (fun (k,_) -> k = key)
        |> List.map snd

    let tags = List.ungroup event.Tags

    {
        Event = event
        Id = Utils.toHex eventId
        PubKey = Utils.toHex pubkey
        Serialized = serializedEvent
        Seen = DateTime.UtcNow
        Tags = event.Tags
        RefEvents = tagsByKey "e" tags
        RefPubKeys = tagsByKey "p" tags
        DTag =
            tagsByKey "d" tags
            |> List.tryHead
    }

let ackError eventId error =
    BusinessError (RMAck (eventId, false, error))

let noticeError error =
    BusinessError (RMNotice error)

let canPersistEvent (event : Event) = result {
    do! Result.requireTrue (ackError event.Id "invalid: the signature is incorrect") (verify event)
    }

let verifyCanSubscribe subscriptionId filters (subscriptionStore : SubscriptionStore) = result {
    let filterCount = Seq.length filters
    do! Result.requireTrue (noticeError "Too many filters") (filterCount < 5)
    let subscriptionCount = Seq.length subscriptionStore
    do! Result.requireTrue (noticeError "Too many subscription") (subscriptionCount < 10)
    let exists, existingFilters = subscriptionStore.TryGetValue(subscriptionId)
    do! Result.requireFalse (noticeError "Duplicated subscription") (exists && existingFilters = filters)
    }

let processRequest (env : Context) (subscriptionStore : SubscriptionStore) requestText = asyncResult {
    let! request =
        deserialize requestText
        |> Result.mapError (fun _ -> noticeError "invalid: it was not possible to deserialize")

    match request with
    | CMEvent event ->
        do! (canPersistEvent event)
        let serializedEvent = requestText[(requestText.IndexOf "{")..(requestText.LastIndexOf "}")]

        let preprocessedEvent = preprocessEvent event serializedEvent
        do! storeEvent env.eventStore.saveEvent env.eventStore.deleteEvents preprocessedEvent
        env.clientRegistry.notifyEvent preprocessedEvent
        return! Ok [ RMAck (event.Id, true, "added") ]

    | CMSubscribe(subscriptionId, filters) ->
        do! (verifyCanSubscribe subscriptionId filters subscriptionStore)
        subscriptionStore.Add( subscriptionId, filters )
        let! matchingEvents =
            filterEvents env.eventStore.fetchEvents filters DateTime.Now
            |> AsyncResult.mapError (fun ec -> noticeError "Something was wrong.")

        let relayMessages =
            matchingEvents
            |> List.map (fun event ->RMEvent(subscriptionId, event))

        return (RMEOSE subscriptionId) :: relayMessages

    | CMUnsubscribe subscriptionId ->
        subscriptionStore.Remove subscriptionId |> ignore
        return! Ok []
    }

