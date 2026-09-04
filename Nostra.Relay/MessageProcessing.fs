module MessageProcessing

open System.Collections.Generic
open ClientRegistry
open EventStore
open FsToolkit.ErrorHandling
open System
open Nostra
open Nostra.ClientContext
open Nostra.Relay
open Nostra.Relay.InfoDocument
open Nostra.Relay.Plugin
open Relay.Request
open Relay.Response
open Relay.Configuration

type SubscriptionStore = Dictionary<SubscriptionId, Filter list>

type Context = {
    eventStore : EventStore
    clientRegistry : ClientRegistry
    logger: IOLogger
    config: RelayConfig
    pluginManager: PluginManager
}

type EventProcessingError =
    | UnexpectedError of Exception
    | BusinessError of RelayMessage

let preprocessEvent (event : Event) serializedEvent =
    let (EventId eventId) = event.Id
    let (AuthorId author) = event.PubKey
    let pubkey = author.ToBytes()

    {
        Event = event
        Id = Utils.toHex eventId
        PubKey = Utils.toHex pubkey
        Serialized = serializedEvent
        Seen = DateTime.UtcNow
    }

let ackError eventId error =
    BusinessError (RMAck (eventId, false, error))

let noticeError error =
    BusinessError (RMNotice error)

let checkWritePolicy (event: Event) (pluginManager: PluginManager) (timeoutSeconds: int) (sourceInfo: string) (logger: IOLogger) =
    let action = pluginManager.acceptEvent timeoutSeconds event EventSourceType.IP4 sourceInfo
    match action with
    | Ok Accept -> true
    | Ok (Reject msg) ->
        logger.logInfo $"{event.Id} blocked: {msg}"
        false
    | Result.Error msg ->
        logger.logError $"{event.Id} {msg}"
        false

let canPersistEvent (event : Event) (limits : Limitation) (pluginManager: PluginManager) (timeoutSeconds: int) (sourceInfo: string) (logger : IOLogger) = result {
    do! Result.requireTrue (ackError event.Id "invalid: too many tags") (event.Tags.Length <= limits.MaxEventTags)
    do! Result.requireTrue (ackError event.Id "invalid: content too large") (event.Content.Length <= limits.MaxContentLength)
    do! Result.requireTrue (ackError event.Id "invalid: the signature is incorrect") (Event.verify event)
    do! Result.requireTrue (ackError event.Id "event cannot be accepted") (checkWritePolicy event pluginManager timeoutSeconds sourceInfo logger)
    }

let verifyCanSubscribe (subscriptionId : SubscriptionId) filters (subscriptionStore : SubscriptionStore) (limits : Limitation) = result {
    do! Result.requireTrue (noticeError "too large subscription id") (subscriptionId.Length <= limits.MaxSubidLength)
    let filterCount = Seq.length filters
    do! Result.requireTrue (noticeError "too many filters") (filterCount <= limits.MaxFilters)
    let isNewSubscription = not (subscriptionStore.ContainsKey subscriptionId)
    let subscriptionCount = Seq.length subscriptionStore
    do! Result.requireTrue (noticeError "too many subscriptions") (subscriptionCount < limits.MaxSubscriptions || not isNewSubscription)
    }

let processRequest (env : Context) (subscriptionStore : SubscriptionStore) requestText = asyncResult {
    let limits = env.config.RelayInfo.Limitation

    let! request =
        deserialize requestText
        |> Result.mapError (fun err ->
            env.logger.logError $"invalid: it was not possible to deserialize: {err}. Raw request: {requestText}"
            noticeError "invalid: it was not possible to deserialize")

    do! Result.requireTrue (noticeError "message too large") (requestText.Length <= limits.MaxMessageLength)

    match request with
    | CMEvent event ->
        let timeoutSeconds = env.config.WritePolicy.TimeoutSeconds
        do! canPersistEvent event limits env.pluginManager timeoutSeconds "websocket" env.logger
        let serializedEvent = requestText[(requestText.IndexOf "{")..(requestText.LastIndexOf "}")]

        let preprocessedEvent = preprocessEvent event serializedEvent
        do! storeEvent env.eventStore.saveEvent env.eventStore.deleteEvents preprocessedEvent
        env.clientRegistry.notifyEvent preprocessedEvent
        return! Ok [ RMAck (event.Id, true, "added") ]

    | CMSubscribe(subscriptionId, filters) ->
        do! (verifyCanSubscribe subscriptionId filters subscriptionStore limits)
        subscriptionStore[subscriptionId] <- filters
        let! matchingEvents =
            filterEvents env.eventStore.fetchEvents filters DateTime.Now
            |> AsyncResult.mapError (fun _ -> noticeError "Something was wrong.")

        let relayMessages =
            matchingEvents
            |> List.map (fun event ->RMEvent(subscriptionId, event))

        return (RMEOSE subscriptionId) :: relayMessages

    | CMUnsubscribe subscriptionId ->
        subscriptionStore.Remove subscriptionId |> ignore
        return! Ok []
    }

