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

type ValidationFailure = string * EventProcessingError

let formatClientId (ClientId(ip, port)) = $"{ip}:{port}"

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

let formatEventId (EventId bytes) = Utils.toHex bytes

let ackError eventId logDetails clientMessage : ValidationFailure =
    (logDetails, BusinessError (RMAck (eventId, false, clientMessage)))

let noticeError logDetails clientMessage : ValidationFailure =
    (logDetails, BusinessError (RMNotice clientMessage))

let checkWritePolicy (event: Event) (pluginManager: PluginManager) (timeoutSeconds: int) (sourceInfo: string) (logger: IOLogger) =
    let action = pluginManager.acceptEvent timeoutSeconds event EventSourceType.IP4 sourceInfo
    match action with
    | Ok Accept -> true
    | Ok (Reject msg) ->
        logger.logInfo $"blocked: {msg}"
        false
    | Result.Error msg ->
        logger.logError $"{msg}"
        false

let canPersistEvent (event : Event) (limits : Limitation) (pluginManager: PluginManager) (timeoutSeconds: int) (sourceInfo: string) (logger : IOLogger) : Result<unit, ValidationFailure> = result {
    do! Result.requireTrue (ackError event.Id $"too many tags ({event.Tags.Length} > {limits.MaxEventTags})" "invalid: too many tags") (event.Tags.Length <= limits.MaxEventTags)
    do! Result.requireTrue (ackError event.Id $"content too large ({event.Content.Length} > {limits.MaxContentLength})" "invalid: content too large") (event.Content.Length <= limits.MaxContentLength)
    do! Result.requireTrue (ackError event.Id "invalid signature" "invalid: the signature is incorrect") (Event.verify event)
    do! Result.requireTrue (ackError event.Id "blocked by write policy" "event cannot be accepted") (checkWritePolicy event pluginManager timeoutSeconds sourceInfo logger)
    }

let verifyCanSubscribe (subscriptionId : SubscriptionId) filters (subscriptionStore : SubscriptionStore) (limits : Limitation) : Result<unit, ValidationFailure> = result {
    do! Result.requireTrue (noticeError $"subscription id too large ({subscriptionId.Length} > {limits.MaxSubidLength})" "too large subscription id") (subscriptionId.Length <= limits.MaxSubidLength)
    let filterCount = Seq.length filters
    do! Result.requireTrue (noticeError $"too many filters ({filterCount} > {limits.MaxFilters})" "too many filters") (filterCount <= limits.MaxFilters)
    let isNewSubscription = not (subscriptionStore.ContainsKey subscriptionId)
    let subscriptionCount = Seq.length subscriptionStore
    do! Result.requireTrue (noticeError $"too many subscriptions ({subscriptionCount} >= {limits.MaxSubscriptions})" "too many subscriptions") (subscriptionCount < limits.MaxSubscriptions || not isNewSubscription)
    }

let processRequest (env : Context) (clientId : ClientId) (subscriptionStore : SubscriptionStore) requestText = asyncResult {
    let limits = env.config.RelayInfo.Limitation
    let clientIdStr = formatClientId clientId

    let logAndExtractError (logMessage, error) =
        env.logger.logWarn $"[{clientIdStr}] {logMessage}"
        error

    let! request =
        deserialize requestText
        |> Result.mapError (fun err ->
            env.logger.logError $"[{clientIdStr}] Failed to deserialize: {err}. Raw request: {requestText}"
            BusinessError (RMNotice "invalid: it was not possible to deserialize"))

    do! Result.requireTrue (noticeError $"message too large ({requestText.Length} > {limits.MaxMessageLength})" "message too large") (requestText.Length <= limits.MaxMessageLength)
        |> Result.mapError logAndExtractError

    match request with
    | CMEvent event ->
        let timeoutSeconds = env.config.WritePolicy.TimeoutSeconds
        do! canPersistEvent event limits env.pluginManager timeoutSeconds "websocket" env.logger
            |> Result.mapError (fun (logMessage, error) ->
                env.logger.logWarn $"[{clientIdStr}] Rejected event {formatEventId event.Id}: {logMessage}"
                error)
        let serializedEvent = requestText[(requestText.IndexOf "{")..(requestText.LastIndexOf "}")]

        let preprocessedEvent = preprocessEvent event serializedEvent
        do! storeEvent env.eventStore.saveEvent env.eventStore.deleteEvents preprocessedEvent
        env.clientRegistry.notifyEvent preprocessedEvent
        return! Ok [ RMAck (event.Id, true, "added") ]

    | CMSubscribe(subscriptionId, filters) ->
        do! verifyCanSubscribe subscriptionId filters subscriptionStore limits
            |> Result.mapError logAndExtractError
        subscriptionStore[subscriptionId] <- filters
        let! matchingEvents =
            filterEvents env.eventStore.fetchEvents filters DateTime.UtcNow
            |> AsyncResult.mapError (fun err ->
                env.logger.logWarn $"[{clientIdStr}] query failed: {err}"
                BusinessError (RMNotice "Something was wrong."))

        let relayMessages =
            matchingEvents
            |> List.map (fun event ->RMEvent(subscriptionId, event))

        return (RMEOSE subscriptionId) :: relayMessages

    | CMUnsubscribe subscriptionId ->
        subscriptionStore.Remove subscriptionId |> ignore
        return! Ok []

    | CMCount(subscriptionId, filters) ->
        let! count =
            env.eventStore.countEvents filters DateTime.UtcNow
            |> AsyncResult.mapError (fun err ->
                env.logger.logWarn $"[{clientIdStr}] COUNT query failed: {err}"
                BusinessError (RMNotice "Something was wrong."))
        return! Ok [ RMCount (subscriptionId, count) ]
    }

