module MessageProcessing

open System.Collections.Generic
open ClientRegistry
open EventStore
open FsToolkit.ErrorHandling
open System
open Nostra
open Nostra.Event
open Nostra.Relay
open Relay.Request
open Relay.Response
open YoLo

type SubscriptionStore = Dictionary<SubscriptionId, Filter list>

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

let processRequest (eventStore : EventStore) (subscriptions : SubscriptionStore) (clientRegistry : ClientRegistry) requestText = asyncResult {
    let! request =
        deserialize requestText
        |> Result.mapError (fun e -> Exception e)

    match request with
    | CMEvent event ->
        if verify event then
            let serializedEvent = requestText[(requestText.IndexOf "{")..(requestText.LastIndexOf "}")]
            let preprocessedEvent = preprocessEvent event serializedEvent
            do! storeEvent eventStore.saveEvent eventStore.deleteEvents preprocessedEvent            
            clientRegistry.notifyEvent preprocessedEvent
            return! Ok [ RMAck (event.Id, true, "added") ]
        else
            return! Ok [ RMAck (event.Id, false, "invalid: the signature is incorrect") ]
            
    | CMSubscribe(subscriptionId, filters) ->
        subscriptions.Add( subscriptionId, filters )
        let! matchingEvents = filterEvents eventStore.fetchEvents filters
        let relayMessages =
            matchingEvents
            |> List.map (fun event ->RMEvent(subscriptionId, event))

        return (RMEOSE subscriptionId) :: relayMessages  
        
    | CMUnsubscribe subscriptionId ->
        subscriptions.Remove subscriptionId |> ignore
        return! Ok []
    }

