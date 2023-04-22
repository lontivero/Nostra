namespace Nostra

open System
open System.Text
open Thoth.Json.Net

module Relay =
    open Nostra.Event
    
    type StoredEvent = {
        Id: string
        Event: Event
        PubKey: string
        Serialized: SerializedEvent
        Seen: DateTime
        RefEvents: string list
        RefPubKeys: string list
    }
    
    module Request =
        type Filter = {
            Ids: string list
            Kinds: Kind list 
            Authors: string list 
            Limit: int option 
            Since: DateTime option 
            Until: DateTime option 
            Events: string list 
            PubKeys: string list 
        }

        module Filter =
            module Decode =
                let filter : Decoder<Filter> =
                    Decode.object (fun get -> {
                        Ids = get.Optional.Field "ids" (Decode.list Decode.string) |> Option.defaultValue [] 
                        Kinds = get.Optional.Field "kinds" (Decode.list Decode.Enum.int) |> Option.defaultValue []
                        Authors = get.Optional.Field "authors" (Decode.list Decode.string) |> Option.defaultValue []
                        Limit = get.Optional.Field "limit" Decode.int
                        Since = get.Optional.Field "since" Decode.unixDateTime
                        Until = get.Optional.Field "until" Decode.unixDateTime
                        Events = get.Optional.Field "#e" (Decode.list Decode.string) |> Option.defaultValue []
                        PubKeys = get.Optional.Field "#p" (Decode.list Decode.string) |> Option.defaultValue []
                    })

            let eventMatchesFilter (eventInfo: StoredEvent) filter =
                let matchList items list =
                    match list with
                    | [] -> true
                    | _ ->
                        Set.intersect (Set.ofList list) (Set.ofList items)
                        |> Set.isEmpty
                        |> not

                let isInTimeWindow =
                    match filter.Since, filter.Until with
                    | None, None -> true
                    | Some since, None -> eventInfo.Event.CreatedAt >= since
                    | None, Some until -> eventInfo.Event.CreatedAt <= until
                    | Some since, Some until -> eventInfo.Event.CreatedAt >= since && eventInfo.Event.CreatedAt <= until
                
                isInTimeWindow &&
                filter.Ids |> matchList [eventInfo.Id] &&
                filter.Kinds |> matchList [eventInfo.Event.Kind] &&
                filter.Authors |> matchList [eventInfo.PubKey] &&
                filter.Events |> matchList eventInfo.RefEvents &&
                filter.PubKeys |> matchList eventInfo.RefPubKeys
             
            let eventMatchesAnyFilter (filters: Filter list) (event: StoredEvent) =
                filters |> List.exists (eventMatchesFilter event)

        type ClientMessage =
            | CMEvent of Event
            | CMSubscribe of SubscriptionId * Filter list
            | CMUnsubscribe of SubscriptionId

        module Decode =
            let listOfFilters : Decoder<Filter list> =
                fun path token ->
                    let items = Decode.Helpers.asArray token
                    let len = items.Length
                    if len >= 3 then
                        (Ok [], items |> Array.mapi (fun i x -> i, x) |> Array.skip 2 )
                        ||> Array.fold (fun acc values ->
                            match acc with
                            | Error _ -> acc
                            | Ok acc ->
                                match Decode.index (fst values) Filter.Decode.filter path token with
                                | Error er -> Error er
                                | Ok value -> Ok (value::acc))
                    else
                        Error ("", BadType("", token))
                        
            let clientMessage : Decoder<ClientMessage> =
                Decode.index 0 Decode.string
                |> Decode.andThen ( function
                    | "EVENT" ->
                        Decode.map
                            (fun event -> CMEvent event)
                            (Decode.index 1 Decode.event)
                    | "CLOSE" ->
                        Decode.map
                            (fun subscriptionId -> CMUnsubscribe subscriptionId)
                            (Decode.index 1 Decode.string)
                    | "REQ" ->
                        Decode.map2
                            (fun subscriptionId filters -> CMSubscribe (subscriptionId, filters))
                            (Decode.index 1 Decode.string)
                            listOfFilters
                    | _ -> Decode.fail "Client request type is unknown")

        let deserialize str  =
            Decode.fromString Decode.clientMessage str

    module Response =
        type RelayMessage =
            | RMEvent of SubscriptionId * SerializedEvent
            | RMNotice of string
            | RMAck of EventId * bool * string
            | RMEOSE of string
            
        module Encode =
            let quote (x: string) = "\"" + x + "\""
            let serialize xs =
                "[" + (String.concat "," xs) + "]"
                
            let relayMessage = function
                | RMEvent (subscriptionId, serializedEvent) ->
                    serialize (seq {
                        yield quote "EVENT"
                        yield quote subscriptionId
                        yield serializedEvent } )   
                | RMNotice message ->
                    serialize (seq {
                        yield quote "NOTICE"
                        yield quote message})
                | RMAck (EventId eventId, success, message) ->
                    serialize (seq {
                        yield quote "OK"
                        yield quote (eventId |> Utils.toHex)   
                        yield (if success then "true" else "false")
                        yield quote message })
                | RMEOSE subscriptionId ->
                    serialize (seq {
                        yield quote "EOSE"
                        yield quote subscriptionId})

        let serialize (msg: RelayMessage) =
            msg |> Encode.relayMessage

        let toPayload (msg: RelayMessage) =
            msg
            |> serialize
            |> Encoding.UTF8.GetBytes
            |> ArraySegment

    module InfoDocument =
        let getRelayInfoDocument () =
            Encode.object [
                "name", Encode.string ""
                "description", Encode.string "Relay without persistence"
                "pubkey", Encode.string ""
                "contact", Encode.string "lucasontivero@gmail.com"
                "supported_nips", Encode.list (List.map Encode.int [1; 2; 4; 9; 11; 12; 16; 20; 33 ]) 
                "software", Encode.string "https://github.com/lontivero/Nostra/"
                "version", Encode.string "0.0.1"
            ]
            |> Encode.toString 2