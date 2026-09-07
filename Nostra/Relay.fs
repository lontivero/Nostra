namespace Nostra

open System
open System.Reflection
open System.Text
open Thoth.Json.Net

module Relay =

    let getAssemblyVersion () =
        let assembly = Assembly.GetExecutingAssembly()
        let attr = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
        if isNull attr then "0.0.1"
        else attr.InformationalVersion

    type StoredEvent = {
        Id: string
        Event: Event
        PubKey: string
        Serialized: SerializedEvent
        Seen: DateTime
    }

    module Request =
        type Filter = {
            Ids: string list
            Kinds: Kind list
            Authors: string list
            Limit: int option
            Since: DateTime option
            Until: DateTime option
            Tags: Tag list
        }

        module Filter =
            module Decode =
                let filter : Decoder<Filter> =
                    let knownDecoder = Decode.object (fun get -> {
                        Ids = get.Optional.Field "ids" (Decode.list Decode.string) |> Option.defaultValue []
                        Kinds = get.Optional.Field "kinds" (Decode.list Decode.Enum.int) |> Option.defaultValue []
                        Authors = get.Optional.Field "authors" (Decode.list Decode.string) |> Option.defaultValue []
                        Limit = get.Optional.Field "limit" Decode.int
                        Since = get.Optional.Field "since" Decode.unixDateTime
                        Until = get.Optional.Field "until" Decode.unixDateTime
                        Tags = []
                    })

                    let tagsDecoder : Decoder<Tag list> =
                        fun path value ->
                            match Decode.keys path value with
                            | Ok objectKeys ->
                                let tagKeys = objectKeys |> Seq.filter (fun t -> t.Length > 1 && t.StartsWith "#")
                                (Ok [], tagKeys ) ||> Seq.fold (fun acc prop ->
                                    match acc with
                                    | Error _ -> acc
                                    | Ok acc ->
                                        match Decode.Helpers.getField prop value |> (Decode.list Decode.string) path with
                                        | Error er -> Error er
                                        | Ok value -> (prop, value)::acc |> Ok)
                                |> Result.map List.rev
                            | Error e -> Error e

                    Decode.map2 (fun known tags -> { known with Tags = tags })
                        knownDecoder
                        tagsDecoder

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

                let removeCat (tagKey:string) = if tagKey.StartsWith("#") then tagKey[1..] else tagKey
                isInTimeWindow &&
                filter.Ids     |> matchList [eventInfo.Id] &&
                filter.Kinds   |> matchList [eventInfo.Event.Kind] &&
                filter.Authors |> matchList [eventInfo.PubKey] &&
                filter.Tags    |> Tag.ungroup |> List.map (fun (k,v) -> removeCat k, v ) |> matchList (Tag.ungroup eventInfo.Event.Tags)

            let eventMatchesAnyFilter (filters: Filter list) (event: StoredEvent) =
                filters |> List.exists (eventMatchesFilter event)

        type ClientMessage =
            | CMEvent of Event
            | CMSubscribe of SubscriptionId * Filter list
            | CMUnsubscribe of SubscriptionId
            | CMCount of SubscriptionId * Filter list

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
                            CMEvent
                            (Decode.index 1 Event.Decode.event)
                    | "CLOSE" ->
                        Decode.map
                            CMUnsubscribe
                            (Decode.index 1 Decode.string)
                    | "REQ" ->
                        Decode.map2
                            (fun subscriptionId filters -> CMSubscribe (subscriptionId, filters))
                            (Decode.index 1 Decode.string)
                            listOfFilters
                    | "COUNT" ->
                        Decode.map2
                            (fun subscriptionId filters -> CMCount (subscriptionId, filters))
                            (Decode.index 1 Decode.string)
                            listOfFilters
                    | c -> Decode.fail $"Client request type '{c}' is unknown")

        let deserialize str  =
            Decode.fromString Decode.clientMessage str

    module Response =
        type RelayMessage =
            | RMEvent of SubscriptionId * SerializedEvent
            | RMNotice of string
            | RMAck of EventId * bool * string
            | RMEOSE of string
            | RMCount of SubscriptionId * int

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
                | RMCount (subscriptionId, count) ->
                    serialize (seq {
                        yield quote "COUNT"
                        yield quote subscriptionId
                        yield $"{{\"count\":{count}}}"})

        let serialize (msg: RelayMessage) =
            msg |> Encode.relayMessage

        let toPayload (msg: RelayMessage) =
            msg
            |> serialize
            |> Encoding.UTF8.GetBytes
            |> ArraySegment

    module InfoDocument =
        type Limitation = {
            MaxMessageLength: int
            MaxSubscriptions: int
            MaxFilters: int
            MaxLimit: int
            DefaultLimit: int
            MaxSubidLength: int
            MaxEventTags: int
            MaxContentLength: int
            MinPowDifficulty: int
            AuthRequired: bool
            PaymentRequired: bool
            RestrictedWrites: bool
            CreatedAtLowerLimit: int option
            CreatedAtUpperLimit: int option
        }

        module Limitation =
            let defaults = {
                MaxMessageLength = 524288
                MaxSubscriptions = 10
                MaxFilters = 5
                MaxLimit = 5000
                DefaultLimit = 50
                MaxSubidLength = 100
                MaxEventTags = 2000
                MaxContentLength = 102400
                MinPowDifficulty = 0
                AuthRequired = false
                PaymentRequired = false
                RestrictedWrites = false
                CreatedAtLowerLimit = None
                CreatedAtUpperLimit = None
            }

            let decode : Decoder<Limitation> =
                Decode.object (fun get ->
                    let defaults = defaults
                    {
                        MaxMessageLength = get.Optional.Field "max_message_length" Decode.int |> Option.defaultValue defaults.MaxMessageLength
                        MaxSubscriptions = get.Optional.Field "max_subscriptions" Decode.int |> Option.defaultValue defaults.MaxSubscriptions
                        MaxFilters = get.Optional.Field "max_filters" Decode.int |> Option.defaultValue defaults.MaxFilters
                        MaxLimit = get.Optional.Field "max_limit" Decode.int |> Option.defaultValue defaults.MaxLimit
                        DefaultLimit = get.Optional.Field "default_limit" Decode.int |> Option.defaultValue defaults.DefaultLimit
                        MaxSubidLength = get.Optional.Field "max_subid_length" Decode.int |> Option.defaultValue defaults.MaxSubidLength
                        MaxEventTags = get.Optional.Field "max_event_tags" Decode.int |> Option.defaultValue defaults.MaxEventTags
                        MaxContentLength = get.Optional.Field "max_content_length" Decode.int |> Option.defaultValue defaults.MaxContentLength
                        MinPowDifficulty = get.Optional.Field "min_pow_difficulty" Decode.int |> Option.defaultValue defaults.MinPowDifficulty
                        AuthRequired = get.Optional.Field "auth_required" Decode.bool |> Option.defaultValue defaults.AuthRequired
                        PaymentRequired = get.Optional.Field "payment_required" Decode.bool |> Option.defaultValue defaults.PaymentRequired
                        RestrictedWrites = get.Optional.Field "restricted_writes" Decode.bool |> Option.defaultValue defaults.RestrictedWrites
                        CreatedAtLowerLimit = get.Optional.Field "created_at_lower_limit" Decode.int
                        CreatedAtUpperLimit = get.Optional.Field "created_at_upper_limit" Decode.int
                    })

            let encode (limitation: Limitation) =
                Encode.object [
                    "max_message_length", Encode.int limitation.MaxMessageLength
                    "max_subscriptions", Encode.int limitation.MaxSubscriptions
                    "max_filters", Encode.int limitation.MaxFilters
                    "max_limit", Encode.int limitation.MaxLimit
                    "default_limit", Encode.int limitation.DefaultLimit
                    "max_subid_length", Encode.int limitation.MaxSubidLength
                    "max_event_tags", Encode.int limitation.MaxEventTags
                    "max_content_length", Encode.int limitation.MaxContentLength
                    "min_pow_difficulty", Encode.int limitation.MinPowDifficulty
                    "auth_required", Encode.bool limitation.AuthRequired
                    "payment_required", Encode.bool limitation.PaymentRequired
                    "restricted_writes", Encode.bool limitation.RestrictedWrites
                    "created_at_lower_limit", Encode.option Encode.int limitation.CreatedAtLowerLimit
                    "created_at_upper_limit", Encode.option Encode.int limitation.CreatedAtUpperLimit
                ]

        type RelayInfo = {
            Name: string
            Description: string
            Pubkey: string
            Contact: string
            SupportedNips: int list
            Software: string
            Version: string
            Limitation: Limitation
        }

        module RelayInfo =
            let defaults = {
                Name = ""
                Description = "Nostr Relay"
                Pubkey = ""
                Contact = ""
                SupportedNips = [1; 2; 4; 9; 11; 12; 16; 20; 33; 40; 45]
                Software = "https://github.com/lontivero/Nostra/"
                Version = getAssemblyVersion ()
                Limitation = Limitation.defaults
            }

            let decode : Decoder<RelayInfo> =
                Decode.object (fun get ->
                    let defaults = defaults
                    {
                        Name = get.Optional.Field "name" Decode.string |> Option.defaultValue defaults.Name
                        Description = get.Optional.Field "description" Decode.string |> Option.defaultValue defaults.Description
                        Pubkey = get.Optional.Field "pubkey" Decode.string |> Option.defaultValue defaults.Pubkey
                        Contact = get.Optional.Field "contact" Decode.string |> Option.defaultValue defaults.Contact
                        SupportedNips = get.Optional.Field "supported_nips" (Decode.list Decode.int) |> Option.defaultValue defaults.SupportedNips
                        Software = get.Optional.Field "software" Decode.string |> Option.defaultValue defaults.Software
                        Version = getAssemblyVersion ()  // Always use assembly version, ignore config
                        Limitation = get.Optional.Field "limitation" Limitation.decode |> Option.defaultValue defaults.Limitation
                    })

            let encode (info: RelayInfo) =
                Encode.object [
                    "name", Encode.string info.Name
                    "description", Encode.string info.Description
                    "pubkey", Encode.string info.Pubkey
                    "contact", Encode.string info.Contact
                    "supported_nips", Encode.list (List.map Encode.int info.SupportedNips)
                    "software", Encode.string info.Software
                    "version", Encode.string info.Version
                    "limitation", Limitation.encode info.Limitation
                ]

        let getRelayInfoDocument (info: RelayInfo) =
            RelayInfo.encode info |> Encode.toString 2

    module Configuration =
        open InfoDocument

        type LogLevel =
            | Verbose
            | Debug
            | Info
            | Warn
            | Error
            | Fatal

        module LogLevel =
            let fromString = function
                | "verbose" -> Verbose
                | "debug" -> Debug
                | "info" -> Info
                | "warn" -> Warn
                | "error" -> Error
                | "fatal" -> Fatal
                | _ -> Info

            let decode : Decoder<LogLevel> =
                Decode.string |> Decode.map (fun s -> fromString (s.ToLowerInvariant()))

        type WritePolicy = {
            Plugin: string option
            TimeoutSeconds: int
        }

        module WritePolicy =
            let defaults = {
                Plugin = None
                TimeoutSeconds = 10
            }

            let decode : Decoder<WritePolicy> =
                Decode.object (fun get ->
                    let defaults = defaults
                    {
                        Plugin = get.Optional.Field "plugin" Decode.string
                        TimeoutSeconds = get.Optional.Field "timeout_seconds" Decode.int |> Option.defaultValue defaults.TimeoutSeconds
                    })

        type RelayConfig = {
            LogLevel: LogLevel
            DatabasePath: string
            RelayInfo: RelayInfo
            WritePolicy: WritePolicy
        }

        module RelayConfig =
            let defaults = {
                LogLevel = Info
                DatabasePath = "relay.db"
                RelayInfo = RelayInfo.defaults
                WritePolicy = WritePolicy.defaults
            }

            let decode : Decoder<RelayConfig> =
                Decode.object (fun get ->
                    let defaults = defaults
                    {
                        LogLevel = get.Optional.Field "log_level" LogLevel.decode |> Option.defaultValue defaults.LogLevel
                        DatabasePath = get.Optional.Field "database_path" Decode.string |> Option.defaultValue defaults.DatabasePath
                        RelayInfo = get.Optional.Field "relay_info" RelayInfo.decode |> Option.defaultValue defaults.RelayInfo
                        WritePolicy = get.Optional.Field "write_policy" WritePolicy.decode |> Option.defaultValue defaults.WritePolicy
                    })

            let load (filePath: string) =
                if IO.File.Exists(filePath) then
                    let json = IO.File.ReadAllText(filePath)
                    match Decode.fromString decode json with
                    | Ok config -> config
                    | Result.Error _ -> defaults
                else
                    defaults