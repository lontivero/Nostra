namespace Nostra

type Tag = string * (string list)
type SingleTag = string * string

[<RequireQualifiedAccess>]
module Tag =
    open Utils

    let ungroup (tags: Tag list) : SingleTag list = tags |> List.ungroup |> List.distinct
    let group (tags: SingleTag list) : Tag list = tags |> List.groupBy fst |> List.map (fun (k,vs) -> k, vs |> List.map snd)
    let normalize (tags: Tag list) = tags |> ungroup |> group

    let findByKey key (tags : Tag list) =
        tags
        |> ungroup
        |> List.filter (fun (k,_) -> k = key)
        |> List.map snd

    let replyTag (EventId replyTo) uri = Tag("p", [ toHex replyTo; uri ])

    let authorTag (AuthorId pubkey) = Tag("p", [ toHex (pubkey.ToBytes()) ])

    let encryptedTo  = authorTag

    let eventRefTag (EventId eventId) = Tag("e", [ toHex eventId ])
    let rootEventRefTag (EventId eventId) = Tag("e", [ toHex eventId; ""; "root" ])

    let relayTag relay = Tag("r", relay)
    let relayReadTag relayUri = relayTag [ relayUri; "read" ]
    let relayWriteTag relayUri = relayTag [ relayUri; "write" ]

    open Thoth.Json.Net

    module Decode =
        let tag: Decoder<Tag> =
            Decode.list Decode.string
            |> Decode.andThen (function
                | key :: values -> Decode.succeed (Tag(key, values))
                | _ -> Decode.fail "The Tag isn't a key/value pair.")

        let tagList: Decoder<Tag list> =
            Decode.list tag

    module Encode =
        let tag (tag: Tag) =
            let key, values = tag
            Encode.list ([ Encode.string key ] @ (values |> List.map Encode.string))

        let tagList (tags : Tag list) =
            tags |> List.map tag |> Encode.list
