namespace Nostra

module Content =
  let (|Nip27Mention|_|) = Regex.matches @"\bnostr:((?:note|npub|naddr|nevent|nprofile)1\w+)\b"
  let (|HashTag|_|) = Regex.matches @"#(\w+)\b"

  let extractReferences content =
      let rec parseMentions content (mentions : Tag list) =
          match content with
          | Nip27Mention (mention, endPos) ->
              Shareable.decode mention
              |> Option.map (function
                 | NPub authorId -> Tag.authorTag authorId
                 | Note eventId -> Tag.eventRefTag eventId
                 | NProfile(AuthorId authorId, relays) -> Tag("p", (Utils.toHex (authorId.ToBytes()))::relays)
                 | NEvent(EventId eventId, relays, _, _) -> Tag("e", (Utils.toHex eventId)::relays)
                 | NRelay relay -> Tag.relayTag [relay]
                 | NSec _ -> failwith "Are you crazy!?" )
              |> function
                 | Some mention -> parseMentions content[endPos..] (mention::mentions)
                 | None -> parseMentions content[endPos..] mentions
          | HashTag (hashtag, endPos) ->
              let t = Tag("t", [hashtag])
              parseMentions content[endPos..] (t::mentions)
          | _ -> mentions
      parseMentions content []
      |> List.rev

