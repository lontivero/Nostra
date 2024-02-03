namespace Nostra

module Content =
  let contentToExtractRegex = Regex.matches  @"\bnostr:(?<nip27>(?:note|npub|naddr|nevent|nprofile)1\w+)\b|#(?<hashtag>\w+)\b|\b(?<reference>https?:\/\/\S+)\b"

  let extractReferences content =
      let parseMentions = function
          | "nip27", mention ->
              Shareable.decode mention
              |> Option.map (function
                 | NPub authorId -> Tag.authorTag authorId
                 | Note eventId -> Tag.eventRefTag eventId
                 | NProfile(AuthorId authorId, relays) -> Tag("p", (Utils.toHex (authorId.ToBytes()))::relays)
                 | NEvent(EventId eventId, relays, _, _) -> Tag("e", (Utils.toHex eventId)::relays)
                 | NRelay relay -> Tag.relayTag [relay]
                 | NSec _ -> failwith "Are you crazy!?" )
          | "reference", reference ->
              Some (Tag("r", [reference]))
          | "hashtag", hashtag ->
              Some (Tag("t", [hashtag]))
          | _ -> None

      (contentToExtractRegex content, [])
      ||> Seq.foldBack (fun g acc -> (parseMentions g)::acc)
      |> Seq.choose id
      |> Seq.toList

