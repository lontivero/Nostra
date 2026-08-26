namespace Nostra

open System.Text.RegularExpressions

module Content =
  let private contentRegex = Regex(@"\bnostr:(?<nip27>(?:note|npub|naddr|nevent|nprofile)1\w+)\b|#(?<hashtag>\w+)\b|\b(?<reference>https?:\/\/\S+)\b", RegexOptions.Compiled)
  let private imageExtensions = [| ".jpg"; ".jpeg"; ".png"; ".gif"; ".webp" |]

  let private isImageUrl (url: string) =
      let urlLower = url.ToLowerInvariant()
      let baseUrl = match urlLower.IndexOf('?') with | -1 -> urlLower | idx -> urlLower.Substring(0, idx)
      imageExtensions |> Array.exists (fun ext -> baseUrl.EndsWith(ext))

  type ContentPart =
      | TextPart of string
      | ImagePart of string
      | UrlPart of string
      | HashtagPart of string
      | MentionPart of AuthorId * relays: string list
      | EventPart of EventId * relays: string list * author: AuthorId option

  let parseContent (content: string) : ContentPart list =
      let matches = contentRegex.Matches(content)
      if matches.Count = 0 then
          [ TextPart content ]
      else
          let parts = ResizeArray<ContentPart>()
          let mutable lastIndex = 0

          for m in matches do
              if m.Index > lastIndex then
                  let text = content.Substring(lastIndex, m.Index - lastIndex)
                  if text.Length > 0 then
                      parts.Add(TextPart text)

              let nip27Group = m.Groups["nip27"]
              let hashtagGroup = m.Groups["hashtag"]
              let referenceGroup = m.Groups["reference"]

              if nip27Group.Success then
                  match Shareable.decode nip27Group.Value with
                  | Some (NPub authorId) -> parts.Add(MentionPart(authorId, []))
                  | Some (NProfile(authorId, relays)) -> parts.Add(MentionPart(authorId, relays))
                  | Some (Note eventId) -> parts.Add(EventPart(eventId, [], None))
                  | Some (NEvent(eventId, relays, author, _)) -> parts.Add(EventPart(eventId, relays, author))
                  | _ -> parts.Add(TextPart m.Value)
              elif hashtagGroup.Success then
                  parts.Add(HashtagPart hashtagGroup.Value)
              elif referenceGroup.Success then
                  let url = referenceGroup.Value
                  if isImageUrl url then
                      parts.Add(ImagePart url)
                  else
                      parts.Add(UrlPart url)

              lastIndex <- m.Index + m.Length

          if lastIndex < content.Length then
              let text = content.Substring(lastIndex)
              if text.Length > 0 then
                  parts.Add(TextPart text)

          parts |> Seq.toList

  let contentToExtractRegex content =
      contentRegex.Matches(content)
      |> Seq.collect (_.Groups)
      |> Seq.filter (_.Success)
      |> Seq.map (fun g -> g.Name, g.Value)

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

