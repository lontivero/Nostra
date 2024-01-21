namespace Nostra

open Thoth.Json.Net

[<CompiledName("ProfileT")>]
type Profile =
    { Name : string
      About : string
      Picture : string
      Additional : (string * JsonValue) list }

module Profile =

    let additional (fieldName : string) (profile : Profile) =
        profile.Additional |> List.tryFind (fun (k, _) -> k = fieldName ) |> Option.map (fun (_, v) -> v.ToString())

    let nip05 = additional "nip05"
    let displayName = additional "display_name"
    let banner = additional "banner"
    let lud06 = additional "lud06"
    let lud16 = additional "lud16"

    module Decode =
        let profile : Decoder<Profile> =
            let commonFieldNames = ["name"; "about"; "picture"; "banner"]
            let commonFieldsDecoder = Decode.object (fun get ->
                { Name = get.Required.Field "name" Decode.string
                  About = get.Required.Field "about" Decode.string
                  Picture = get.Required.Field "picture" Decode.string
                  Additional = []
                })
            let additionalFieldsDecoder : Decoder<(string * JsonValue) list> =
                Decode.keyValuePairs Decode.value
                |> Decode.map (List.filter (fun (name, _) -> not (List.contains name commonFieldNames)))

            Decode.map2 (fun common additional -> { common with Additional =  additional })
                commonFieldsDecoder
                additionalFieldsDecoder

    module Encode =
        let profile (profile : Profile) =
            [ "name", Encode.string profile.Name
              "about", Encode.string profile.About
              "picture", Encode.string profile.Picture
            ] @ [for k, v in profile.Additional do k, v]
            |> Encode.object
