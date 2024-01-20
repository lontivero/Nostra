namespace Nostra

open System
open Utils
open NBitcoin.Secp256k1
open Thoth.Json.Net
open System.IO
open Newtonsoft.Json

type EventId = EventId of byte[]
type AuthorId = AuthorId of ECXOnlyPubKey
type ProfileName = string
type SubscriptionId = string

type SchnorrSignature = SchnorrSignature of SecpSchnorrSignature
type SerializedEvent = string

module Author =
    let parse = function
        | Base64 64 byteArray ->
            match (ECXOnlyPubKey.TryCreate byteArray) with
            | true, author -> Ok (AuthorId author)
            | _ -> Error "The byte array is not a valid xonly publick key."
        | invalid ->
            Error $"Author is invalid. The byte array is not 32 length but %i{invalid.Length / 2}"

    let toBytes (AuthorId ecpk) =
        ecpk.ToBytes()

    let equals pk1 pk2 =
        toBytes pk1 = toBytes pk2

module SchnorrSignature =
    let parse = function
        | Base64 128 byteArray ->
            match (SecpSchnorrSignature.TryCreate byteArray) with
            | true, signature -> Ok (SchnorrSignature signature)
            | _ -> Error "The byte array is not a valid schnorr signature."
        | invalid ->
            Error $"SchnorrSignature is invalid. The byte array is not 64 length but %i{invalid.Length / 2}"

module EventId =
    let parse = function
        | Base64 64 byteArray -> Ok (EventId byteArray)
        | invalid -> Error $"EventId is invalid. The byte array is not 32 length but %i{invalid.Length / 2}"

    let toBytes (EventId eid) =
        eid

module Decode =
    let ofResult = function
        | Ok result -> Decode.succeed result
        | Error err -> Decode.fail err

    let ofOption errMsg = function
        | Some result -> Decode.succeed result
        | None -> Decode.fail errMsg

    let expect expectedValue : Decoder<string> =
        Decode.string
        |> Decode.andThen (fun value ->
           if value = expectedValue then
              Decode.succeed value
           else
              Decode.fail $"'{expectedValue}' was expected but '{value} was found instead'")

    let uri: Decoder<Uri> =
        Decode.string
        |> Decode.map Uri

    let unixDateTime: Decoder<DateTime> =
        Decode.uint32
        |> Decode.andThen (fun n ->
            Decode.succeed (fromUnixTime (int n)))

    let eventId: Decoder<EventId> =
        Decode.string
        |> Decode.map (fun x -> EventId.parse x)
        |> Decode.andThen ofResult

    let author: Decoder<AuthorId> =
        Decode.string
        |> Decode.map Author.parse
        |> Decode.andThen ofResult

    let schnorrSignature: Decoder<SchnorrSignature> =
        Decode.string
        |> Decode.map SchnorrSignature.parse
        |> Decode.andThen ofResult


module Encode =
    let uri (uri : Uri) =
        Encode.string (uri.ToString())

    let unixDateTime (date: DateTime) = Encode.uint32 (toUnixTime date)

    let eventId (EventId id) = Encode.string (toHex id)

    let author (AuthorId author) =
        Encode.string (toHex (author.ToBytes()))

    let schnorrSignature (SchnorrSignature signature) =
        Encode.string (toHex (signature.ToBytes()))

    let toCanonicalForm (token: JsonValue) : string =
        use stream = new StringWriter(NewLine = "")

        use jsonWriter =
            new JsonTextWriter(stream, Formatting = Formatting.None, Indentation = 0)

        token.WriteTo(jsonWriter)
        stream.ToString()
