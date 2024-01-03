namespace Nostra

open System
open Utils
open NBitcoin.Secp256k1
open Thoth.Json.Net
open System.IO
open Newtonsoft.Json

type EventId = EventId of byte[]
type XOnlyPubKey = XOnlyPubKey of ECXOnlyPubKey
type ProfileName = string
type SubscriptionId = string

type Uri_ = string
type SchnorrSignature = SchnorrSignature of SecpSchnorrSignature
type SerializedEvent = string

module XOnlyPubKey =
    let parse = function
        | Base64 64 byteArray ->
            match (ECXOnlyPubKey.TryCreate byteArray) with
            | true, pubkey -> Ok (XOnlyPubKey pubkey)
            | _ -> Error "The byte array is not a valid xonly publick key."
        | invalid ->
            Error $"XOnlyPubKey is invalid. The byte array is not 32 length but %i{invalid.Length / 2}"

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


module Decode =
    let ofResult = function
        | Ok result -> Decode.succeed result
        | Error err -> Decode.fail err

    let expect expectedValue : Decoder<string> =
        Decode.string
        |> Decode.andThen (fun value ->
           if value = expectedValue then
              Decode.succeed value
           else
              Decode.fail $"'{expectedValue}' was expected but '{value} was found instead'")

    let unixDateTime: Decoder<DateTime> =
        Decode.uint32
        |> Decode.andThen (fun n ->
            Decode.succeed (fromUnixTime (int n)))

    let eventId: Decoder<EventId> =
        Decode.string
        |> Decode.map EventId.parse
        |> Decode.andThen ofResult

    let xOnlyPubkey: Decoder<XOnlyPubKey> =
        Decode.string
        |> Decode.map XOnlyPubKey.parse
        |> Decode.andThen ofResult

    let schnorrSignature: Decoder<SchnorrSignature> =
        Decode.string
        |> Decode.map SchnorrSignature.parse
        |> Decode.andThen ofResult


module Encode =
    let unixDateTime (date: DateTime) = Encode.uint32 (toUnixTime date)

    let eventId (EventId id) = Encode.string (toHex id)

    let xOnlyPubkey (XOnlyPubKey pubkey) =
        Encode.string (toHex (pubkey.ToBytes()))

    let schnorrSignature (SchnorrSignature signature) =
        Encode.string (toHex (signature.ToBytes()))

    let toCanonicalForm (token: JsonValue) : string =
        use stream = new StringWriter(NewLine = "")

        use jsonWriter =
            new JsonTextWriter(stream, Formatting = Formatting.None, Indentation = 0)

        token.WriteTo(jsonWriter)
        stream.ToString()
