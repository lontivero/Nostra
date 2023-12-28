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
type Tag = string * (string list)
type SerializedEvent = string

module Decode =
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
        |> Decode.andThen (function
            | Base64 64 byteArray -> Decode.succeed (EventId byteArray)
            | invalid -> Decode.fail $"EventId is invalid. The byte array is not 32 length but %i{invalid.Length / 2}")

    let xOnlyPubkey: Decoder<XOnlyPubKey> =
        Decode.string
        |> Decode.andThen (function
            | Base64 64 byteArray ->
                match (ECXOnlyPubKey.TryCreate byteArray) with
                | true, pubkey -> Decode.succeed (XOnlyPubKey pubkey)
                | _ -> Decode.fail "The byte array is not a valid xonly publick key."
            | invalid ->
                Decode.fail $"XOnlyPubKey is invalid. The byte array is not 32 length but %i{invalid.Length / 2}")

    let schnorrSignature: Decoder<SchnorrSignature> =
        Decode.string
        |> Decode.andThen (function
            | Base64 128 byteArray ->
                match (SecpSchnorrSignature.TryCreate byteArray) with
                | true, signature -> Decode.succeed (SchnorrSignature signature)
                | _ -> Decode.fail "The byte array is not a valid schnorr signature."
            | invalid ->
                Decode.fail $"SchnorrSignature is invalid. The byte array is not 64 length but %i{invalid.Length / 2}")

    let tag: Decoder<Tag> =
        Decode.list Decode.string
        |> Decode.andThen (function
            | key :: values -> Decode.succeed (Tag(key, values))
            | _ -> Decode.fail "The Tag isn't a key/value pair.")

module Encode =
    let unixDateTime (date: DateTime) = Encode.uint32 (toUnixTime date)

    let eventId (EventId id) = Encode.string (toHex id)

    let xOnlyPubkey (XOnlyPubKey pubkey) =
        Encode.string (toHex (pubkey.ToBytes()))

    let schnorrSignature (SchnorrSignature signature) =
        Encode.string (toHex (signature.ToBytes()))

    let tag (tag: Tag) =
        let key, values = tag
        Encode.list ([ Encode.string key ] @ (values |> List.map Encode.string))

    let toCanonicalForm (token: JsonValue) : string =
        use stream = new StringWriter(NewLine = "")

        use jsonWriter =
            new JsonTextWriter(stream, Formatting = Formatting.None, Indentation = 0)

        token.WriteTo(jsonWriter)
        stream.ToString()
