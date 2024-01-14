module Bech32Tests

open System
open System.Text
open System.Security.Cryptography
open NBitcoin.Secp256k1
open Nostra
open Nostra.Shareable
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

type ``Nip19 Bech32-Shareable entities``(output:ITestOutputHelper) =

    let encodeDecode entity =
        entity
        |> encode
        |> decode
        |> Option.get

    [<Fact>]
    let ``Encode/Decode nsec`` () =
        let secKey = Key.createNewRandom()
        NSec secKey
        |> encodeDecode
        |> function
        | NSec decodedSecKey ->
            should equal decodedSecKey secKey
        | _ -> failwith "The entity is not a nsec"

    [<Fact>]
    let ``Encode/Decode npub`` () =
        let secKey = Key.createNewRandom()
        let pubkey = XOnlyPubKey (Key.getPubKey secKey)
        NPub pubkey
        |> encodeDecode
        |> function
        | NPub decodedPubKey ->
            should equal (XOnlyPubKey.toBytes decodedPubKey) (XOnlyPubKey.toBytes pubkey)
        | _ -> failwith "The entity is not a npub"

    [<Fact>]
    let ``Encode/Decode note`` () =
        let eventId = "hello" |> Encoding.UTF8.GetBytes |> SHA256.HashData |> EventId
        Note eventId
        |> encodeDecode
        |> function
        | Note decodedEventId ->
            should equal decodedEventId eventId
        | _ -> failwith "The entity is not a note"

    [<Fact>]
    let ``Encode/Decode nprofile`` () =
        let nprofile =
            let pubkey = XOnlyPubKey.parse "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d" |> Result.requiresOk
            NProfile(pubkey, [
                    "wss://r.x.com"
                    "wss://djbas.sadkb.com"
                ])

        let encodedNprofile = encode nprofile
        should equal "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p" encodedNprofile

        let reconcodedNprofile =
            encodedNprofile
            |> decode
            |> Option.get |> encode
        should equal "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p" reconcodedNprofile

    [<Fact>]
    let ``Encode/Decode nevent`` () =
       // should equal "nevent1qqsq3gvnfyk8lvn6k8v47fvyv8j2ph78754ue40qyf6xev5yrrh5jpgppamhxue69uhkummnw3ezumt0d5pzp75cf0tahv5z7plpdeaws7ex52nmnwgtwfr2g3m37r844evqrr6jqvzqqqqqqycnq9s3" encodedNevent
        NEvent(
            EventId (Utils.fromHex "08a193492c7fb27ab1d95f258461e4a0dfc7f52bccd5e022746cb28418ef4905"),
            ["wss://nostr.mom"],
            Some (XOnlyPubKey.parse "fa984bd7dbb282f07e16e7ae87b26a2a7b9b90b7246a44771f0cf5ae58018f52" |> Result.requiresOk),
            Some Kind.Text
            )
        |> encodeDecode
        |> function
            | NEvent(EventId eventId, ["wss://nostr.mom"], Some ecxOnlyPubKeyOption, Some Kind.Text) ->
                should equal (Utils.fromHex "08a193492c7fb27ab1d95f258461e4a0dfc7f52bccd5e022746cb28418ef4905") eventId
            | _ -> failwith "error"

    [<Fact>]
    let ``Enc`` () =
        let event = encodeNote (EventId.parse "4211fc228be5af10923f56e60b1b11b8e63bf0ac7dbd3e1e3d767392fdaed4a4" |> Result.requiresOk)
        true
