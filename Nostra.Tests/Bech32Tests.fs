module Bech32Tests

open System.Text
open System.Security.Cryptography
open Nostra
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

type ``Nip19 Bech32-Shareable entities``(output:ITestOutputHelper) =

    let encodeDecode entity =
        entity
        |> Shareable.encode
        |> Shareable.decode
        |> Option.get

    [<Fact>]
    let ``Encode/Decode nsec`` () =
        let secKey = SecretKey.createNewRandom()
        NSec secKey
        |> encodeDecode
        |> function
        | NSec decodedSecKey ->
            should equal decodedSecKey secKey
        | _ -> failwith "The entity is not a nsec"

    [<Fact>]
    let ``Encode/Decode npub`` () =
        let secKey = SecretKey.createNewRandom()
        let author = SecretKey.getPubKey secKey
        NPub author
        |> encodeDecode
        |> function
        | NPub decodedPubKey ->
            should equal (AuthorId.toBytes decodedPubKey) (AuthorId.toBytes author)
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
            let author = AuthorId.parse "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d" |> Result.requiresOk
            NProfile(author, [
                    "wss://r.x.com"
                    "wss://djbas.sadkb.com"
                ])

        let encodedNprofile = Shareable.encode nprofile
        should equal "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p" encodedNprofile

        let reconcodedNprofile =
            encodedNprofile
            |> Shareable.decode
            |> Option.get |> Shareable.encode
        should equal "nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p" reconcodedNprofile

    [<Fact>]
    let ``Encode/Decode nevent`` () =
       // should equal "nevent1qqsq3gvnfyk8lvn6k8v47fvyv8j2ph78754ue40qyf6xev5yrrh5jpgppamhxue69uhkummnw3ezumt0d5pzp75cf0tahv5z7plpdeaws7ex52nmnwgtwfr2g3m37r844evqrr6jqvzqqqqqqycnq9s3" encodedNevent
        NEvent(
            EventId (Utils.fromHex "08a193492c7fb27ab1d95f258461e4a0dfc7f52bccd5e022746cb28418ef4905"),
            ["wss://nostr.mom"],
            Some (AuthorId.parse "fa984bd7dbb282f07e16e7ae87b26a2a7b9b90b7246a44771f0cf5ae58018f52" |> Result.requiresOk),
            Some Kind.Text
            )
        |> encodeDecode
        |> function
            | NEvent(EventId eventId, ["wss://nostr.mom"], Some ecxOnlyPubKeyOption, Some Kind.Text) ->
                should equal (Utils.fromHex "08a193492c7fb27ab1d95f258461e4a0dfc7f52bccd5e022746cb28418ef4905") eventId
            | _ -> failwith "error"
