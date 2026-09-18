module Nip119Tests

open Nostra.Client.Request
open Xunit
open Nostra
open TestingFramework

type ``NIP-119 AND Tag Filters``() =

    [<Fact>]
    let ``Events with multiple tags matching AND filter are returned`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish events`` [
            note "Both tags" 1722337838L |> tags [Tag("t", ["tag1"]); Tag("t", ["tag2"])]
            note "Only tag1" 1722337839L |> tags [Tag("t", ["tag1"])]
            note "Only tag2" 1722337840L |> tags [Tag("t", ["tag2"])]
        ]
        $ ``given`` Bob BobPrivateKey
        $ ``subscribe`` "abcd" [Filter.all |> Filter.notes |> Filter.tags [Tag("t", ["tag1"; "tag2"])]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Both tags")
            ExpectEvent (withContent "abcd" "Only tag1")
            ExpectEvent (withContent "abcd" "Only tag2")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())
