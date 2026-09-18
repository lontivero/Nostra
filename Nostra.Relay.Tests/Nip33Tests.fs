module Nip33Tests

open Xunit
open Nostra
open TestingFramework

type ``NIP-33 Parameterized Replaceable Events``() =

    [<Fact>]
    let ``Different d-tags are stored independently`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            event |> content "Note A v1" |> kind (enum 30000) |> created 1722337838L |> tags [Tag("d", ["note-a"])]
            event |> content "Note B v1" |> kind (enum 30000) |> created 1722337839L |> tags [Tag("d", ["note-b"])]
            event |> content "Note A v2" |> kind (enum 30000) |> created 1722337840L |> tags [Tag("d", ["note-a"])]
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Note B v1")
            ExpectEvent (withContent "abcd" "Note A v2")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Same d-tag replaces previous event`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            event |> content "Version 1" |> kind (enum 30000) |> created 1722337838L |> tags [Tag("d", ["my-note"])]
            event |> content "Version 2" |> kind (enum 30000) |> created 1722337840L |> tags [Tag("d", ["my-note"])]
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Version 2")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())
