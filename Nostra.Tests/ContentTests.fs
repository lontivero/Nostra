module ContentTests

open Xunit
open Xunit.Abstractions
open FsUnit.Xunit
open Nostra

type ``Nip27 Mentions``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Parse npub reference`` () =
        "Hello nostr:npub1nccwjspr3nv7h67xx2qhdh2dzzvpyy55gte2dsu8yl7xd7n74y9qydz7mj !"
        |> Content.extractReferences
        |> should equal  [("p", ["9e30e940238cd9ebebc6328176dd4d109812129442f2a6c38727fc66fa7ea90a"])]

    [<Fact>]
    let ``Parse multiple npub references`` () =
        "Hello nostr:npub1nccwjspr3nv7h67xx2qhdh2dzzvpyy55gte2dsu8yl7xd7n74y9qydz7mj and nostr:npub14zln9kg0yx7qdn2kx8p2z9zdrz3ujfhyx6adeepmjsl98gjlfj5sr6fcn4!"
        |> Content.extractReferences
        |> should equal [
            ("p", ["9e30e940238cd9ebebc6328176dd4d109812129442f2a6c38727fc66fa7ea90a"])
            ("p", ["a8bf32d90f21bc06cd5631c2a1144d18a3c926e436badce43b943e53a25f4ca9"])
        ]

type ``Simple references``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Parse hashtag reference`` () =
        "Hello #nostr world"
        |> Content.extractReferences
        |> should equal  [("t", ["nostr"])]
