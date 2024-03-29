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

    [<Fact>]
    let ``Parse web reference reference`` () =
        "My repo https://github.com/lontivero/Nostra"
        |> Content.extractReferences
        |> should equal  [("r", ["https://github.com/lontivero/Nostra"])]

type ``Composed references``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Parse all kind of references/mentions`` () =
        "hello #nostr look at my repo https://github.com/lontivero/Nostra nostr:nevent1qqswdshtyvpxkfelmzrmk9uk35vm0762mmt5pde600ljstswzd275hcpp4mhxue69uhkummn9ekx7mqpr4mhxue69uhkummnw3ez6ur4vgh8wetvd3hhyer9wghxuet5qgsfuv8fgq3cek0ta0rr9qtkm4x3pxqjz22y9u4xcwrj0lrxlfl2jzsrqsqqqqqpx9tsca"
        |> Content.extractReferences
        |> should equal [
           ("t", ["nostr"])
           ("r", ["https://github.com/lontivero/Nostra"])
           ("e", ["e6c2eb23026b273fd887bb17968d19b7fb4aded740b73a7bff282e0e1355ea5f"; "wss://nos.lol"; "wss://nostr-pub.wellorder.net"])]