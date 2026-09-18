namespace Nostra

open System
open System.Security.Cryptography
open NBitcoin.Secp256k1

[<CompiledName("SecretKeyT")>]
type SecretKey = SecretKey of ECPrivKey

[<RequireQualifiedAccess>]
module SecretKey =
    [<CompiledName("CreateRandom")>]
    let createNewRandom () =
        fun _ -> ECPrivKey.TryCreate(ReadOnlySpan(RandomNumberGenerator.GetBytes(32)))
        |> Seq.initInfinite
        |> Seq.skipWhile (fun (succeed, _) -> not succeed)
        |> Seq.map snd
        |> Seq.head
        |> SecretKey

    [<CompiledName("FromBytes")>]
    let fromBytes (bytes: byte[]) =
        match ECPrivKey.TryCreate(ReadOnlySpan(bytes)) with
        | true, key -> Some (SecretKey key)
        | _ -> None

    [<CompiledName("FromHex")>]
    let fromHex (hex: string) =
        Utils.fromHex hex |> fromBytes

    let getPubKey (SecretKey secret) = secret.CreateXOnlyPubKey() |> AuthorId

    let sign content (SecretKey secret) = secret.SignBIP340 content