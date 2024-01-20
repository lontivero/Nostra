namespace Nostra

open System
open System.Security.Cryptography
open NBitcoin.Secp256k1

type SecretKey = SecretKey of ECPrivKey

[<RequireQualifiedAccess>]
module SecretKey =
    let createNewRandom () =
        fun _ -> ECPrivKey.TryCreate(ReadOnlySpan(RandomNumberGenerator.GetBytes(32)))
        |> Seq.initInfinite
        |> Seq.skipWhile (fun (succeed, _) -> not succeed)
        |> Seq.map snd
        |> Seq.head
        |> SecretKey

    let getPubKey (SecretKey secret) = secret.CreateXOnlyPubKey() |> AuthorId

    let sign content (SecretKey secret) = secret.SignBIP340 content