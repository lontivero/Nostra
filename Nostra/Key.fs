namespace Nostra

open System
open System.Security.Cryptography
open NBitcoin.Secp256k1

module Key =
    let createNewRandom () =
        fun _ -> ECPrivKey.TryCreate(ReadOnlySpan(RandomNumberGenerator.GetBytes(32)))
        |> Seq.initInfinite
        |> Seq.skipWhile (fun (succeed, _) -> not succeed)
        |> Seq.map snd
        |> Seq.head

    let getPubKey (secret: ECPrivKey) = secret.CreateXOnlyPubKey()
