namespace Nostra

open System
open System.Security.Cryptography
open System.Text

module Encryption =
    let encrypt (encryptionKey: byte[]) (plainText: string) =
        let iv = RandomNumberGenerator.GetBytes(16)
        let aes = Aes.Create(Key = encryptionKey, IV = iv)
        let plainTextBytes = ReadOnlySpan(Encoding.UTF8.GetBytes(plainText))
        let cipherTextBytes = aes.EncryptCbc(plainTextBytes, iv)
        iv, cipherTextBytes

    let decrypt (decryptionKey: byte[]) (iv: byte[]) (cipherTextBytes: byte[]) =
        let aes = Aes.Create(Key = decryptionKey, IV = iv)
        aes.DecryptCbc(cipherTextBytes, iv) |> Encoding.UTF8.GetString
