namespace Nostra

open System

module Utils =
    open System

    let toHex (bytes: byte[]) =
        bytes |> Convert.ToHexString |> (fun s -> s.ToLower())

    let fromHex (str: string) = Convert.FromHexString str

    let (|Base64|_|) (len: int) (str: string) =
        let ishexa c =
            (c >= 'a' && c <= 'f') || (c >= '0' && c <= 'f')

        match str with
        | s when s.Length = len && s.ToCharArray() |> Array.forall ishexa ->
            Some (fromHex s)
        | _ -> None

    let toUnixTime date =
        date - DateTime.UnixEpoch |> (fun t -> t.TotalSeconds) |> uint32

[<RequireQualifiedAccess>]
module Option =
    let ofTuple = function
        | true, value -> Some value
        | _ -> None

module Monad =
    type Reader<'environment, 'a> = Reader of ('environment -> 'a)

    let injectedWith environment (Reader action) =
        action environment



module ClientContext =
    type Reader<'a> = unit -> 'a
    type AsyncReader<'a> = byte[] -> Async<'a>
    type Writer<'a> = 'a -> unit
    type AsyncWriter<'a> = 'a -> Async<unit>

    type WebSocketResult = { Count: int; EndOfMessage: bool }

    type IOWebSocket =
        { read: AsyncReader<WebSocketResult>
          write: AsyncWriter<byte[]> }

    type IOLogger =
        { logInfo: Writer<string>
          logDebug: Writer<string>
          logError: Writer<string> }

    type Context =
        { WebSocket: IOWebSocket
          Logger: IOLogger }
