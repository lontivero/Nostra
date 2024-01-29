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

    let fromUnixTime (unixDateTime : int) =
        (int64 unixDateTime * TimeSpan.TicksPerSecond + DateTime.UnixEpoch.Ticks |> DateTime)

    let toBE (v : int) =
        v |> BitConverter.GetBytes |> Array.rev

    let fromBE (vs : byte[]) =
        vs |> Array.rev |> BitConverter.ToInt32

[<RequireQualifiedAccess>]
module Option =
    let ofTuple = function
        | true, value -> Some value
        | _ -> None

    let ofChoice = function
        | Choice1Of2 v -> Some v
        | _ -> None

[<RequireQualifiedAccess>]
module List =
    let ungroup lst =
        List.collect (fun (k, vs) -> List.map (fun v -> k, v) vs) lst

    let lift lst =
        if List.contains None lst then
            None
        else
            Some (List.map Option.get lst)

    let addUniques items list =
        items
        |> List.append list
        |> List.distinct

[<RequireQualifiedAccess>]
module Result =
    let requiresOk = function
        | Ok v -> v
        | Error e -> failwith e

    let ofChoice = function
        | Choice1Of2 v -> Ok v
        | Choice2Of2 e -> Error e

[<RequireQualifiedAccess>]
module Regex =
    open System.Text.RegularExpressions
    let matches pattern input =
        let m = Regex.Match(input, pattern, RegexOptions.Multiline ||| RegexOptions.Compiled ||| RegexOptions.CultureInvariant)
        if m.Success then
            Some(m.Groups[1].Value, m.Index + m.Length)
        else
            None

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
