namespace Nostra.Client

open System

module StdIn =
    let readOnce prompt =
        Console.Write $"{prompt}: "
        Console.ReadLine ()

    let read prompt =
        fun _ -> readOnce prompt
        |> Seq.initInfinite
        |> Seq.skipWhile String.IsNullOrWhiteSpace
        |> Seq.head

module CliArgsParser =

    type GlobalOptions = {
        UserFile: string
        DataDir: string option
        Proxy: string option
        Secret: string option
    }

    type Command =
        | CreateUser of name: string * displayName: string option * about: string option * picture: string option * nip05: string option
        | AddRelay of urls: string list
        | RemoveRelay of urls: string list
        | SubscribeAuthor of npubs: string list
        | UnsubscribeAuthor of npubs: string list
        | SubscribeChannel of noteIds: string list
        | UnsubscribeChannel of noteIds: string list
        | Create of text: string * publish: bool
        | PublishToChannel of channel: string option * message: string option
        | Listen of sinceHoursAgo: int option * limit: int option
        | WhoAmI
        | ShowMetadata
        | ShowContacts
        | ShowPublicKey
        | ShowSecretKey
        | NpubToHex of npubs: string list
        | HexToNpub of hexes: string list

    type ParsedArgs = {
        Global: GlobalOptions
        Commands: Command list
    }

    module private Helpers =
        let tryFindValue flag (args: string list) =
            args
            |> List.pairwise
            |> List.tryFind (fun (f, _) -> f = flag)
            |> Option.map snd

        let tryFindValue2 flag1 flag2 args =
            tryFindValue flag1 args
            |> Option.orElse (tryFindValue flag2 args)

        let takeValuesAfter (args: string list) =
            args |> List.takeWhile (fun (s: string) -> not (s.StartsWith "-"))

        let hasFlag flag (args: string list) =
            args |> List.contains flag

        let skipSafe n list =
            if n >= List.length list then [] else List.skip n list

    open Helpers

    let private parseGlobal (args: string list) : GlobalOptions =
        {
            UserFile = tryFindValue2 "-u" "--user" args |> Option.defaultValue "default-user.json"
            DataDir = tryFindValue "--datadir" args
            Proxy = tryFindValue "--proxy" args
            Secret = tryFindValue "--secret" args
        }

    let private parseCommands (args: string list) : Command list =
        let rec parse (args: string list) acc =
            match args with
            | [] -> List.rev acc

            | "--create-user" :: rest ->
                let name = tryFindValue "--name" rest |> Option.defaultWith (fun () -> StdIn.read "Name")
                let displayName = tryFindValue "--display-name" rest
                let about = tryFindValue "--about" rest
                let picture = tryFindValue "--picture" rest
                let nip05 = tryFindValue "--nip05" rest
                parse rest (CreateUser(name, displayName, about, picture, nip05) :: acc)

            | "--add-relay" :: rest ->
                let urls = takeValuesAfter rest
                parse (skipSafe (List.length urls) rest) (AddRelay urls :: acc)

            | "--remove-relay" :: rest ->
                let urls = takeValuesAfter rest
                parse (skipSafe (List.length urls) rest) (RemoveRelay urls :: acc)

            | "--subscribe-author" :: rest ->
                let npubs = takeValuesAfter rest
                parse (skipSafe (List.length npubs) rest) (SubscribeAuthor npubs :: acc)

            | "--unsubscribe-author" :: rest ->
                let npubs = takeValuesAfter rest
                parse (skipSafe (List.length npubs) rest) (UnsubscribeAuthor npubs :: acc)

            | "--subscribe-channel" :: rest ->
                let noteIds = takeValuesAfter rest
                parse (skipSafe (List.length noteIds) rest) (SubscribeChannel noteIds :: acc)

            | "--unsubscribe-channel" :: rest ->
                let noteIds = takeValuesAfter rest
                parse (skipSafe (List.length noteIds) rest) (UnsubscribeChannel noteIds :: acc)

            | "--create" :: rest ->
                let text = takeValuesAfter rest |> List.tryHead |> Option.defaultWith (fun () -> StdIn.read "Note")
                let publish = hasFlag "--publish" rest || hasFlag "-p" rest
                parse rest (Create(text, publish) :: acc)

            | "--publish-to-channel" :: rest ->
                let values = takeValuesAfter rest
                let channel, message =
                    match values with
                    | [] -> None, None
                    | [c] -> Some c, None
                    | c :: m :: _ -> Some c, Some m
                parse (skipSafe (List.length values) rest) (PublishToChannel(channel, message) :: acc)

            | "--listen" :: rest ->
                let since = tryFindValue "--since-hours-ago" rest |> Option.map Int32.Parse
                let limit = tryFindValue "--limit" rest |> Option.map Int32.Parse
                parse rest (Listen(since, limit) :: acc)

            | "--whoami" :: rest ->
                parse rest (WhoAmI :: acc)

            | "--show-metadata" :: rest ->
                parse rest (ShowMetadata :: acc)

            | "--show-contacts" :: rest ->
                parse rest (ShowContacts :: acc)

            | "--show-public-key" :: rest ->
                parse rest (ShowPublicKey :: acc)

            | "--show-secret-key" :: rest ->
                parse rest (ShowSecretKey :: acc)

            | "--npub-to-hex" :: rest ->
                let npubs = takeValuesAfter rest
                parse (skipSafe (List.length npubs) rest) (NpubToHex npubs :: acc)

            | "--hex-to-npub" :: rest ->
                let hexes = takeValuesAfter rest
                parse (skipSafe (List.length hexes) rest) (HexToNpub hexes :: acc)

            | _ :: rest ->
                parse rest acc

        parse args []

    let parse (args: string[]) : ParsedArgs =
        let argList = Array.toList args
        {
            Global = parseGlobal argList
            Commands = parseCommands argList
        }