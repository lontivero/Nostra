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
    let words (str: string) = str.Split (' ', StringSplitOptions.RemoveEmptyEntries)

    type Token =
        | User
        | AddRelay
        | RemoveRelay
        | CreateUser
        | RemoveUser
        | Name
        | DisplayName
        | About
        | Picture
        | Nip05
        | Publish
        | Create
        | PublishToChannel
        | DirectMessage
        | Listen
        | Tag
        | Alias
        | Relay
        | Proxy
        | Key
        | SubscribeAuthor
        | UnsubscribeAuthor
        | SubscribeChannel
        | UnsubscribeChannel
        | Secret
        | Value of string

    let tokenize (args : string[]) =
        [for x in args do
           yield match x with
                 | "-u" | "--user" -> User
                 | "--add-relay" -> AddRelay
                 | "--remove-relay" -> RemoveRelay
                 | "--create-user" -> CreateUser
                 | "--remove-user" -> RemoveUser
                 | "--name" -> Name
                 | "--display-name" -> DisplayName
                 | "--about" -> About
                 | "--nip05" -> Nip05
                 | "-p" | "--publish" -> Publish
                 | "--create" -> Create
                 | "--publish-to-channel" -> PublishToChannel
                 | "--dm" -> DirectMessage
                 | "--tag" -> Tag
                 | "--listen" -> Listen
                 | "--alias" -> Alias
                 | "--relay" -> Relay
                 | "--proxy" -> Proxy
                 | "--key" -> Key
                 | "--subscribe-author" -> SubscribeAuthor
                 | "--unsubscribe-author" -> UnsubscribeAuthor
                 | "--subscribe-channel" -> SubscribeChannel
                 | "--unsubscribe-channel" -> UnsubscribeChannel
                 | "--secret" -> Secret
                 | _ -> Value x]

    [<TailCall>]
    let rec _groupTokens tokens cur acc =
        match tokens with
        | [] -> acc
        | Value h::t -> _groupTokens t cur ((cur, h) :: acc)
        | h::t -> _groupTokens t h ((h, "") :: acc)

    let groupTokens tokens =
        match tokens with
        | [] -> []
        | Value t::_ -> failwith "Invalid command"
        | command::t ->
            _groupTokens t command [ (command, "") ]

    let tryGet key opts =
        opts
        |> List.filter (fun (k, _) -> k = key)
        |> List.map snd
        |> List.rev
        |> function
            | [] -> None
            | ""::rest -> Some rest
            | values -> Some values

    let parseArgs args =
        let tryGetFirst key opts = tryGet key opts |> Option.bind List.tryHead
        let orAsk prompt maybeValue  =
            maybeValue |> Option.defaultWith (fun _ -> StdIn.read prompt)

        let opts = args |> tokenize |> groupTokens
        {|
            isCreateUser = fun () -> tryGet CreateUser opts |> Option.isSome
            getName = fun () -> tryGetFirst Name opts |> orAsk "Name"
            getDisplayName = fun () -> tryGetFirst DisplayName opts
            getAbout = fun () -> tryGetFirst About opts
            getPicture = fun () -> tryGetFirst Picture opts
            getNip05 = fun () -> tryGetFirst Nip05 opts
            isAddRelay = fun () -> tryGetFirst AddRelay opts |> Option.isSome
            isRemoveRelay = fun () -> tryGetFirst RemoveRelay opts |> Option.isSome
            getRelaysToAdd = fun () -> tryGet AddRelay opts |> Option.defaultValue []
            getRelaysToRemove = fun () -> tryGet RemoveRelay opts |> Option.defaultValue []
            getProxy = fun () -> tryGetFirst Proxy opts
            isSubscribeAuthor = fun () -> tryGetFirst SubscribeAuthor opts |> Option.isSome
            isUnsubscribeAuthor = fun () -> tryGetFirst UnsubscribeAuthor opts |> Option.isSome
            getSubcribeAuthor = fun () -> tryGet SubscribeAuthor opts |> Option.defaultValue []
            getUnsubcribeAuthor = fun () -> tryGet UnsubscribeAuthor opts |> Option.defaultValue []
            isSubscribeChannel = fun () -> tryGetFirst SubscribeChannel opts |> Option.isSome
            isUnsubscribeChannel = fun () -> tryGetFirst UnsubscribeChannel opts |> Option.isSome
            getSubcribeChannel = fun () -> tryGet SubscribeChannel opts |> Option.defaultValue []
            getUnsubcribeChannel = fun () -> tryGet UnsubscribeChannel opts |> Option.defaultValue []
            isListen = fun () -> tryGet Listen opts |> Option.isSome
            isPublish = fun () -> tryGetFirst Publish opts |> Option.isSome
            isCreate = fun () -> tryGetFirst Create opts |> Option.isSome
            getNoteText = fun () -> tryGetFirst Create opts |> orAsk "Note"
            isPublishToChannel = fun () -> tryGet PublishToChannel opts |> Option.isSome
            getMessageToChannel = fun () -> tryGet PublishToChannel opts
            getUserFilePath = fun () -> tryGetFirst User opts |> Option.defaultValue "default-user.json"
            getSecret = fun () -> tryGetFirst Secret opts
        |}
