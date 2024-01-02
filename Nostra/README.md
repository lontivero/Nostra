# Nostra

Nostra is a F# library for creating applications that interact with the [Nostr Protocol](https://github.com/nostr-protocol/nostr).

### Push a note to one relay
```f#
    let note = "Hello world!"
            |> Event.createNote
            |> Event.sign secret

    let! relay = Client.connectToRelay (Uri "wss://nostr-relay.com")
    do! relay.publish note
```

```f#
    let printEvent = function
        | Ok (RMEvent(id, event)) -> printfn "%s" event.Content
        | _ -> printf "not an event"

    let! relay = Client.connectToRelay (Uri "wss://nostr-relay.com")
    do! relay.startListening printEvent
    let filter = Filter.notes |> Filter.since (DateTime.UtcNow.AddDays -1)

    do! relay.subscribe "all" [ filter ]
```