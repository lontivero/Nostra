# Nostra

Nostra is a F# library for creating applications that interact with the [Nostr Protocol](https://github.com/nostr-protocol/nostr).

### Push a note to one relay
```f#
    let secret = ..... 
    let unsignedNote = Event.createNoteEvent "Hello world!"
    let note = Event.sign secret unsignedNote
    
    let! pushToRelay, _, _ = Client.connectToRelay (Uri("wss://nostr-relay.com")) 
    do! pushToRelay (Request.CMEvent note)
```

```f#
    let printEvent = 
        function
        | Ok (RMEvent(id, event)) -> printfn "%s" event.Content
        | _ -> printf "not an event"
        
    let! pushToRelay, receiveFromRelay, _ = Client.connectToRelay (Uri("wss://nostr-relay.com")) 
    do! receiveFromRelay printEvent
    let filter = toFilter (AllNotes(DateTime.UtcNow.AddDays(-1)))

    do! pushToRelay (Client.Request.CMSubscribe("all", [ filter ]))
```