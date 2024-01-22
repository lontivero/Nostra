# C# Interop demo

This is a simple working example of how to consume Nostra from C# projects.

### Create a note, sign it and publish it
Creates a note with a text, signs it using a new randomly-generated secret key and sends it to the connected relay.
```c#
// Connect to the relay
var relay = await Client.ConnectToRelayAsync(new Uri("wss://relay.damus.io"));

// Create a note
var unsignedEvent = Event.CreateNote("Hello everybody!");

// Sign the note
var signedEvent = Event.Sign(SecretKey.CreateRandom(), unsignedEvent);

// Publish the note in the relay
Client.Publish(signedEvent, relay);
```

#### Subscribe to events
Creates a filter to match all events created from now and start listening for them.
Display the raw json for those messages received from the relay that are events.
```c#
// Connect to the relay
var relay = await Client.ConnectToRelayAsync(new Uri("wss://relay.damus.io"));

// Subscribes to all the new events.
var filter = Filter.since(DateTime.UtcNow, Filter.all);
var filters = ToFSharpList([filter]);
Client.Subscribe("all", filters, relay);

// Start listeniong for all the events.
await Client.StartListening(FuncConvert.ToFSharpFunc((NostrListenerCallback) (mresult =>
{
    var relayMessage = Result.requiresOk(mresult);
    if (relayMessage.IsRMEvent)
    {
        var (_, relayEvent) = GetEvent(relayMessage);
        Console.WriteLine(Event.Serialize(relayEvent));
    }
})), relay);
```