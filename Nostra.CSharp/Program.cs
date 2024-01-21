namespace Nostra.CSharp;
using NostrListenerCallback = Action<FSharpResult<RelayMessage, string>>;

public static class Program
{
    public static async Task Main(string[] args)
    {
        var relay = await Client.ConnectToRelayAsync(new Uri("wss://relay.damus.io"));
        if (args.Length > 1)
        {
            var textToPublish = args[1];
            PublishNote(relay, textToPublish);
        }
        await ListenEverything(relay);
    }

    private static void PublishNote(RelayConnection relay, string noteText)
    {
        // Creates a note with the given text, sign it using a new randomly-generated
        // secret key and send it to the connected relay.
        var unsignedEvent = Event.CreateNote(noteText);
        var signedEvent = Event.Sign(SecretKey.CreateRandom(), unsignedEvent);
        Client.Publish(signedEvent, relay);

        // Encode the event as a shareable bech32 nevent event and prints it
        // in the console.
        Console.WriteLine(Shareable.ToNEvent(
            signedEvent.Id,
            ToFSharpList(["wss://relay.damus.io"]),
            Some(signedEvent.PubKey),
            Some(signedEvent.Kind)));

        // Serialized the event as SJON and prints it in the console.
        var signedEventAsJson = Event.Serialize(signedEvent);
        Console.WriteLine(signedEventAsJson);
    }

    private static async Task ListenEverything(RelayConnection relay)
    {
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
    }

    private static FSharpList<T> ToFSharpList<T>(this IEnumerable<T> seq) =>
        seq.Aggregate(FSharpList<T>.Empty, (state, e) => new FSharpList<T>(e, state));
}
