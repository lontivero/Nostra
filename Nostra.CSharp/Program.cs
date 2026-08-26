namespace Nostra.CSharp;

public static class Program
{
    public static async Task Main(string[] args)
    {
        // Connect to relay using the helper class
        var relay = await Nostr.ConnectAsync("wss://relay.damus.io");

        if (args.Length > 0)
        {
            var textToPublish = args[0];
            PublishNote(relay, textToPublish);
        }

        await ListenEverything(relay);
    }

    private static void PublishNote(RelayClient relay, string noteText)
    {
        // Create, sign, and publish a note - clean and idiomatic
        var unsignedEvent = Nostr.CreateNote(noteText);
        var signedEvent = Nostr.SignEvent(Nostr.CreateRandomKey(), unsignedEvent);
        relay.Publish(signedEvent);

        // Encode as shareable nevent - no more FSharpList or Some() required
        var nevent = Shareable.ToNEvent(
            signedEvent.Id,
            relays: ["wss://relay.damus.io"],
            author: signedEvent.PubKey,
            kind: signedEvent.Kind);
        Console.WriteLine(nevent);

        // Serialize and print
        Console.WriteLine(Nostr.SerializeEvent(signedEvent));
    }

    private static async Task ListenEverything(RelayClient relay)
    {
        // Create filter using fluent API - no ToFSharpList needed
        var filter = Filter.All.Since(DateTime.UtcNow);
        relay.Subscribe("all", filter);

        // Start listening with idiomatic C# pattern matching - no FuncConvert needed
        await relay.StartListeningAsync(
            onMessage: message =>
            {
                // C#-friendly pattern matching with switch expression
                switch (message)
                {
                    case RelayMessageResult.Event evt:
                        Console.WriteLine(Nostr.SerializeEvent(evt.EventData));
                        break;

                    case RelayMessageResult.Notice notice:
                        Console.WriteLine($"Notice: {notice.Message}");
                        break;

                    case RelayMessageResult.EndOfStoredEvents eose:
                        Console.WriteLine($"End of stored events for: {eose.SubscriptionId}");
                        break;

                    case RelayMessageResult.Ack ack:
                        var status = ack.Success ? "accepted" : "rejected";
                        Console.WriteLine($"Event {status}: {ack.Message}");
                        break;
                }
            },
            onError: error => Console.WriteLine($"Error: {error}"));
    }
}