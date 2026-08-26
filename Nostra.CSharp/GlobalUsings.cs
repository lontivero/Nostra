// F# Core types (kept minimal - most usage is hidden by Interop)
global using Microsoft.FSharp.Core;

// Nostra types with C#-friendly aliases
global using Event = Nostra.EventModule;
global using SecretKey = Nostra.SecretKeyModule;
global using RelayClient = Nostra.Client.RelayConnection;
global using RelayMessage = Nostra.Client.Response.RelayMessage;
global using FilterModule = Nostra.Client.Request.Filter;
global using SubscriptionFilter = Nostra.Client.SubscriptionFilter;

// Static imports for interop utilities
global using static Nostra.CSharp.List;
global using static Nostra.CSharp.OptionExtensions;