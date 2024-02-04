# Nostra.Client

A Command line Nostr <https://github.com/nostr-protocol/nostr> client based on the F# Nostra library.

Using Nostra.Client you can create Nostr users, publish notes, subscribe and follow posts of other
users and public channels and more.

Nostra.Client is heavily "inspired" on [nostr-commander-rs](https://github.com/8go/nostr-commander-rs)

# Build from source

## Using Nix

The easiest way to have both the relay and the client is by building the nix flake as follow:

```bash
$ nix build github:lontivero/nostra?dir=Contrib
```

# Config File

You don't need to know any of this. This is just for the curious ones.

The config file looks something like this. If you want to do some quick testing,
you can copy and paste this config file to get going real fast.

```
{
  "secret": "nsec1r4yjuyn3ppypju03rr2pzyynd0w66aaednuj2hflewd3xu0r62fqqvjxkz",
  "metadata": {
    "name": "Juan",
    "display_name": "Juan"
  },
  "relays": [
    {
      "uri": "wss://nostr-pub.wellorder.net/"
    }
  ],
  "contacts": [
    {
      "key": "npub1vxpwwydpmmc4usmxf2zkr8shdjq888g532ydyls3nadxxd9wyx5ssjq5nd",
      "metadata": {
        "name": "kristapsk",
        "picture": "https://avatars.githubusercontent.com/u/4500994",
        "about": "Bitcoin dev (Bitcoin Core, JoinMarket, SatSale, etc)\n\nPGP - 70A1 D47D D44F 59DF 8B22 2443 33E4 72FE 870C 7E5D",
        "display_name": "Kristaps K.",
        "nip05": "kristapsk@kristapsk.lv"
      }
    },
    {
      "key": "npub1nccwjspr3nv7h67xx2qhdh2dzzvpyy55gte2dsu8yl7xd7n74y9qydz7mj",
      "metadata": {
        "name": "lontivero",
        "picture": "https://nostrcheck.me/media/lontivero/avatar.webp?18",
        "about": "Bitcoin privacy warrior.",
        "display_name": "lontivero",
        "nip05": "_@lontivero.github.io"
      }
    }
  ],
  "subscribed_authors": [
    "npub1vxpwwydpmmc4usmxf2zkr8shdjq888g532ydyls3nadxxd9wyx5ssjq5nd",
    "npub1nccwjspr3nv7h67xx2qhdh2dzzvpyy55gte2dsu8yl7xd7n74y9qydz7mj"
  ],
  "subscribed_channels": []
}
```

# Example Usage

```
$ ncli --create-user --name "Juan" \
    --display-name "The greatest Juan" --about "Just a random guy for the sake of this documentation" \
    --picture "https://i.imgur.com/lKMdIps.png" \
    --nip05 juan@nostr.example.org \
    --add-relay "wss://relay.primal.net" "wss://nostr.bitcoiner.social"

$ ncli --create "Wow, that was easy!" --publish
$ ncli --subscribe-author npub1xtscya34g58tk0z605fvr788k263gsu6cy9x0mhnm87echrgufzsevkk5s
$ ncli --subscribe-channel 25e5c82273a271cb1a840d0060391a0bf4965cafeb029d5ab55350b418953fbb
```

```
$ ncli \
      --create "hello #nostr look at my repo https://github.com/lontivero/Nostra nostr:nevent1qqswdshtyvpxkfelmzrmk9uk35vm0762mmt5pde600ljstswzd275hcpp4mhxue69uhkummn9ekx7mqpr4mhxue69uhkummnw3ez6ur4vgh8wetvd3hhyer9wghxuet5qgsfuv8fgq3cek0ta0rr9qtkm4x3pxqjz22y9u4xcwrj0lrxlfl2jzsrqsqqqqqpx9tsca"
      --secret nsec1tc43eek43unn8ntcx8y3e2lkdxk66zjvq0t8zqaqvhk0jqd7w3as3wwca7"
```

```json
{
  "id": "3143fc2616eeadb0e23c149c398b2b79cb54a62bc0cc2007c4c9537e78b0beb9",
  "content": "hello #nostr look at my repo https://github.com/lontivero/Nostra nostr:nevent1qqswdshtyvpxkfelmzrmk9uk35vm0762mmt5pde600ljstswzd275hcpp4mhxue69uhkummn9ekx7mqpr4mhxue69uhkummnw3ez6ur4vgh8wetvd3hhyer9wghxuet5qgsfuv8fgq3cek0ta0rr9qtkm4x3pxqjz22y9u4xcwrj0lrxlfl2jzsrqsqqqqqpx9tsca",
  "pubkey": "c7d398301c213940d017166a58132dfc6e7cc12f5e3196458e18c47396dedfc6",
  "created_at": 1706984702,
  "kind": 1,
  "tags": [
    [ "t", "nostr" ],
    [ "r", "https://github.com/lontivero/Nostra" ],
    [ "e", "e6c2eb23026b273fd887bb17968d19b7fb4aded740b73a7bff282e0e1355ea5f", "wss://nos.lol", "wss://nostr-pub.wellorder.net" ]
  ],
  "sig": "723d2656bbf1da5798ff17341123c448678f452221a0ccdd7f013672a3d300ab43223062bb7283cff22d87c4d225b4b73744d96852948b864547c26cf9574ec9"
}
```
