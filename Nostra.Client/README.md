# Nostra.Client

A Command line Nostr <https://github.com/nostr-protocol/nostr> client based on the F# Nostra library.

Using Nostra.Client you can create Nostr users, publish notes, subscribe and follow posts of other
users and public channels and more.

Nostra.Client is heavily "inspired" on [nostr-commander-rs](https://github.com/8go/nostr-commander-rs)

# Build from source


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
$ Nostra.Client --create-user --name "Juan" \
    --display-name "The greatest Juan" --about "Just a random guy for the sake of this documentation" \
    --picture "https://i.imgur.com/lKMdIps.png" \
    --nip05 juan@nostr.example.org \
    --add-relay "wss://relay.primal.net" "wss://nostr.bitcoiner.social"

$ Nostra.Client --publish "Wow, that was easy!"
$ Nostra.Client --subscribe-author npub1xtscya34g58tk0z605fvr788k263gsu6cy9x0mhnm87echrgufzsevkk5s
$ Nostra.Client --subscribe-channel 25e5c82273a271cb1a840d0060391a0bf4965cafeb029d5ab55350b418953fbb
```

# Usage

```
```
