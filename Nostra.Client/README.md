# Nostra

# How to play with it

### Generate a new key pair:

```
$ dotnet fsi nostracli.fsx genkey

secret: supersecretkey
pubkey: dc04a357c5ef17dd9ca245b7fa24842fc227a5b86a57f5d6a36a8d4443c21014
```

### Listen for all the notes

```
$ dotnet fsi nostracli.fsx listen --relay=wss://nostr-pub.wellorder.net

---------------------------------------------------------------
Kind: Text  - Author: 668ceee55475f595ec0e8ef44c64bf9da8d9dd6008ac54dcd24b2501930b960e
Another day, and 1 BTC now costs 100 million satoshis. Are we too late, or are we still early?
---------------------------------------------------------------
Kind: Text  - Author: 3129509e23d3a6125e1451a5912dbe01099e151726c4766b44e1ecb8c846f506
Damn… some generous Nostrich just sent me my first tip!! It will be redistributed at my next ☕️ run. Thank you ⚡️ 🙏 🤙
---------------------------------------------------------------
Kind: Text  - Author: b9a1608d4ad164cb115a1d40ff36efd12b93c097cd2a3bf82a58c32534488893
For all of the new peeps just joining us, you can search for relays near you using this service: https://nostr.watch/

Quite neat 💫
---------------------------------------------------------------
Kind: Text  - Author: 6f0ec447e0da5ad4b9a3a2aef3e56b24601ca2b46ad7b23381d1941002923274
We all build during a bear market. I just build up my Bitcoin treasury and there is a purpose to it. We are all alike in some ways, i am exactly where i am supposed to be #nostr

[771925]
---------------------------------------------------------------
Kind: Text  - Author: ab6e4c12e15cbd17f976ce5b919d1032e37ddb9a57d2491aee2a80d8c4bfa76f
What if i would like to have an address on my own domain?
---------------------------------------------------------------
Kind: Text  - Author: 353781e629477a5ddb2fcf40ead51d2c049f526f4d6161cef28a3ecc75cef5ea
k well first things first, jack owes Elon an undisclosed amount of money for a bet they both made on who was gonna fuck me, but jack won't pay up cause Elon is giving me the money. and I want my money. 
---------------------------------------------------------------
Kind: Text  - Author: 67ddca50751581c703c174790588c2cd8b00f80313d0f80a5b9e73d45e48ac20
I think you need your own domain and a webserver that handles the requests. It's kinda hard to do it on a self-custody wallet.
```

### Send Encrypted messages

```
$ dotnet fsi nostracli.fsx sendmsg \
   --to=dc04a357c5ef17dd9ca245b7fa24842fc227a5b86a57f5d6a36a8d4443c21014 \
   --secret=65efca3c243e4132afbfc7e30fbc41d8d3698d26d11d816bc24a7787aa57f0dc \
   --relay=wss://nostr-pub.wellorder.net \
   --msg="yeah baby"

23f8ec3cf92d67314448844bbc987346755e5e9333cafa551ee87e45f74e9aa4
```

![image](https://user-images.githubusercontent.com/127973/212485551-803b3f6a-dee5-49ea-8ae8-9e1164921490.png)

