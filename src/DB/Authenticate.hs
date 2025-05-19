{-# LANGUAGE OverloadedStrings #-}
module DB.Authenticate where


import Data.ByteString.Lazy (ByteString)

type BSMessage a = ByteString
{-
0) TCP packets can't be parsed
That the OS job. Don't bother. Perhaps you've meant websocket frames. Then it is the websocket library author's job - don't bother either.

1) 
Yep, do a handshake. It's a good practice anyway - at least let the server tell its version and then the client bail out or do the same.
Then pin the negotiated version to the app context associated with the socket.

2) Making sure that if someone is sending stuff, it's from the game client (hello message)
Now that's outside you control. So don't bother.

3) That the client side user can't see what the hello message is (certificate pinning) so they can't then start sending random shit lol
Not sure I understand what's the intention here...

where
  dont bother = don't waste thinking cycles on it
Basically that hello message is my masterkey and that's as far as I'll take it for this project I think 
Be aware that certificate pinning is for drive-by intercepts. If an attacker has your app, then it is trivial to replace the pin, then run the app through a proxy to snoop on TLS.
That means the hello message only protects from "portscan and flood" type of attack.
An app can't protect itself from someone who owns the machine. Absolutely nothing you can put into the app that wouldn't be pwned by someone who can read it like an open book.
But a drive-by script kiddie that's gloating over your cafe wifi would do no harm.
-}



helloMessage :: BSMessage a
helloMessage = "{client:v1}"