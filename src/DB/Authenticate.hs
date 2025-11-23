{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DB.Authenticate where

import DB.Types (BSMessage)
import Data.Default.Class
import Data.Maybe (fromMaybe, listToMaybe)
import Data.X509
import Data.X509.CertificateStore (listCertificates, readCertificateStore)
import Data.X509.File
import Network.TLS
import Network.TLS.Extra
import qualified Data.ByteString as BS

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

getCertInfo :: IO (Shared, [SignedCertificate])
getCertInfo = do
  store <- fromMaybe (error "No store") <$> readCertificateStore "./.certs"
  creds <-
    either (error "Failed to get certs") Credentials . sequence
      <$> traverse
        (uncurry credentialLoadX509)
        [ ("./.certs/snake_server.crt", "./.certs/snake_server.key"),
          ("./.certs/snake_client.crt", "./.certs/snake_client.key"),
          ("./.certs/ca.crt","./.certs/ca.key")
        ]
  let caStore = listCertificates store
      sharedStuff =
        (def @Shared)
          { sharedCredentials = creds,
            sharedCAStore = store
          }
  pure (sharedStuff, caStore)

clientAuth :: BS.ByteString -> IO ClientParams
clientAuth ident = do
  (shared, _) <- getCertInfo
  chain <- readSignedObject "./.certs/snake_client.crt"
  privKey <- fromMaybe (error "No PrivKey found") . listToMaybe <$> readKeyFile "./.certs/snake_client.key"

  let clientParams =
        (defaultParamsClient "haskell-server.tobioloke.com" ident)
          { clientUseServerNameIndication = True,
            clientWantSessionResume = def,
            clientShared = shared,
            clientHooks =
              def
                { onCertificateRequest = \_ -> pure $ Just (CertificateChain chain, privKey)
                },
            clientSupported = def {supportedCiphers = ciphersuite_default, supportedVersions = [TLS13]},
            clientUseEarlyData = def
          }
  pure clientParams

serverAuth :: IO ServerParams
serverAuth = do
  (shared, certs) <- getCertInfo
  let  
    hooks = (def @ServerHooks) {
        onClientCertificate = \_chain -> pure CertificateUsageAccept
      }
    serverParams =
        (def @ServerParams)
          { serverWantClientCert = True,
            serverCACertificates = certs,
            serverShared = shared,
            serverHooks = hooks,
            serverSupported = def {supportedCiphers = ciphersuite_default, supportedVersions = [TLS13]}
          }
  pure serverParams
