{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Logging.ServerLogger where

import Bluefin.Compound (Handle (mapHandle), useImplIn)
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Reader (Reader, ask, runReader)
import Bluefin.State (State, evalState, get, modify)
import Bluefin.Stream (Stream, yield)
import Brick (BrickEvent (..))
import qualified Brick.Keybindings as K
import Control.Concurrent.STM (TChan, isEmptyTChan, readTChan, tryReadTChan)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Graphics.Vty.CrossPlatform as V ()
import qualified Graphics.Vty.Input as V
import Network.Socket (SockAddr)

data Logger i o e = Logger i (Stream o e)

data ClientLog = ClientLog {clientID :: Int, clientAddr :: SockAddr, clientMsg :: Text}

instance Show ClientLog where
  show (ClientLog iD sockAddr msg) =
    unlines
      [ replicate 80 '=',
        "Client ID: " <> show iD,
        "Client Addr: " <> show sockAddr,
        if not (Text.null msg) then "Message: " <> Text.unpack msg else ""
      ]

type ClientLogger = Logger (TChan ClientLog) String

logClient :: (e :> es) => IOE e -> ClientLogger e -> Eff es ()
logClient io l@(Logger clq str) = do
  res <- effIO io . atomically $ tryReadTChan clq
  case res of
    Nothing -> pure ()
    Just cl -> do 
      yield str (show cl)
      logClient io l
    
