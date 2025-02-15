{-# LANGUAGE OverloadedStrings #-}

module DB.Server where

import Control.Concurrent (MVar, newMVar, readMVar)
import DB.Client (Client)
import Data.IntMap.Strict as Map
import qualified Network.WebSockets as WS
import qualified Web.Scotty as S
import Web.Scotty.Trans (scottyT)

data ServerState = ServerState {clients :: ClientMap, currentIx :: CIndex}

type ClientMap = Map.IntMap Client

type CIndex = Int

data ServerStateError = MaxPlayers

main :: IO ()
main = do
  state <- newMVar newServerState
  -- WS.runServer "127.0.0.1" 33333 $ application state
  S.scotty 33333 serverApp

maxPlayers :: Int
maxPlayers = 1024

defaultAcceptRequest :: WS.AcceptRequest
defaultAcceptRequest = WS.defaultAcceptRequest

newServerState :: ServerState
newServerState = ServerState mempty 0

numClients :: ServerState -> Int
numClients = Map.size . clients

serverApp :: S.ScottyM ()
serverApp = do

  -- Gets the database from the main server
  S.get "/get-scores" $ 
    S.text "viewScores"

  -- Posts the score of the player
  S.post "/newScore/:s" $ 
    error "Not implemented"

  -- Uploads the player's replay data
  S.post "/uploadReplay/:content" $ do
    error "Not implemented"

addClient :: Client -> ServerState -> Either ServerStateError ServerState

-- | Adds a client to the existing @ServerState@ at the current index. If the index is full, try the
-- (next one @mod@ 1024) recursively until a space is found. If there are more than maxPlayers, return an error.
addClient c s@(ServerState cs ix)
  | numClients s >= maxPlayers = Left MaxPlayers
  | otherwise =
      maybe
        (Right $ ServerState (Map.insert ix c cs) (ix + 1 `mod` maxPlayers)) -- If successful return a new state
        (const $ addClient c $ s {currentIx = ix + 1 `mod` maxPlayers}) -- If a player still exists at our current index, try again at plus one
        (Map.lookup ix (clients s)) -- Find the existing index

removeClient :: CIndex -> ClientMap -> ClientMap
removeClient = Map.delete

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequestWith pending defaultAcceptRequest
  WS.withPingPong WS.defaultPingPongOptions conn $
    appHandling state

appHandling :: MVar ServerState -> WS.Connection -> IO ()
appHandling state conn = do
  clients <- readMVar state
  msg <- WS.receiveDataMessage conn
  undefined