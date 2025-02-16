{-# LANGUAGE OverloadedStrings #-}

module DB.Server where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, swapMVar)
import Control.Exception (Exception, finally)
import Control.Monad (void)
import DB.Client
import qualified Data.Bimap as BM
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as Map
import qualified Data.Text as Text
import Data.Word (Word64)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import Logging.Replay (Seed, runReplayG)
import qualified Network.WebSockets as WS
import System.Random (StdGen, mkStdGen)
import qualified Web.Scotty as S
import Web.Scotty.Trans (scottyT)
import GameLogic (World(..), GameState (getWorld))
import Data.Maybe (fromMaybe)
import DB.Highscores (Score)

data ServerState = ServerState {clients :: ClientMap, currentIx :: CIndex}

type ClientMap = Map.IntMap Client

type CIndex = Int

data ServerStateError = MaxPlayers deriving (Show, Eq)

instance Exception ServerStateError

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 33333 $ application state

maxPlayers :: Int
maxPlayers = 1024

defaultAcceptRequest :: WS.AcceptRequest
defaultAcceptRequest = WS.defaultAcceptRequest

newServerState :: ServerState
newServerState = ServerState mempty 0

numClients :: ServerState -> Int
numClients = Map.size . clients

addClient :: Client -> ServerState -> Either ServerStateError ServerState

-- | Adds a client to the existing @ServerState@ at the current index. If the index is full, try the
-- (next one @mod@ 1024) recursively until a space is found. If there are more than maxPlayers, return an error.
addClient c s@(ServerState cs ix)
  | numClients s >= maxPlayers = Left MaxPlayers
  | otherwise =
      maybe
        (Right $ ServerState (Map.insert ix c cs) (ix + 1 `mod` maxPlayers)) -- If successful return a new state and the index the current player was inserted at
        (const $ addClient c $ s {currentIx = ix + 1 `mod` maxPlayers}) -- If a player still exists at our current index, try again at plus one
        (Map.lookup ix (clients s)) -- Find the existing index

removeClient :: CIndex -> ClientMap -> ClientMap
removeClient = Map.delete

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequestWith pending defaultAcceptRequest
  WS.withPingPong WS.defaultPingPongOptions conn $
    appHandling state

disconnect :: CIndex -> MVar ServerState -> IO ()
disconnect cix state = do
  modifyMVar_ state $ \s -> pure $ s {clients = removeClient cix (clients s)}

handleEventList :: BSMessage EventList -> EventList
handleEventList bs =
  let chunks = splitEvery 3 bs
      splitEvery n inp
        | B.null inp = []
        | otherwise = B.take n inp : splitEvery n (B.drop 3 inp)
   in map messageToGameEvent chunks
  where
    messageToGameEvent bss =
      let (ev, tick) = B.splitAt 1 bss
          convertedEv = (BM.!>) keyEvBytesMap ev
          t = fromIntegral $ B.index tick 0 * 255 + B.index tick 1
       in GameEvent (TickNumber t) convertedEv

messageToSeed :: TextMessage Word64 -> Seed
messageToSeed =  mkStdGen . fromIntegral . read . Text.unpack

messageToScore :: B.ByteString -> Score
messageToScore = maybe 0 fst . B.uncons


appHandling :: MVar ServerState -> WS.Connection -> IO ()
appHandling state conn = do
  st <- readMVar state
  cix <- case addClient (Client conn) st of
    Left MaxPlayers -> undefined -- figure out what to do if the queue of scores being uploaded is full; ideally create a queue and process asynchronously while not full
    Right newSt -> do
      void $ swapMVar state newSt
      pure (currentIx newSt)

  flip finally (disconnect cix state) $ do
    s <- messageToScore <$> WS.receiveData conn
    seed <- messageToSeed <$> WS.receiveData conn
    evList <- handleEventList <$> WS.receiveData conn
    let game = runReplayG seed evList
    if s /= (score . getWorld) game 
      then error "Mismatch!"
      else putStrLn "Valid score" -- placeholder

-- serverApp :: S.ScottyM ()
-- serverApp = do

--   -- Gets the database from the main server
--   S.get "/get-scores" $
--     S.text "viewScores"

--   -- Posts the score of the player
--   S.post "/newScore/:s" $
--     error "Not implemented"

--   -- Uploads the player's replay data
--   S.post "/uploadReplay/:content" $ do
--     error "Not implemented"