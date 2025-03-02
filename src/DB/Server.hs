{-# LANGUAGE OverloadedStrings #-}

module DB.Server where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, swapMVar)
import Control.Exception (Exception, finally)
import Control.Monad (void)
import DB.Client
import DB.Highscores (Score, Name, addScore, openDatabase)
import qualified Data.Bimap as BM
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as Map
import GameLogic (GameState (getWorld), World (..), ScoreType)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import Logging.Replay (Seed, runReplayG)
import qualified Network.WebSockets as WS
import System.Random (mkStdGen)
import qualified Database.SQLite.Simple as DB
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T

data ServerState = ServerState {clients :: ClientMap, currentIx :: CIndex}

type ClientMap = Map.IntMap Client

type CIndex = Int

data ServerStateError = MaxPlayers deriving (Show, Eq)

instance Exception ServerStateError

main :: IO ()
main = do
  putStrLn "Running server on localhost:34560 ..."
  state <- newMVar newServerState
  dbConn <- openDatabase "highscores.db"
  WS.runServer "127.0.0.1" 34560 $ application state dbConn

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

application :: MVar ServerState -> DB.Connection -> WS.ServerApp
application state dbconn pending = do
  conn <- WS.acceptRequestWith pending defaultAcceptRequest
  WS.withPingPong WS.defaultPingPongOptions conn $
    appHandling state dbconn

disconnect :: CIndex -> MVar ServerState -> IO ()
disconnect cix state = do
  modifyMVar_ state $ \s -> pure $ s {clients = removeClient cix (clients s)}

handleEventList :: EventListMessage -> EventList
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

messageToSeed :: SeedMessage -> Seed
messageToSeed = mkStdGen . decode

messageToScore :: ScoreMessage -> Score
messageToScore = decode

messageToName :: NameMessage -> Name
messageToName = id

appHandling :: MVar ServerState -> DB.Connection -> WS.Connection -> IO ()
appHandling state dbConn cliConn = do
  st <- readMVar state
  cix <- case addClient (Client cliConn) st of
    Left MaxPlayers -> undefined -- figure out what to do if the queue of scores being uploaded is full; ideally create a queue and process asynchronously while not full
    Right newSt -> do
      void $ swapMVar state newSt
      pure (currentIx newSt)

  flip finally (disconnect cix state) $ do
    s <- messageToScore <$> WS.receiveData cliConn
    putStrLn $ "Score of " ++ show s ++ " received"
    name <- messageToName <$> WS.receiveData cliConn
    putStrLn $ "Name of " ++ show (T.toUpper name) ++ " received"
    seed <- messageToSeed <$> WS.receiveData cliConn
    putStrLn $ "Seed: " ++ show seed
    evList <- handleEventList <$> WS.receiveData cliConn
    putStrLn $ "First three events: " ++ show (take 3 evList)
    let game = runReplayG seed evList
        s' = (score. getWorld) game
    if s /= s'
      then do
        putStrLn "Mismatched score!"
        putStrLn $ "Expected score: " ++ show s
        putStrLn $ "Actual score: " ++ show s'
        error "Mismatch!"
      else do
        putStrLn "Valid score" -- placeholder
        time <- liftIO (round <$> getPOSIXTime)
        -- addScore dbConn name s time
        return ()

