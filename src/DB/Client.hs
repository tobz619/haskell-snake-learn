{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module DB.Client where

import DB.Highscores (Name)
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Binary (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import GameLogic (ScoreType)
import Logging.Logger (EventList, GameEvent (..), TickNumber (..))
import qualified Network.WebSockets as WS
import UI.Gameplay (SeedSize)
import UI.Keybinds (KeyEvent (..))
import Data.List (scanl')
import Control.Monad (replicateM_)
import System.Random (mkStdGen)
import UI.ReplayPlayer (runReplayApp)
import qualified Wuss

newtype Client = Client {sConn :: WS.Connection}

type BSMessage a = ByteString

type TextMessage a = Text.Text

keyEvBytesMap :: Bimap KeyEvent ByteString
keyEvBytesMap =
  foldr
    (uncurry BM.insert)
    BM.empty
    [ (MoveUp, B.singleton 251),
      (MoveDown, B.singleton 252),
      (MoveLeft, B.singleton 253),
      (MoveRight, B.singleton 254),
      (GameEnded, B.singleton 255)
    ]

gameEvToMessage :: GameEvent -> BSMessage GameEvent
gameEvToMessage (GameEvent tn ev) = looked <> tickNoToBytes tn
  where
    looked = fromMaybe B.empty (BM.lookup ev keyEvBytesMap)
    tickNoToBytes (TickNumber tno) = encode tno

type SeedMessage = BSMessage SeedSize

sendSeedMessage :: WS.Connection -> SeedSize -> IO ()
sendSeedMessage c = WS.sendBinaryData c . seedToMessage
  where
    seedToMessage :: SeedSize -> SeedMessage
    seedToMessage = encode

type ScoreMessage = BSMessage ScoreType

sendScoreMessage :: WS.Connection -> ScoreType -> IO ()
sendScoreMessage c = WS.sendBinaryData c . scoreToMessage
  where
    scoreToMessage :: ScoreType -> ScoreMessage
    scoreToMessage = encode

type EventListMessage = BSMessage EventList

sendEventList :: WS.Connection -> EventList -> IO ()
sendEventList c = WS.sendBinaryData c . B.concat . map gameEvToMessage
-- sendEventList c = let
--  evListSize = length evlist
--  in do
    -- WS.sendBinaryData c (B.singleton $ fromIntegral evListSize)


closeConn :: WS.Connection -> IO ()
closeConn conn = WS.sendClose conn ("Closing connection" :: ByteString)

type NameMessage = TextMessage Name

sendName :: WS.Connection -> Name -> IO ()
sendName c = WS.sendTextData c . nameToMessage
  where
    nameToMessage = id

runClientApp :: SeedSize -> ScoreType -> Text.Text -> [GameEvent] -> IO ()
runClientApp seed score name evList = Wuss.runSecureClient "127.0.0.1" 34560 "/" app
  where
    app c = do
      sendScoreMessage c score
      sendName c name
      sendSeedMessage c seed
      sendEventList c evList
      closeConn c


testClient :: IO ()
testClient = let
    moves = [1,3,8,10,1,11,14,1,1]
    events = zipWith (GameEvent . TickNumber)
              (scanl' (+) 1 moves)
              [MoveRight, MoveDown, MoveLeft, MoveUp, MoveRight, MoveDown, MoveRight, MoveDown, MoveLeft]
    in do
        putStrLn $ "Sending seed: " ++  show (mkStdGen 4)
        -- evs <- newMVar events
        runClientApp 4 4 ("Max" :: Name) events
        -- runReplayApp (mkStdGen 4) evs