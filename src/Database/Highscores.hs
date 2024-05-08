{-# LANGUAGE OverloadedStrings #-}
module Database.Highscores where

import Database.SQLite.Simple
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T 

type Score = Int
data Name = Name Text Text Text 
type Time = UTCTime

data ScoreField = ScoreField Name Int Time

instance FromRow ScoreField where
  fromRow = ScoreField <$> fromRow <*> field <*> field

instance FromRow Name where
  fromRow = Name <$> field <*> field <*> field

instance ToRow ScoreField where
  toRow (ScoreField name score time) = toRow (name, score, time)

instance ToRow Name where
  toRow (Name cha1 cha2 cha3) = toRow (cha1, cha2, cha3)
