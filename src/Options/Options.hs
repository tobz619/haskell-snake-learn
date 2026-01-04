{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Options.Options 
 (Options(..),
  getOpts,
  openOpts,
  saveOpts,
  online,
 )
where

import Lens.Micro
import Lens.Micro.TH ( makeLenses )
import qualified Toml
import qualified Toml.Parser as TomlP
import Toml.Codec ((.=), TomlDecodeError, TomlCodec)
import Data.Text (Text)
import System.IO

data Options = Options {_online :: Bool}
  deriving (Show)

concat <$> mapM makeLenses [
  ''Options
  ]

optionsFile :: FilePath
optionsFile = "./options.toml"

optionsCodec :: TomlCodec Options
optionsCodec =
  flip Toml.table "options" $
    Options <$> Toml.bool "online" .= _online

openOpts :: IO (Either [TomlDecodeError] Options)
openOpts = do
  withFile optionsFile ReadWriteMode $ \_ -> pure ()
  Toml.decodeFileEither optionsCodec optionsFile

getOpts :: IO Options
getOpts = Toml.decodeFile optionsCodec optionsFile

saveOpts :: Options -> IO Text
saveOpts = 
  Toml.encodeToFile optionsCodec optionsFile 
