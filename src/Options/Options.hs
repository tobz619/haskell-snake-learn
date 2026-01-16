{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options.Options 
 (Options(..),
  getOpts,
  openOpts,
  setOpts,
  saveOpts,
  toggleOnline,
  online,
 )
where

import Lens.Micro.TH ( makeLenses )
import qualified Toml
import Toml.Codec ((.=), TomlDecodeError, TomlCodec)
import Data.Text (Text)
import System.IO
import qualified Data.Text.IO as TextIO
import Control.Monad (when, join)
import qualified Control.Exception as E
import qualified System.IO.Error as IOE
import Data.Functor (($>))
import Lens.Micro ((.~), (%~))

data Options = Options {_online :: Bool}
  deriving (Show)

concat <$> mapM makeLenses [
  ''Options
  ]

optionsFile :: FilePath
optionsFile = "./.options.toml"

defOptions :: Options
defOptions = Options False

optionsCodec :: TomlCodec Options
optionsCodec =
  flip Toml.table "options" $
    Options <$> Toml.bool "online" .= _online

openOpts :: IO (Either [TomlDecodeError] Options)
openOpts = do
  join $ when <$> (not <$> doesFileExist optionsFile) <*> pure (() <$ saveOpts defOptions)
  withFile optionsFile ReadWriteMode $ \h -> do
    hText <- TextIO.hGetContents h
    pure $ Toml.decodeExact optionsCodec hText

  where doesFileExist p = do
          flip IOE.catchIOError (\_ -> pure False) $ withFile p ReadMode  $ \_ -> pure True
 
getOpts :: IO Options
getOpts = either (const defOptions) id <$> openOpts

setOpts :: (Options -> Options) -> IO ()
setOpts f = (f <$> getOpts >>= saveOpts) $> ()

toggleOnline :: Options -> Options
toggleOnline = online %~ not  

saveOpts :: Options -> IO Text
saveOpts = 
  Toml.encodeToFile optionsCodec optionsFile 

