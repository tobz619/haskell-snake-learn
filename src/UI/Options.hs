{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Options where

import Lens.Micro

data Options = Options { _online :: Bool }
  deriving Show

concat <$> mapM_ makeLenses [''Options]

optionsApp v 