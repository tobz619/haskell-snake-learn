{-# LANGUAGE TemplateHaskell #-}
module UI.HighscoreScreens where

import Brick
import Brick.BChan
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty.CrossPlatform as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import qualified Graphics.Vty as V

data LayerLoc = 
  LayerLoc { _middleLayerLocation :: T.Location
           , _bottomLayerLocation :: T.Location
           }

makeLenses ''LayerLoc 

