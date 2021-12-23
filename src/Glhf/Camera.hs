{-# LANGUAGE TemplateHaskell #-}
module Glhf.Camera
  ( Camera (..)
  , cameraPosition
  , lookDirection
  , fov
  ) where

import           Control.Lens.TH (makeLenses)
import           Glhf.ECS        (Entity)
import           Graphics.GPipe

data Camera = Camera
  { _cameraEntity   :: Entity
  , _cameraPosition :: V3 Float
  , _lookDirection  :: V3 Float
  , _fov            :: Float
  }
makeLenses ''Camera


