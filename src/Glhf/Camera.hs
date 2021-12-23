{-# LANGUAGE TemplateHaskell #-}
module Glhf.Camera
  ( SpinDirection (..)
  , Camera (..)
  , cameraPosition
  , spinDirection
  ) where

import           Control.Lens.TH (makeLenses)
import           Glhf.Quad       (HasPosition (..))
import           Graphics.GPipe

data SpinDirection = Rest | SpinLeft | SpinRight

data Camera = Camera
  { _cameraPosition :: V3 Float
  , _spinDirection  :: SpinDirection
  }
makeLenses ''Camera

instance HasPosition Camera where
  position = cameraPosition
