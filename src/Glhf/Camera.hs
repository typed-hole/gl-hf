{-# LANGUAGE TemplateHaskell #-}

module Glhf.Camera
  ( Camera (..),
    cameraPosition,
    cameraDirection,
    fov,
    up,
    viewMatrix,
  )
where

import           Control.Lens.Getter (Getter, to, view, (^.))
import           Control.Lens.Lens   (Lens', lens)
import           Control.Lens.TH     (makeLenses)
import           Data.Function       ((&))
import           Glhf.ECS            (Entity)
import           Graphics.GPipe

data Camera = Camera
  { _cameraEntity    :: Entity,
    _cameraPosition  :: V3 Float,
    _cameraDirection :: V3 Float,
    _fov             :: Float
  }

makeLenses ''Camera

up :: Getter Camera (V3 Float)
up = to $ \cam -> cross (cam^.cameraDirection) (right cam)
  where
    right cam = cross (unit _y) (cam^.cameraDirection)

viewMatrix :: Getter Camera (M44 Float)
viewMatrix = to $ \cam -> lookAt
  (cam^.cameraPosition)
  (cam & view cameraPosition ^+^ view cameraDirection)
  (cam^.up)
