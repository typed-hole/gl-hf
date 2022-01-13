{-# LANGUAGE TemplateHaskell #-}

module Glhf.Camera
  ( Camera (..)
  , cameraDirection
  , fov
  , up
  , right
  , CameraSystem(..)
  , mkCameraSystem
  )
where

import           Control.Lens.Getter (Getter, to, view, (^.))
import           Control.Lens.TH     (makeLenses)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Glhf.ECS            (Component (..), Entity, Position,
                                      position)
import           Graphics.GPipe

data Camera = Camera
  { _cameraEntity    :: Entity
  , _cameraDirection :: V3 Float
  , _fov             :: Float
  }
makeLenses ''Camera

instance Component Camera where
  entity = cameraEntity

up :: Getter Camera (V3 Float)
up = to $ \cam -> cross (cam^.cameraDirection) (right cam)
  where
    right cam = cross (unit _y) (cam^.cameraDirection)

right :: Getter Camera (V3 Float)
right = to $ \cam -> cross (cam^.cameraDirection) (cam^.up)

newtype CameraSystem = CameraSystem
  { getViewMatrix :: Entity -> M44 Float
  }

mkCameraSystem :: Map Entity Position -> Map Entity Camera -> CameraSystem
mkCameraSystem positions cameras = CameraSystem $ fromMaybe identity . \entity -> do
  pos <- M.lookup entity positions
  cam <- M.lookup entity cameras
  pure $ lookAt
    (pos^.position)
    (view position pos ^+^ view cameraDirection cam)
    (cam^.up)
