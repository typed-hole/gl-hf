{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Glhf.Env
  ( width
  , height
  , Components (..)
  , positions
  , renderables
  , cameras
  , GlhfEnv (..)
  , fps
  , components
  , uniforms
  , window
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.MVar (MVar)
import           Control.Lens.Lens       (Lens')
import           Control.Lens.TH         (makeLenses)
import           Data.Map.Strict         (Map)
import           Graphics.GPipe
--------------------------------------------------------------------------------
import           Glhf.Camera             (Camera)
import           Glhf.ECS                (Entity, Position, Renderable)
import           Glhf.Shader             (Uniforms)
--------------------------------------------------------------------------------

width :: Num a => a
width = 1024
height :: Num a => a
height = 768

data Components os = Components
  { _positions   :: MVar (Map Entity Position)
  , _renderables :: MVar (Map Entity (Renderable os))
  , _cameras     :: MVar (Map Entity Camera)
  }
makeLenses ''Components

data GlhfEnv os = GlhfEnv
  { _fps        :: Integer
  , _components :: Components os
  , _uniforms   :: Uniforms os
  , _window     :: Window os RGBAFloat Depth
  }
makeLenses ''GlhfEnv
