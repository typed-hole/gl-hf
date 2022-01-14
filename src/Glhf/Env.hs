{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Glhf.Env
  ( width
  , height
  , Components (..)
  , positions
  , velocities
  , renderables
  , cameras
  , kbmInputs
  , LoopTimes (..)
  , lastRender
  , lastPhysics
  , lastInputs
  , GlhfEnv (..)
  , fps
  , components
  , uniforms
  , window
  , lastFrameMousePos
  , loopTimes
  ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.MVar (MVar)
import           Control.Lens.TH         (makeLenses)
import           Data.Map.Strict         (Map)
import           Graphics.GPipe
--------------------------------------------------------------------------------
import           Data.Time.Clock.POSIX   (POSIXTime)
import           Glhf.Camera             (Camera)
import           Glhf.ECS                (Entity, KbmInput, Position,
                                          Renderable)
import           Glhf.Physics            (Velocity)
import           Glhf.Shader             (Uniforms)
--------------------------------------------------------------------------------

width :: Num a => a
width = 1024
height :: Num a => a
height = 768

data Components os = Components
  { _positions   :: MVar (Map Entity Position)
  , _velocities  :: MVar (Map Entity Velocity)
  , _renderables :: MVar (Map Entity (Renderable os))
  , _cameras     :: MVar (Map Entity Camera)
  , _kbmInputs   :: MVar (Map Entity (KbmInput os))
  }
makeLenses ''Components

data LoopTimes = LoopTimes
  { _lastRender  :: POSIXTime
  , _lastPhysics :: POSIXTime
  , _lastInputs  :: POSIXTime
  }
makeLenses ''LoopTimes

data GlhfEnv os = GlhfEnv
  { _fps               :: Integer
  , _components        :: Components os
  , _uniforms          :: Uniforms os
  , _window            :: Window os RGBAFloat Depth
  , _lastFrameMousePos :: MVar (V2 Double)
  , _loopTimes         :: LoopTimes
  }
makeLenses ''GlhfEnv
