{-# LANGUAGE TemplateHaskell #-}
module Glhf.Physics
  ( Velocity (..)
  , velocityVector
  , PhysicsSystem (..)
  , mkPhysicsSystem
  ) where
import           Control.Concurrent.MVar     (MVar, putMVar, readMVar)
import           Control.Lens.Operators      ((%~), (^.))
import           Control.Lens.TH             (makeLenses)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Glhf.ECS                    (Component (..), Entity, Position,
                                              position)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW (Handle)

data Velocity = Velocity
  { _velocityEntity :: Entity
  , _velocityVector :: V3 Float
  }
makeLenses ''Velocity

instance Component Velocity where
  entity = velocityEntity

newtype PhysicsSystem os = PhysicsSystem
  { runPhysics :: Entity -> ContextT Handle os IO ()
  }

mkPhysicsSystem ::
     MVar (Map Entity Position)
  -> MVar (Map Entity Velocity)
  -> PhysicsSystem os
mkPhysicsSystem positions velocities = PhysicsSystem $ \entity -> liftIO $ do
  velos <- readMVar velocities
  poss <- readMVar positions
  case M.lookup entity velos of
    Nothing -> pure ()
    Just velocity ->
      putMVar positions $
        M.alter (fmap (position %~ (+ velocity^.velocityVector))) entity poss
