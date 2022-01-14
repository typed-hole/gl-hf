{-# LANGUAGE TemplateHaskell #-}
module Glhf.Physics
  ( Velocity (..)
  , velocityVector
  , PhysicsSystem (..)
  , mkPhysicsSystem
  ) where
import           Control.Concurrent.MVar     (MVar, modifyMVar_, readMVar)
import           Control.Lens.Operators      ((+~), (-~), (.~), (^.))
import           Control.Lens.TH             (makeLenses)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Function               ((&))
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
  { runPhysics :: Float -> Entity -> ContextT Handle os IO ()
  }

mkPhysicsSystem ::
     MVar (Map Entity Position)
  -> MVar (Map Entity Velocity)
  -> PhysicsSystem os
mkPhysicsSystem positions velocities = PhysicsSystem $ \dt entity -> liftIO $ do
  v0 <- (M.! entity) <$> readMVar velocities
  pos0 <- (M.! entity) <$> readMVar positions
  let
    pos1 = pos0 & position +~ dt *^ v0^.velocityVector
    (pos2, v1) =
      if pos1^.position._y <= 0 then
        (pos1 & position._y .~ 0, v0 & velocityVector._y .~ 0)
      else
        (pos1, v0 & velocityVector._y -~ 9.82*dt)
  modifyMVar_ positions $ pure . M.insert entity pos2
  modifyMVar_ velocities $ pure . M.insert entity v1
