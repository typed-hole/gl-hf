module Glhf.Physics
  ( runPhysics
  , PhysicsSystem (..)
  , mkPhysicsSystem
  ) where
import           Control.Concurrent.MVar     (MVar, modifyMVar_)
import           Control.Lens.Operators      ((%~), (.~), (^.))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Function               ((&))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Glhf.Camera                 (Camera, cameraPosition)
import           Glhf.ECS                    (Entity)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW (Handle)

newtype PhysicsSystem os = PhysicsSystem (Entity -> ContextT Handle os IO ())

mkPhysicsSystem ::
     MVar (Map Entity Camera)
  -> PhysicsSystem os
mkPhysicsSystem positions = PhysicsSystem $ \entity -> do
  liftIO . modifyMVar_ positions $ flip M.alterF entity . traverse $ \cam ->
    pure $ if cam^.cameraPosition._y <= 0 then
      cam & cameraPosition._y .~ 0
    else
      cam & cameraPosition._y %~ subtract 0.02

runPhysics ::
     PhysicsSystem os
  -> Entity
  -> ContextT Handle os IO ()
runPhysics (PhysicsSystem f) = f
