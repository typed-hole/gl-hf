module Glhf.Physics
  ( PhysicsSystem (..)
  , mkPhysicsSystem
  ) where
import           Control.Concurrent.MVar     (MVar, modifyMVar_)
import           Control.Lens.Operators      ((%~), (.~), (^.))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Function               ((&))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Glhf.ECS                    (Entity, Position, position)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW (Handle)

newtype PhysicsSystem os = PhysicsSystem
  { runPhysics :: Entity -> ContextT Handle os IO ()
  }

mkPhysicsSystem ::
     MVar (Map Entity Position)
  -> PhysicsSystem os
mkPhysicsSystem positions = PhysicsSystem $ \entity -> do
  liftIO . modifyMVar_ positions $ flip M.alterF entity . traverse $ \pos ->
    pure $ if pos^.position._y <= 0 then
      pos & position._y .~ 0
    else
      pos & position._y %~ subtract 0.02
