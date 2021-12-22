{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

--------------------------------------------------------------------------------
import Graphics.GPipe hiding (angle)
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Control.Monad (forever, unless)
import Data.Functor ((<&>))
import Control.Arrow (first, (>>>), returnA)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Fixed (HasResolution(resolution))
import System.CPUTime (getCPUTime)
import Control.Monad.Reader (ReaderT (..), MonadReader (ask))
import Control.Monad.State (MonadState (state))
import Data.Tuple (swap)
import Control.Lens.Operators ((^.), (^..), (.~))
import qualified Data.ByteString as BS
import Codec.Picture.Png (decodePng)
import Codec.Picture (imagePixels, convertRGB8, convertRGBA8)
import Control.Lens.Fold (folded)
import Data.Function ((&))
import Codec.Picture.Types (PixelRGB8(PixelRGB8), PixelRGBA8 (PixelRGBA8))
import Control.Lens.Getter (view)
--------------------------------------------------------------------------------
import Glhf.Shader (shader, Uniforms (..), ShaderInput (..))
import Glhf.Env (GlhfEnv (..), Things (..), width, height)
import Glhf.Quad (Quad (..), Thing (..), QuadVertex (..), mkQuad, mkThing, loadTexture)
import Control.Applicative (liftA2)
--------------------------------------------------------------------------------

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) (GLFW.defaultWindowConfig "glhf")
    { GLFW.configWidth = width
    , GLFW.configHeight = height
    }
  triforceTexture <- loadTexture "./triforce.png"
  triforce <- mkThing
    <$> mkQuad triforceTexture (V2 2365 2048 ^* (1/236.5))
    <*> pure 0
  mvp <- newBuffer 1
  let
    uniforms = Uniforms
      { mvp
      }
    env = GlhfEnv
      { things = Things
        { triforce
        }
      , fps = 144
      , uniforms = Uniforms
        { mvp
        }
      }
  shader <- compileShader $ shader uniforms
  loop shader win env

loop ::
     CompiledShader os (ShaderInput os)
  -> Window os RGBAFloat Depth
  -> GlhfEnv os
  -> ContextT GLFW.Handle os IO ()
loop shader win env = go
  where
    cameraPos = V3 0 0 10
    cameraDirection = cameraPos ^-^ V3 0 0 1
    up = V3 0 1 0
    go = do
      start <- liftIO getCPUTime

      render $ do
        clearWindowColor win 0.1
      let
        projection = perspective (pi/2) (width/height) 1 100
        view = lookAt cameraPos cameraDirection up
        vp = projection !*! view

      let
        triforceThing = triforce . things $ env
        m = model' triforceThing
      writeBuffer (mvp . uniforms $ env) 0 [vp !*! m]
      render $ do
        triforce <- toPrimitives . quad $ triforceThing
        shader ShaderInput
          { _primitives = triforce
          , _texture = texture . quad $ triforceThing
          , _window = win
          }

      swapWindowBuffers win
      finish <- liftIO getCPUTime
      let
        elapsedMicro =
          (/1_000_000)
          . fromInteger
          $ finish - start
        sleepTime = floor $ (1_000_000/fps env) - elapsedMicro
      liftIO $ threadDelay sleepTime
      closeRequested <- GLFW.windowShouldClose win

      unless (closeRequested == Just True) go
