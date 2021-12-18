{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
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
  win <- newWindow (WindowFormatColor RGBA8) (GLFW.defaultWindowConfig "glhf")
    { GLFW.configWidth = width
    , GLFW.configHeight = height
    }
  triforceTexture <- loadTexture "./triforce.png"
  triforce <- mkThing
    <$> mkQuad triforceTexture (V2 2365 2048 ^* 0.1)
    <*> pure (V3 200 200 0)
  bigTriforce <- mkThing
    <$> mkQuad triforceTexture (V2 2365 2048 ^* 0.2)
    <*> pure (V3 500 300 1)
  mvp <- newBuffer 1
  let
    uniforms = Uniforms
      { mvp
      }
    env = GlhfEnv
      { things = Things
        { triforce
        , bigTriforce
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
  -> Window os RGBAFloat ()
  -> GlhfEnv os
  -> ContextT GLFW.Handle os IO ()
loop shader win env = do
  start <- liftIO getCPUTime

  render $ do
    clearWindowColor win 0.1
  let
    projection = ortho 0 width 0 height (-1) 1
    view = lookAt (V3 0 0 1) (V3 0 0 0) (V3 0 1 0)
    vp = projection !*! view
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
  let
    bigTriforceThing = bigTriforce . things $ env
    m = model' bigTriforceThing
  writeBuffer (mvp . uniforms $ env) 0 [vp !*! m]
  render $ do
    bigTriforce <- toPrimitives . quad $ bigTriforceThing
    shader ShaderInput
      { _primitives = bigTriforce
      , _texture = texture . quad $ bigTriforceThing
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
  unless (closeRequested == Just True) $
    loop shader win env
