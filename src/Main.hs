{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores    #-}
module Main (main) where

--------------------------------------------------------------------------------
import           Codec.Picture               (convertRGB8, convertRGBA8,
                                              imagePixels)
import           Codec.Picture.Png           (decodePng)
import           Codec.Picture.Types         (PixelRGB8 (PixelRGB8),
                                              PixelRGBA8 (PixelRGBA8))
import           Control.Applicative         (liftA2)
import           Control.Arrow               (first, returnA, (>>>))
import           Control.Concurrent          (threadDelay)
import           Control.Lens.Fold           (folded)
import           Control.Lens.Getter         (view)
import           Control.Lens.Operators      ((.~), (^.), (^..))
import           Control.Monad               (forever, unless)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (MonadReader (ask), ReaderT (..))
import           Control.Monad.State         (MonadState (state))
import qualified Data.ByteString             as BS
import           Data.Fixed                  (HasResolution (resolution))
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.Tuple                  (swap)
import           Graphics.GPipe              hiding (angle)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           System.CPUTime              (getCPUTime)
--------------------------------------------------------------------------------
import           Glhf.Env                    (GlhfEnv (..), Things (..), fps,
                                              height, mvp, things, triforce,
                                              uniforms, width, window)
import           Glhf.Quad                   (Quad (..), QuadVertex (..),
                                              Thing (..), loadTexture, mkQuad,
                                              mkThing)
import           Glhf.Shader                 (ShaderInput (..), Uniforms (..),
                                              shader)
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
      { _mvp = mvp
      }
    env = GlhfEnv
      { _things = Things
        { _triforce = triforce
        }
      , _fps = 144
      , _uniforms = uniforms
      , _window = win
      }
  shader <- compileShader $ shader uniforms
  mainLoop shader env

mainLoop ::
     (ShaderInput os -> Render os ())
  -> GlhfEnv os
  -> ContextT GLFW.Handle os IO ()
mainLoop shader env = go
  where
    go = do
      startMicro <- (`div` 1_000_000) <$> liftIO getCPUTime
      renderStep shader env
      finishMicro <- (`div` 1_000_000) <$> liftIO getCPUTime
      let
        elapsedMicro = finishMicro - startMicro
        microsPerFrame = 1_000_000 `div` env ^. fps
        sleepTime = microsPerFrame - elapsedMicro
      liftIO $ threadDelay $ fromInteger sleepTime
      closeRequested <- GLFW.windowShouldClose (env ^. window)

      unless (closeRequested == Just True) go

renderStep ::
     (ShaderInput os -> Render os ())
  -> GlhfEnv os
  -> ContextT GLFW.Handle os IO ()
renderStep shader env = do
  render $ do
    clearWindowColor (env ^. window) 0.1
  let
    projection = perspective (pi/2) (width/height) 1 100
    view = lookAt cameraPos cameraDirection up
    vp = projection !*! view

  let
    triforceThing = env ^. things . triforce
    m = model' triforceThing
  writeBuffer (env ^. uniforms . mvp) 0 [vp !*! m]
  render $ do
    triforce <- toPrimitives . quad $ triforceThing
    shader ShaderInput
      { _primitives = triforce
      , _texture = texture . quad $ triforceThing
      , _window = env ^. window
      }

  swapWindowBuffers $ env ^. window

  where
    cameraPos = V3 0 0 10
    cameraDirection = cameraPos ^-^ V3 0 0 1
    up = V3 0 1 0
