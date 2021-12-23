{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main
  ( main
  ) where

--------------------------------------------------------------------------------
import           Codec.Picture               (Image (imageHeight, imageWidth),
                                              convertRGB8, convertRGBA8,
                                              imagePixels)
import           Codec.Picture.Png           (decodePng)
import           Codec.Picture.Types         (PixelRGB8 (PixelRGB8),
                                              PixelRGBA8 (PixelRGBA8))
import           Control.Applicative         (liftA2)
import           Control.Arrow               (first, returnA, (>>>))
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.MVar     (modifyMVar, modifyMVar_, newMVar,
                                              readMVar)
import           Control.Lens.Combinators    (set)
import           Control.Lens.Fold           (folded)
import           Control.Lens.Getter         (view)
import           Control.Lens.Operators      ((.~), (^.), (^..))
import           Control.Lens.Setter         (over, (%~))
import           Control.Monad               (forever, unless)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (MonadReader (ask), ReaderT (..))
import           Control.Monad.State         (MonadState (state))
import qualified Data.ByteString             as BS
import           Data.Fixed                  (HasResolution (resolution))
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import qualified Data.Map.Strict             as M
import           Data.Tuple                  (swap)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import           System.CPUTime              (getCPUTime)
--------------------------------------------------------------------------------
import           Glhf.Camera                 (Camera (..), cameraPosition, fov,
                                              lookDirection)
import           Glhf.ECS                    (Entity (..),
                                              Renderable (Renderable))
import qualified Glhf.ECS                    as ECS
import           Glhf.Env                    (Components (..), GlhfEnv (..),
                                              cameras, components, fps, height,
                                              positions, renderables, uniforms,
                                              width, window)
import           Glhf.Render                 (drawEntity, mkPainter,
                                              texturedQuad)
import           Glhf.Shader                 (ShaderInput (..), Uniforms (..),
                                              mvp, shader)
--------------------------------------------------------------------------------

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) (GLFW.defaultWindowConfig "glhf")
    { GLFW.configWidth = width
    , GLFW.configHeight = height
    }
  triforceTexture <- loadTexture "./triforce.png"
  let
    triforce = Entity "triforce"
  (renderTriforce, triforcePos) <- texturedQuad
    triforce
    triforceTexture
    (V2 2365 2048 ^* (1/236.5))

  mvp <- newBuffer 1
  positions <- liftIO . newMVar . M.fromList $
    [ (triforce, triforcePos)
    ]
  renderables <- liftIO . newMVar . M.fromList $
    [ (triforce, renderTriforce)
    ]
  let
    camera = Camera
      { _cameraEntity = "camera"
      , _cameraPosition = V3 0 0 10
      , _lookDirection = V3 0 0 (-1)
      , _fov = pi/2
      }
  cameras <- liftIO . newMVar . M.fromList $
    [ ("camera", camera)
    ]
  let
    uniforms = Uniforms
      { _mvp = mvp
      }
    env = GlhfEnv
      { _components = Components
        { _positions = positions
        , _renderables = renderables
        , _cameras = cameras
        }
      , _fps = 144
      , _uniforms = uniforms
      , _window = win
      }
  shader <- compileShader $ shader uniforms

  GLFW.setKeyCallback win . Just $ \key wat state mods -> do
    pure ()

  mainLoop shader env

spin :: Float -> V3 Float -> V3 Float
spin angle = rotate (axisAngle (V3 0 1 0) angle)

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
  Just camera <- liftIO $ M.lookup "camera" <$> readMVar (env^.components.cameras)
  render $ do
    clearWindowColor (env^.window) 0.1
  let
    projection = perspective (camera^.fov) (width/height) 1 100
    view = lookAt
      (camera^.cameraPosition)
      (camera^.cameraPosition ^+^ camera^.lookDirection)
      (V3 0 1 0)
    vp = projection !*! view
    painter = mkPainter shader (env^.window) (env^.uniforms.mvp)
  positions <- liftIO . readMVar $ env^.components.positions
  renderables <- liftIO . readMVar $ env^.components.renderables

  drawEntity painter positions renderables vp "triforce"

  swapWindowBuffers $ env ^. window

loadTexture :: FilePath -> ContextT GLFW.Handle os IO (Texture2D os (Format RGBAFloat))
loadTexture path = do
  liftIO $ putStrLn "Loading texture"
  (!size, !pixels) <- liftIO $ do
    putStrLn "Reading texture file"
    png <- BS.readFile path >>= either fail pure . decodePng
    let
      rgba8Img = convertRGBA8 png
      size = V2 (imageWidth rgba8Img) (imageHeight rgba8Img)
      pixels = rgba8Img ^.. imagePixels <&> \(PixelRGBA8 r g b a) -> V4 r g b a
    pure (size, pixels)
  liftIO $ putStrLn "Creating texture buffer"
  textureBuffer <- newTexture2D RGBA8 size 1
  liftIO $ putStrLn "Writing texture to buffer"
  writeTexture2D textureBuffer 0 0 size pixels
  liftIO $ putStrLn "Loaded texture!"
  pure textureBuffer
