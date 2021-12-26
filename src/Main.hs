{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
  ( main,
  )
where

--------------------------------------------------------------------------------
import           Codec.Picture                      (Image (imageHeight, imageWidth),
                                                     convertRGB8, convertRGBA8,
                                                     imagePixels)
import           Codec.Picture.Png                  (decodePng)
import           Codec.Picture.Types                (PixelRGB8 (PixelRGB8),
                                                     PixelRGBA8 (PixelRGBA8))
import           Control.Applicative                (liftA2)
import           Control.Arrow                      (Arrow ((&&&)), first,
                                                     returnA, (>>>))
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.MVar            (modifyMVar, modifyMVar_,
                                                     newMVar, readMVar)
import           Control.Lens.Combinators           (set)
import           Control.Lens.Fold                  (folded)
import           Control.Lens.Getter                (view)
import           Control.Lens.Operators             ((.~), (^.), (^..))
import           Control.Lens.Prism                 (_Just)
import           Control.Lens.Setter                (over, (%~))
import           Control.Monad                      (forever, unless, (>=>))
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import qualified Data.ByteString                    as BS
import           Data.Fixed                         (HasResolution (resolution))
import           Data.Foldable                      (for_)
import           Data.Function                      ((&))
import           Data.Functor                       ((<&>))
import qualified Data.Map.Strict                    as M
import           Data.Tuple                         (swap)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW        (CursorInputMode (..),
                                                     Handle, configHeight,
                                                     configWidth,
                                                     defaultHandleConfig,
                                                     defaultWindowConfig,
                                                     getCursorPos,
                                                     setCursorInputMode)
import           Graphics.GPipe.Context.GLFW.Input  (Key (..), KeyState (..),
                                                     getCursorPos, getKey)
import           Graphics.GPipe.Context.GLFW.Window (windowShouldClose)
import           System.CPUTime                     (getCPUTime)
--------------------------------------------------------------------------------
import           Glhf.Camera                        (Camera (..),
                                                     cameraDirection,
                                                     cameraPosition, fov, right,
                                                     up, viewMatrix)
import           Glhf.ECS                           (Component (entity),
                                                     Entity (..), KbmInput (..),
                                                     MouseHandlerInput (..),
                                                     Renderable (..),
                                                     kbInputMappings,
                                                     mouseHandler, offset,
                                                     position)
import qualified Glhf.ECS                           as ECS
import           Glhf.Env                           (Components (..),
                                                     GlhfEnv (..), cameras,
                                                     components, fps, height,
                                                     kbmInputs,
                                                     lastFrameMousePos,
                                                     positions, renderables,
                                                     uniforms, width, window)
import           Glhf.Render                        (drawEntity, mkPainter,
                                                     texturedQuad)
import           Glhf.Shader                        (ShaderInput (..),
                                                     Uniforms (..), mvp, shader)

--------------------------------------------------------------------------------

main :: IO ()
main = runContextT defaultHandleConfig $ do
  win <-
    newWindow
      (WindowFormatColorDepth RGBA8 Depth16)
      (defaultWindowConfig "glhf")
        { configWidth = width,
          configHeight = height
        }
  triforceTexture <- loadTexture "./triforce.png"
  let
    triforceA = Entity "triforceA"
    triforceB = Entity "triforceB"
  (renderTriforceA, triforcePosA) <-
    texturedQuad
      triforceA
      triforceTexture
      (V2 2365 2048 ^* (1 / 236.5))
  let
    renderTriforceB = renderTriforceA & entity .~ triforceB
    triforcePosB = triforcePosA
      & entity .~ triforceB
      & position %~ (^+^ V3 2 2 1)
  positions <-
    liftIO . newMVar . M.fromList $
      [ (triforcePosA^.entity, triforcePosA)
      , (triforcePosB^.entity, triforcePosB)
      ]
  renderables <-
    liftIO . newMVar . M.fromList $
      [ (renderTriforceA^.entity, renderTriforceA)
      , (renderTriforceB^.entity, renderTriforceB)
      ]
  let
    camera =
      Camera
        { _cameraEntity = "player"
        , _cameraPosition = V3 0 0 10
        , _cameraDirection = normalize $ V3 0.13 0.1 (-1)
        , _fov = pi / 2
        }
  cameras <-
    liftIO . newMVar . M.fromList $
      [ (camera^.entity, camera)
      ]
  setCursorInputMode win CursorInputMode'Hidden
  let
    playerInputs = KbmInput
      { _kbmInputEntity = "player"
      , _mouseHandler = \mouseState -> liftIO $ do
        let
          lookAround cam = cam
            & cameraDirection %~ rotate (axisAngle (cam^.up) (-0.01 * mouseState^.offset._x))
            & cameraDirection %~ rotate (axisAngle (cam^.right) (-0.01 * mouseState^.offset._y))
        modifyMVar_ cameras
          $ pure
          . M.alter (over _Just lookAround) "player"
      , _kbInputMappings = M.fromList
        [ ( Key'W
          , \case
              KeyState'Pressed -> liftIO $ do
                modifyMVar_ cameras $ \cams -> pure $
                  M.alter (over _Just (\cam -> cam & cameraPosition %~ (^+^ 0.1 *^ cam^.cameraDirection))) "player" cams
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'S
          , \case
              KeyState'Pressed -> liftIO $ do
                modifyMVar_ cameras $ \cams -> pure $
                  M.alter (over _Just (\cam -> cam & cameraPosition %~ (^-^ 0.1 *^ cam^.cameraDirection))) "player" cams
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'A
          , \case
              KeyState'Pressed -> liftIO $ do
                modifyMVar_ cameras $ \cams -> pure $
                  M.alter (over _Just (\cam -> cam & cameraPosition %~ (^-^ 0.1 *^ cam^.right))) "player" cams
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'D
          , \case
              KeyState'Pressed -> liftIO $ do
                modifyMVar_ cameras $ \cams -> pure $
                  M.alter (over _Just (\cam -> cam & cameraPosition %~ (^+^ 0.1*^cam^.right))) "player" cams
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        ]
      }
  inputs <-
    liftIO . newMVar . M.fromList $
      [ (playerInputs^.entity, playerInputs)
      ]
  lastMousePos <- liftIO . newMVar $ V2 (width/2) (height/2)
  mvp <- newBuffer 1
  let
    uniforms =
      Uniforms
        { _mvp = mvp
        }
    env =
      GlhfEnv
        { _components =
          Components
            { _positions = positions,
              _renderables = renderables,
              _cameras = cameras,
              _kbmInputs = inputs
            }
        , _fps = 144
        , _uniforms = uniforms
        , _window = win
        , _lastFrameMousePos = lastMousePos
        }
  shader <- compileShader $ shader uniforms

  mainLoop shader env

spin :: Float -> V3 Float -> V3 Float
spin angle = rotate (axisAngle (V3 0 1 0) angle)

mainLoop ::
  (ShaderInput os -> Render os ()) ->
  GlhfEnv os ->
  ContextT Handle os IO ()
mainLoop shader env = go
  where
    go = do
      startMicro <- (`div` 1_000_000) <$> liftIO getCPUTime
      inputs env
      renderStep shader env
      finishMicro <- (`div` 1_000_000) <$> liftIO getCPUTime
      let elapsedMicro = finishMicro - startMicro
          microsPerFrame = 1_000_000 `div` env ^. fps
          sleepTime = microsPerFrame - elapsedMicro
      liftIO $ threadDelay $ fromInteger sleepTime
      closeRequested <- windowShouldClose (env ^. window)

      unless (closeRequested == Just True) go

inputs :: GlhfEnv os -> ContextT Handle os IO ()
inputs env = do
  handlers <- liftIO . readMVar $ env^.components.kbmInputs
  for_ handlers $ \handler -> do
    for_ (M.keys (handler^.kbInputMappings)) $ \key -> runMaybeT $ do
      state <- MaybeT $ getKey (env^.window) key
      f <- MaybeT . pure $ M.lookup key (handler^.kbInputMappings)
      lift $ f state
    getCursorPos (env^.window) >>= \case
      Nothing -> pure ()
      Just (x, y) -> do
        lastMousePos <- liftIO . modifyMVar (env^.lastFrameMousePos) $ \last -> pure (V2 x y, last)
        handler^.mouseHandler $ MouseHandlerInput
          { _currentFrame = realToFrac <$> V2 x y
          , _lastFrame = realToFrac <$> lastMousePos
          }

renderStep ::
  (ShaderInput os -> Render os ()) ->
  GlhfEnv os ->
  ContextT Handle os IO ()
renderStep shader env = do
  Just camera <- liftIO $ M.lookup "player" <$> readMVar (env ^. components . cameras)
  render $ do
    clearWindowColor (env^.window) 0.1
    clearWindowDepth (env^.window) 0
  let
    projection = perspective (camera ^. fov) (width / height) 0.1 100
    view = camera^.viewMatrix
    vp = projection !*! view
    painter = mkPainter shader (env ^. window) (env ^. uniforms . mvp)
  positions <- liftIO . readMVar $ env ^. components . positions
  renderables <- liftIO . readMVar $ env ^. components . renderables

  for_ renderables $ \renderable ->
    drawEntity painter positions renderables vp (renderable^.entity)

  swapWindowBuffers $ env ^. window

loadTexture :: FilePath -> ContextT Handle os IO (Texture2D os (Format RGBAFloat))
loadTexture path = do
  liftIO $ putStrLn "Loading texture"
  (!size, !pixels) <- liftIO $ do
    putStrLn "Reading texture file"
    png <- BS.readFile path >>= either fail pure . decodePng
    let rgba8Img = convertRGBA8 png
        size = V2 (imageWidth rgba8Img) (imageHeight rgba8Img)
        pixels = rgba8Img ^.. imagePixels <&> \(PixelRGBA8 r g b a) -> V4 r g b a
    pure (size, pixels)
  liftIO $ putStrLn "Creating texture buffer"
  textureBuffer <- newTexture2D RGBA8 size 1
  liftIO $ putStrLn "Writing texture to buffer"
  writeTexture2D textureBuffer 0 0 size pixels
  liftIO $ putStrLn "Loaded texture!"
  pure textureBuffer
