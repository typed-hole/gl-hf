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
                                                     convertRGBA8, imagePixels)
import           Codec.Picture.Png                  (decodePng)
import           Codec.Picture.Types                (PixelRGBA8 (PixelRGBA8))
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.MVar            (modifyMVar, modifyMVar_,
                                                     newMVar, readMVar)
import           Control.Lens.Operators             ((.~), (^.), (^..))
import           Control.Lens.Prism                 (_Just)
import           Control.Lens.Setter                (over, set, (%~))
import           Control.Monad                      (unless, when)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Maybe          (MaybeT (..))
import qualified Data.ByteString                    as BS
import           Data.Foldable                      (for_)
import           Data.Function                      ((&))
import           Data.Functor                       ((<&>))
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW        (CursorInputMode (..),
                                                     Handle, configHeight,
                                                     configWidth,
                                                     defaultHandleConfig,
                                                     defaultWindowConfig,
                                                     getCursorPos,
                                                     setCursorInputMode)
import           Graphics.GPipe.Context.GLFW.Input  (Key (..), KeyState (..),
                                                     getKey)
import           Graphics.GPipe.Context.GLFW.Window (windowShouldClose)
import           System.CPUTime                     (getCPUTime)
--------------------------------------------------------------------------------
import qualified Codec.Wavefront.IO                 as Obj
import           Data.Either                        (fromRight)
import           Data.Time.Clock.POSIX              (POSIXTime, getPOSIXTime)
import           Glhf.Camera                        (Camera (..),
                                                     cameraDirection, fov,
                                                     getViewMatrix,
                                                     mkCameraSystem, right, up)
import           Glhf.ECS                           (Component (entity),
                                                     Entity (..), KbmInput (..),
                                                     MouseHandlerInput (..),
                                                     Position (Position),
                                                     kbInputMappings,
                                                     mouseHandler, offset,
                                                     position)
import           Glhf.Env                           (Components (..),
                                                     GlhfEnv (..),
                                                     LoopTimes (..), cameras,
                                                     components, fps, height,
                                                     kbmInputs,
                                                     lastFrameMousePos,
                                                     lastInputs, lastPhysics,
                                                     lastRender, loopTimes,
                                                     positions, renderables,
                                                     uniforms, velocities,
                                                     width, window)
import           Glhf.Physics                       (Velocity (..),
                                                     mkPhysicsSystem,
                                                     runPhysics, velocityVector)
import           Glhf.Render                        (drawEntity, mkPainter,
                                                     texturedObj, texturedQuad)
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
  triforceTexture <- loadTexture "triforce.png"
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
      & position %~ (^+^ V3 2 2 5)
  boxObj <- liftIO $ fromRight (error "bad box") <$> Obj.fromFile "box.obj"
  boxTexture <- loadTexture "box.png"
  (renderBox, boxPos) <- texturedObj
    "box"
    boxTexture
    boxObj
    (identity & translation .~ V3 10 3 0)
  let
    playerPos = Position "player" (identity & translation .~ V3 0 0 10)
  positions <-
    liftIO . newMVar . M.fromList $
      [ (playerPos^.entity, playerPos)
      , (triforcePosA^.entity, triforcePosA)
      , (triforcePosB^.entity, triforcePosB)
      , (boxPos^.entity, boxPos)
      ]
  let
    playerVelocity = Velocity "player" (V3 0 0 0)
  velocities <- liftIO . newMVar . M.fromList $
    [ (playerVelocity^.entity, playerVelocity)
    ]
  renderables <-
    liftIO . newMVar . M.fromList $
      [ (renderTriforceA^.entity, renderTriforceA)
      , (renderTriforceB^.entity, renderTriforceB)
      , (renderBox^.entity, renderBox)
      ]
  let
    camera =
      Camera
        { _cameraEntity = "player"
        , _cameraDirection = normalize $ V3 0 0 (-1)
        , _fov = pi / 2
        }
  cameras <-
    liftIO . newMVar . M.fromList $
      [ (camera^.entity, camera)
      ]
  fromMaybe () <$> setCursorInputMode win CursorInputMode'Hidden
  let
    moveSpeed = 0.5
    playerInputs = KbmInput
      { _kbmInputEntity = "player"
      , _mouseHandler = \mouseState -> liftIO $ do
        let
          mouseSensitivity = 0.008
          lookAround cam = cam
            & cameraDirection %~ rotate (axisAngle (cam^.up) (-mouseSensitivity * mouseState^.offset._x))
            & cameraDirection %~ rotate (axisAngle (cam^.right) (-mouseSensitivity * mouseState^.offset._y))
        modifyMVar_ cameras
          $ pure
          . M.alter (over _Just lookAround) "player"
      , _kbInputMappings = M.fromList
        [ ( Key'W
          , \case
              KeyState'Pressed -> liftIO $ do
                cam <- readMVar cameras >>=
                  maybe (fail "camera not found") pure .  M.lookup "player"
                modifyMVar_ positions $
                  flip M.alterF "player" . traverse $ \pos -> do
                    let forward = normalize $ unit _y `cross` (cam^.right)
                    pure $ pos
                      & position %~ (+ moveSpeed *^ forward)
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'S
          , \case
              KeyState'Pressed -> liftIO $ do
                cam <- readMVar cameras >>=
                  maybe (fail "camera not found") pure .  M.lookup "player"
                modifyMVar_ positions $
                  flip M.alterF "player" . traverse $ \pos -> do
                    let backwards = normalize $ (cam^.right) `cross` unit _y
                    pure $ pos
                      & position %~ (+ moveSpeed *^ backwards)
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'A
          , \case
              KeyState'Pressed -> liftIO $ do
                cam <- readMVar cameras >>=
                  maybe (fail "camera not found") pure .  M.lookup "player"
                modifyMVar_ positions $
                  flip M.alterF "player" . traverse $ \pos ->
                    pure $ pos
                      & position %~ subtract (moveSpeed *^ cam^.right)
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'D
          , \case
              KeyState'Pressed -> liftIO $ do
                cam <- readMVar cameras >>=
                  maybe (fail "camera not found") pure .  M.lookup "player"
                modifyMVar_ positions $
                  flip M.alterF "player" . traverse $ \pos ->
                    pure $ pos
                      & position %~ (+ moveSpeed *^ cam^.right)
              KeyState'Released -> pure ()
              KeyState'Repeating -> pure ()
          )
        , ( Key'Space
          , \case
            KeyState'Pressed -> liftIO $ do
              modifyMVar_ velocities $
                flip M.alterF "player" . traverse $ \v ->
                  pure $ v & velocityVector._y .~ 10
            KeyState'Released -> pure ()
            KeyState'Repeating -> pure ()
          )
        , ( Key'C
          , \case
            KeyState'Pressed -> liftIO $ do
              modifyMVar_ positions $
                flip M.alterF "player" . traverse $ \pos ->
                  pure $ pos
                    & position._y %~ subtract moveSpeed
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
            { _positions = positions
            , _velocities = velocities
            , _renderables = renderables
            , _cameras = cameras
            , _kbmInputs = inputs
            }
        , _fps = 144
        , _uniforms = uniforms
        , _window = win
        , _lastFrameMousePos = lastMousePos
        , _loopTimes = LoopTimes
          { _lastRender = 0
          , _lastPhysics = 0
          , _lastInputs = 0
          }
        }
  shader <- compileShader $ shader uniforms

  mainLoop shader env

mainLoop ::
  (ShaderInput os -> Render os ()) ->
  GlhfEnv os ->
  ContextT Handle os IO ()
mainLoop shader initialEnv = do
  liftIO (newMVar initialEnv) >>= go
  where
    go envM = do
      env <- liftIO . readMVar $ envM
      time <- liftIO getPOSIXTime
      let
        timings = env^.loopTimes
        dRender = realToFrac $ time - timings^.lastRender
        dPhysics = realToFrac $ time - timings^.lastPhysics
        dInputs = realToFrac $ time - timings^.lastInputs
        inputHz = 120 :: Float
        physicsHz = 120 :: Float
        renderHz = 60 :: Float -- Infinity

      when (dInputs > 1/inputHz) $ do
        inputs env
        liftIO . modifyMVar_ envM $
          pure . set (loopTimes.lastInputs) time
      when (dPhysics > 1/physicsHz) $ do
        physicsStep env dPhysics
        liftIO . modifyMVar_ envM $
          pure . set (loopTimes.lastPhysics) time
      when (dRender > 1/renderHz) $ do
        renderStep shader env
        liftIO . modifyMVar_ envM $
          pure . set (loopTimes.lastRender) time

      closeRequested <- windowShouldClose (env ^. window)
      unless (closeRequested == Just True) $ go envM

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
  poss <- liftIO . readMVar $ env^.components.positions
  cameras <- liftIO . readMVar $ env^.components.cameras
  let
    cameraSystem = mkCameraSystem poss cameras
    Just camera = M.lookup "player" cameras
  render $ do
    clearWindowColor (env^.window) 0
    clearWindowDepth (env^.window) 1
  let
    projection = perspective (camera ^. fov) (width / height) 0.1 100
    view = getViewMatrix cameraSystem "player"
    vp = projection !*! view
  positions <- liftIO . readMVar $ env ^. components . positions
  renderables <- liftIO . readMVar $ env ^. components . renderables
  let
    painter = mkPainter shader (env^.window) (env^.uniforms.mvp) positions renderables

  for_ renderables $ \renderable ->
    drawEntity painter vp (renderable^.entity)

  swapWindowBuffers $ env ^. window

physicsStep ::
     GlhfEnv os
  -> Float
  -> ContextT Handle os IO ()
physicsStep env dt = do
  let
    physics = mkPhysicsSystem
      (env^.components.positions)
      (env^.components.velocities)
  runPhysics physics dt "player"

loadTexture :: FilePath -> ContextT Handle os IO (Texture2D os (Format RGBAFloat))
loadTexture path = do
  liftIO $ putStrLn ("Loading texture " <> path)
  (!size, !pixels) <- liftIO $ do
    putStrLn ("Reading texture file " <> path)
    png <- BS.readFile path >>= either fail pure . decodePng
    let
      rgba8Img = convertRGBA8 png
      size = V2 (imageWidth rgba8Img) (imageHeight rgba8Img)
      pixels = rgba8Img ^.. imagePixels <&> \(PixelRGBA8 r g b a) -> V4 r g b a
    pure (size, pixels)
  liftIO $ putStrLn ("Creating texture buffer " <> path)
  textureBuffer <- newTexture2D RGBA8 size maxBound
  liftIO $ putStrLn ("Writing texture to buffer " <> path)
  writeTexture2D textureBuffer 0 0 size pixels
  liftIO $ putStrLn ("Generating mipmaps " <> path)
  generateTexture2DMipmap textureBuffer
  liftIO $ putStrLn ("Loaded texture! " <> path)
  pure textureBuffer
