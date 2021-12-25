{-# LANGUAGE TypeApplications #-}
module Glhf.Render
  ( texturedQuad
  , drawEntity
  , mkPainter
  ) where

import           Control.Lens.Lens           (Lens')
import           Control.Lens.Operators      ((.~), (^.))
import           Control.Lens.TH             (makeLenses)
import           Control.Monad               (guard, unless)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Function               ((&))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust, isNothing)
import           Data.Word                   (Word8)
import           Glhf.Camera                 (Camera)
import           Glhf.ECS                    (Component (..), Entity,
                                              Position (..), Renderable (..),
                                              model, texture, triangles)
import qualified Glhf.Shader                 as S
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

newtype Painter os = Painter (Map Entity Position -> Map Entity (Renderable os) -> M44 Float -> Entity -> ContextT GLFW.Handle os IO ())

drawEntity ::
     Painter os
  -> Map Entity Position
  -> Map Entity (Renderable os)
  -> M44 Float
  -> Entity
  -> ContextT GLFW.Handle os IO ()
drawEntity (Painter f) = f

mkPainter ::
     (S.ShaderInput os -> Render os ())
  -> Window os RGBAFloat Depth
  -> Buffer os (Uniform (V4 (B4 Float)))
  -> Painter os
mkPainter shader window mvpUniform = Painter $ \positions renderables vp entity -> do
  case (M.lookup entity positions, M.lookup entity renderables) of
    (Just pos, Just renderable) -> do
      let
        mvp = vp !*! pos^.model
      writeBuffer mvpUniform 0 [mvp]
      render $ do
        primitives <- renderable^.triangles
        shader S.ShaderInput
          { S._primitives = primitives
          , S._texture = renderable^.texture
          , S._window = window
          }
    (pos, renderable) -> liftIO $ do
      unless (isJust pos) $ do
        putStrLn $ "Missing position component for Entity " <> show entity
      unless (isJust renderable) $ do
        putStrLn $ "Missing render component for Entity " <> show entity

texturedQuad :: Entity -> Texture2D os (Format RGBAFloat) -> V2 Float -> ContextT GLFW.Handle os IO (Renderable os, Position)
texturedQuad entity texture size = do
  vertices <- newBuffer @_ @(B3 Float, B2 Float) 4
  writeBuffer vertices 0
    [ (V3 0 0 2, V2 0 1)
    , (V3 1 0 2, V2 1 1)
    , (V3 0 1 2, V2 0 0)
    , (V3 1 1 2, V2 1 0)
    ]
  indices <- newBuffer @_ @(BPacked Word8) (3*2)
  writeBuffer indices 0
    [ 0, 1, 2
    , 2, 1, 3
    ]
  let
    center = identity & translation .~ negate (V3 0.5 0.5 0) :: M44 Float
    scale = scaleXY size
    renderable = Renderable
      { _renderEntity = entity
      , _texture = texture
      , _triangles =
            toPrimitiveArrayIndexed TriangleList
        <$> newIndexArray indices Nothing
        <*> newVertexArray vertices
      }
    position = Position
      { _posEntity = entity
      , _model = scale !*! center
      }
  pure (renderable, position)
  where
    scaleXY :: Num a => V2 a -> M44 a
    scaleXY (V2 x y) = identity
      & _m22 .~ V2
        (V2 x 0)
        (V2 0 y)
