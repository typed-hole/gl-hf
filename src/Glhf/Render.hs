{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications #-}
module Glhf.Render
  ( texturedQuad
  , Painter (..)
  , mkPainter
  , texturedObj
  ) where

import           Codec.Wavefront.Element     (Element (..))
import           Codec.Wavefront.Face        (Face, faceLocIndex,
                                              faceTexCoordIndex, pattern Quad,
                                              pattern Triangle)
import           Codec.Wavefront.Location    (Location (Location))
import           Codec.Wavefront.Object      (WavefrontOBJ (..))
import           Codec.Wavefront.TexCoord    (TexCoord (..))
import           Control.Arrow               ((&&&))
import           Control.Lens.Operators      ((.~), (^.))
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Foldable               (toList)
import           Data.Function               ((&))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust)
import           Data.Word                   (Word8)
import           Glhf.ECS                    (Entity, Position (..),
                                              Renderable (..), model, texture,
                                              triangles)
import qualified Glhf.Shader                 as S
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW (Handle)

newtype Painter os = Painter
  { drawEntity :: M44 Float -> Entity -> ContextT Handle os IO ()
  }

mkPainter ::
     (S.ShaderInput os -> Render os ())
  -> Window os RGBAFloat Depth
  -> Buffer os (Uniform (V4 (B4 Float)))
  -> Map Entity Position
  -> Map Entity (Renderable os)
  -> Painter os
mkPainter shader window mvpUniform positions renderables = Painter $ \vp entity -> do
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

texturedQuad :: Entity -> Texture2D os (Format RGBAFloat) -> V2 Float -> ContextT Handle os IO (Renderable os, Position)
texturedQuad entity texture size = do
  vertices <- newBuffer @_ @(B3 Float, B2 Float) 4
  writeBuffer vertices 0
    [ (V3 0 0 0, V2 0 1)
    , (V3 1 0 0, V2 1 1)
    , (V3 0 1 0, V2 0 0)
    , (V3 1 1 0, V2 1 0)
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

texturedObj :: Entity -> Texture2D os (Format RGBAFloat) -> WavefrontOBJ -> M44 Float -> ContextT Handle os IO (Renderable os, Position)
texturedObj entity texture obj model = do
  let
    rawVerts = fmap locToV3 . toList . objLocations $ obj
    rawUvs = fmap texToV2 . toList . objTexCoords $ obj
    foldFace :: Face -> [(V3 Float, V2 Float)] -> [(V3 Float, V2 Float)]
    foldFace = \case
      Triangle a b c -> \stuff ->
        let
          getLoc = (rawVerts !!) . pred . faceLocIndex
          getUv = maybe (error "it's naht ya fawlt") ((rawUvs !!) . pred) . faceTexCoordIndex
        in
          fmap (getLoc &&& getUv) [a, b, c] <> stuff
      Quad a b c d   -> foldFace (Triangle a b c) . foldFace (Triangle a c d)
      _impossible    -> error "Neither Trinangle nor Quad"
    vertsAndUvs = foldr foldFace [] (elValue <$> objFaces obj)
  vertexBuffer <- newBuffer @_ @(B3 Float, B2 Float) $ length vertsAndUvs
  writeBuffer vertexBuffer 0 vertsAndUvs
  let
    renderable = Renderable
      { _renderEntity = entity
      , _triangles = do
        toPrimitiveArray TriangleList <$> newVertexArray vertexBuffer
      , _texture = texture
      }
    position = Position
      { _posEntity = entity
      , _model = model
      }
  pure (renderable, position)
  where
    locToV3 (Location x y z _w) = V3 x y z
    texToV2 (TexCoord u v _) = V2 u (1-v)
