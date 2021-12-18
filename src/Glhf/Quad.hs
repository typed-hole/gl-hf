{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Glhf.Quad
  ( Quad (..)
  , QuadVertex (..)
  , mkQuad
  , Thing (..)
  , mkThing
  , loadTexture
  ) where

--------------------------------------------------------------------------------
import Graphics.GPipe
import Control.Arrow (returnA)
import Graphics.GPipe.Context.GLFW qualified as GLFW
import Codec.Picture.Types (PixelRGBA8(..), imagePixels, Image (imageWidth, imageHeight))
import Data.ByteString qualified as BS
import Control.Monad.IO.Class (liftIO)
import Codec.Picture (convertRGBA8)
import Codec.Picture.Png (decodePng)
import Data.Word (Word8)
import Data.Function ((&))
import Control.Lens.Setter ((.~), (%~))
import Data.Functor ((<&>))
import Control.Lens.Fold ((^..))
--------------------------------------------------------------------------------

data Quad os = Quad
  { toPrimitives :: Render os (PrimitiveArray Triangles QuadVertex)
  , texture :: Texture2D os (Format RGBAFloat)
  , model :: M44 Float
  }

data QuadVertex = QuadVertex
  { position :: B4 Float
  , uv :: B2 Float
  }

instance BufferFormat QuadVertex where
  type HostFormat QuadVertex = HostFormat (B4 Float, B2 Float)
  toBuffer = proc ~(position, uv) -> do
    pos <- toBuffer -< position
    uv <- toBuffer -< uv
    returnA -< QuadVertex pos uv

instance VertexInput QuadVertex where
  type VertexFormat QuadVertex = VertexFormat (B4 Float, B2 Float)
  toVertex = proc ~QuadVertex {position, uv} -> do
    pos <- toVertex -< position
    uv <- toVertex -< uv
    returnA -< (pos, uv)

scaleXY :: Num a => V2 a -> M44 a
scaleXY (V2 x y) = identity
  & _m22 .~ V2
    (V2 x 0)
    (V2 0 y)

rotateZ :: Float -> M33 Float
rotateZ = fromQuaternion . axisAngle (V3 0 0 1)

mkQuad :: Texture2D os (Format RGBAFloat) -> V2 Float -> ContextT GLFW.Handle os IO (Quad os)
mkQuad texture size = do
  vertices <- newBuffer 4
  writeBuffer vertices 0
    [ (V4 0 0 0 1, V2 0 1)
    , (V4 1 0 0 1, V2 1 1)
    , (V4 0 1 0 1, V2 0 0)
    , (V4 1 1 0 1, V2 1 0)
    ]
  indices <- newBuffer @_ @(BPacked Word8) (3*2)
  writeBuffer indices 0
    [ 0, 1, 2
    , 2, 1, 3
    ]
  let
    center = identity & translation .~ negate (V3 0.5 0.5 0) :: M44 Float
    scale = scaleXY size
  pure Quad
    { texture
    , model = scale !*! center
    , toPrimitives =
      toPrimitiveArrayIndexed TriangleList
        <$> newIndexArray indices Nothing
        <*> newVertexArray vertices
    }

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

---

data Thing os = Thing
  { quad :: Quad os
  , model' :: M44 Float
  }

mkThing :: Quad os -> V3 Float -> Thing os
mkThing quad (V3 x y z) = Thing
  { quad
  , model' = model quad & translation %~ (^+^ V3 x y z)
  }
