{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Glhf.Shader
  ( Uniforms (..)
  , ShaderInput (..)
  , shader
  , mvp
  ) where

--------------------------------------------------------------------------------
import           Control.Arrow          (first)
import           Control.Lens.Getter    (view)
import           Control.Lens.Operators ((^.))
import           Control.Lens.TH        (makeLenses)
import           Data.Functor           ((<&>))
import           Graphics.GPipe
--------------------------------------------------------------------------------

data ShaderInput os = ShaderInput
  { _primitives :: PrimitiveArray Triangles (B3 Float, B2 Float)
  , _texture    :: Texture2D os (Format RGBAFloat)
  , _window     :: Window os RGBAFloat Depth
  }
makeLenses ''ShaderInput

newtype Uniforms os = Uniforms
  { _mvp :: Buffer os (Uniform (V4 (B4 Float)))
  }
makeLenses ''Uniforms

shader :: Uniforms os -> Shader os (ShaderInput os) ()
shader Uniforms {_mvp} = do
  primStream <- toPrimitiveStream $ view primitives
  mvp <- getUniform $ const (_mvp, 0)
  let
    transformedPrims = primStream
      <&> first (\(V3 x y z) -> mvp !* V4 x y z 1)
  fragStream <- flip rasterize transformedPrims $ const
    ( FrontAndBack
    , ViewPort
      { viewPortLowerLeft = V2 0 0
      , viewPortSize = V2 1024 768 -- TODO: un-hardcode
      }
    , DepthRange
      { minDepth = 0
      , maxDepth = 1
      }
    )
  sampler <- newSampler2D $ \input ->
    ( input ^. texture
    , SamplerFilter Nearest Nearest Nearest Nothing
    , (V2 ClampToBorder ClampToBorder, V4 1 0 1 1)
    )
  let
    sampledFragments = sample2D sampler SampleAuto Nothing Nothing <$> fragStream
    rgbFactors = BlendingFactors
      { blendFactorSrc = SrcAlpha
      , blendFactorDst = OneMinusSrcAlpha
      }
    alphaFactors = BlendingFactors
      { blendFactorSrc = SrcAlpha
      , blendFactorDst = OneMinusSrcAlpha
      }
  flip drawWindowColor sampledFragments $ \input ->
    ( input ^. window
    , ContextColorOption (BlendRgbAlpha (FuncAdd, FuncAdd) (rgbFactors, alphaFactors) 0) (pure True)
    )

