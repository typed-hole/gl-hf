{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Glhf.Shader
  ( Uniforms (..)
  , ShaderInput (..)
  , shader
  ) where

--------------------------------------------------------------------------------
import           Control.Arrow          (first)
import           Control.Lens.Getter    (view)
import           Control.Lens.Operators ((^.))
import           Control.Lens.TH        (makeLenses)
import           Data.Functor           ((<&>))
import           Graphics.GPipe
--------------------------------------------------------------------------------
import           Glhf.Env               (GlhfEnv (..), Things (..),
                                         Uniforms (..), height, width)
import           Glhf.Quad              (Quad, QuadVertex)
--------------------------------------------------------------------------------

data ShaderInput os = ShaderInput
  { _primitives :: PrimitiveArray Triangles QuadVertex
  , _texture    :: Texture2D os (Format RGBAFloat)
  , _window     :: Window os RGBAFloat Depth
  }
makeLenses ''ShaderInput

shader :: Uniforms os -> Shader os (ShaderInput os) ()
shader Uniforms {_mvp} = do
  primStream <- toPrimitiveStream $ view primitives
  mvp <- getUniform $ const (_mvp, 0)
  let
    transformedPrims = primStream <&> first (mvp !*)
  fragStream <- flip rasterize transformedPrims $ const
    ( Front
    , ViewPort
      { viewPortLowerLeft = V2 0 0
      , viewPortSize = V2 width height
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

