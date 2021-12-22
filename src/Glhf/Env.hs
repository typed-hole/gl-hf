{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Glhf.Env
{-   ( GlhfEnv (..)
  , Things (..)
  , Uniforms (..)
  , width
  , height
  ) -} where

--------------------------------------------------------------------------------
import           Control.Lens.TH (makeLenses)
import           Graphics.GPipe
--------------------------------------------------------------------------------
import           Glhf.Quad       (Quad, Thing)
--------------------------------------------------------------------------------

width :: Num a => a
width = 1024
height :: Num a => a
height = 768

data Things os = Things
  { _triforce :: Thing os
  }
makeLenses ''Things

data Uniforms os = Uniforms
  { _mvp :: Buffer os (Uniform (V4 (B4 Float)))
  }
makeLenses ''Uniforms

data GlhfEnv os = GlhfEnv
  { _fps      :: Integer
  , _things   :: Things os
  , _uniforms :: Uniforms os
  , _window   :: Window os RGBAFloat Depth
  }
makeLenses ''GlhfEnv
