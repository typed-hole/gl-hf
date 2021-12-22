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
import           Control.Lens.Lens (Lens')
import           Control.Lens.TH   (makeLenses)
import           Graphics.GPipe
--------------------------------------------------------------------------------
import           Glhf.Quad         (Quad, Thing)
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

---

class HasPosition a where
  position :: Lens' a (V3 Float)

data Camera = Camera
  { _cameraPosition :: V3 Float
  }
makeLenses ''Camera

instance HasPosition Camera where
  position = cameraPosition
---

data GlhfEnv os = GlhfEnv
  { _fps      :: Integer
  , _things   :: Things os
  , _uniforms :: Uniforms os
  , _window   :: Window os RGBAFloat Depth
  , _camera   :: Camera
  }
makeLenses ''GlhfEnv
