{-# LANGUAGE RankNTypes #-}
module Glhf.Env
  ( GlhfEnv (..)
  , Things (..)
  , Uniforms (..)
  , width
  , height
  ) where
  
--------------------------------------------------------------------------------
import Graphics.GPipe  
--------------------------------------------------------------------------------
import Glhf.Quad (Quad, Thing)
--------------------------------------------------------------------------------

width :: Num a => a
width = 1024
height :: Num a => a
height = 768

newtype Things os = Things
  { triforce :: Thing os
  }

newtype Uniforms os = Uniforms
  { mvp :: Buffer os (Uniform (V4 (B4 Float)))
  }

data GlhfEnv os = GlhfEnv
  { fps :: forall a. Num a => a
  , things :: Things os
  , uniforms :: Uniforms os
  }
