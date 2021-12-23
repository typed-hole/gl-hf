{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
module Glhf.ECS
  ( Entity (..)
  , Component (..)
  , Renderable (..)
  , triangles
  , texture
  , Position (..)
  , model
  ) where

import           Control.Lens.Lens           (Lens')
import           Control.Lens.Operators      ((.~))
import           Control.Lens.TH             (makeLenses)
import           Control.Monad               (guard, unless)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Function               ((&))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (isJust, isNothing)
import           Data.String                 (IsString)
import           Data.Word                   (Word8)
import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

class Component a where
  entity :: Lens' a Entity

newtype Entity = Entity
  { getId :: String -- TODO: UUID?
  }
  deriving (Show, Eq, Ord, IsString)

data Renderable os = Renderable
  { _renderEntity :: Entity
  , _triangles    :: Render os (PrimitiveArray Triangles (B3 Float, B2 Float))
    -- ^ (Position, UV-coordinates)
  , _texture      :: Texture2D os (Format RGBAFloat)
  }
makeLenses ''Renderable

instance Component (Renderable os) where
  entity = renderEntity

data Position = Position
  { _posEntity :: Entity
  , _model     :: M44 Float
  }
makeLenses ''Position

instance Component Position where
  entity = posEntity