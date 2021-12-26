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
  , position
  , model
  , KbmInput (..)
  , kbInputMappings
  , mouseHandler
  , MouseHandlerInput (..)
  , lastFrame
  , currentFrame
  , offset
  ) where

import           Control.Lens.Getter               (Getter, to, view)
import           Control.Lens.Lens                 (Lens')
import           Control.Lens.Operators            ((.~))
import           Control.Lens.TH                   (makeLenses)
import           Control.Monad                     (guard, unless)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Function                     ((&))
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (isJust, isNothing)
import           Data.String                       (IsString)
import           Data.Word                         (Word8)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW       (Handle)
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import           Graphics.GPipe.Context.GLFW.Input (Key, KeyState, ModifierKeys)

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

position :: Lens' Position (V3 Float)
position = model.translation

instance Component Position where
  entity = posEntity

data MouseHandlerInput = MouseHandlerInput
  { _lastFrame    :: V2 Float
  , _currentFrame :: V2 Float
  }
makeLenses ''MouseHandlerInput

offset :: Getter MouseHandlerInput (V2 Float)
offset = to $ view currentFrame ^-^ view lastFrame

data KbmInput os = KbmInput
  { _kbmInputEntity  :: Entity
  , _kbInputMappings :: Map Key (KeyState -> ContextT Handle os IO ())
  , _mouseHandler    :: MouseHandlerInput -> ContextT Handle os IO ()
  }
makeLenses ''KbmInput

instance Component (KbmInput os) where
  entity = kbmInputEntity
