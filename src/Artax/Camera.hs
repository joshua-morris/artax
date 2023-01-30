{-# LANGUAGE TemplateHaskell #-}

module Artax.Camera
  ( Camera(..)
  , updateCamera
  ) where

import Control.Lens
import Data.Default
import Graphics.GL.Types
import Linear
import SDL

data Camera = Camera 
  { _position :: V3 GLfloat
  , _front    :: V3 GLfloat
  , _up       :: V3 GLfloat
  } deriving (Eq,Show)

makeLenses ''Camera

instance Default Camera where
  def = Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0)

updateCamera :: GLfloat -> Camera -> Event -> Camera
updateCamera s c e =
  case eventPayload e of
    KeyboardEvent keyboardEventData -> case keyboardEventKeyMotion keyboardEventData of 
       Pressed -> case keysymKeycode (keyboardEventKeysym keyboardEventData) of
          KeycodeW -> c { _position = c^.position+s*^c^.front }
          KeycodeS -> c { _position = c^.position-s*^c^.front }
          KeycodeA -> c { _position = c^.position-((c^.front) `cross` (c^.up)) } 
          KeycodeD -> c { _position = c^.position+((c^.front) `cross` (c^.up)) } 
          _        -> c  
       _ -> c
    _  -> c
