{-# LANGUAGE ScopedTypeVariables #-}

module Artax.Uniform where

import Artax.Program

import Control.Monad.IO.Class
import Data.Coerce
import Data.StateVar
import Foreign
import Foreign.C.String (newCString)
import Graphics.GL.Core33
import Graphics.GL.Types
import Linear.Matrix

type UniformLocation = GLint

uniformLocation :: MonadIO m => Program -> String -> m UniformLocation
uniformLocation (Program p) s = liftIO $ do
    s' <- newCString s
    l <- glGetUniformLocation p s'
    return $ l 

uniformMatrix4fv :: UniformLocation -> SettableStateVar (M44 Float) 
uniformMatrix4fv l = SettableStateVar s 
  where
    s v = alloca $ \(ptr::Ptr (M44 Float)) -> 
            poke ptr v >> glUniformMatrix4fv (coerce l) 1 GL_FALSE (castPtr ptr) 
