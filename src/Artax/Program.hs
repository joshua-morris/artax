module Artax.Program
  ( Program(..)
  , createProgram
  ) where

import Artax.Shader

import Control.Monad
import Control.Monad.IO.Class
import Graphics.GL.Core33
import Graphics.GL.Types

newtype Program = Program GLuint

newProgram :: MonadIO m => m Program
newProgram = liftM Program glCreateProgram

linkProgram :: MonadIO m => Program -> m ()
linkProgram (Program program) = glLinkProgram program

useProgram :: MonadIO m => Program -> m ()
useProgram (Program program) = glUseProgram program

attachShader :: MonadIO m => Program -> Shader -> m ()
attachShader (Program program) (Shader shader) = glAttachShader program shader

-- | @createProgram vertexShader fragmentShader@ creates and links a shader program
createProgram :: Shader -> Shader -> IO Program
createProgram vertexShader fragmentShader = do
  shaderProgram <- newProgram 
  attachShader shaderProgram vertexShader
  attachShader shaderProgram fragmentShader
  linkProgram shaderProgram
  deleteShader vertexShader
  deleteShader fragmentShader
  useProgram shaderProgram
  return $ shaderProgram
