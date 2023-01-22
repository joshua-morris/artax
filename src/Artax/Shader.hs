module Artax.Shader
  ( ShaderType
  , Shader(..)
  , deleteShader
  , loadShader
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Foreign
import Foreign.C.String (newCAStringLen)
import Graphics.GL.Core33
import Graphics.GL.Types

type ShaderType = GLenum

newtype Shader = Shader GLuint

newShader :: MonadIO m => ShaderType -> m Shader
newShader = liftM Shader . glCreateShader

deleteShader :: MonadIO m => Shader -> m ()
deleteShader (Shader shader) = glDeleteShader shader

-- | @loadShader shaderType path@ loads a shader of type @type@ from the path given by @source@
loadShader :: ShaderType -> String -> IO Shader
loadShader shaderType path = do
  source <- readFile path
  (Shader shader) <- newShader shaderType
  (sourceP, len)  <- newCAStringLen source
  linesPtrsPtr    <- newArray [sourceP]
  lengthsP        <- newArray [fromIntegral len]
  glShaderSource shader 1 linesPtrsPtr lengthsP
  glCompileShader shader
  return $ Shader shader
