{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Foreign
import Foreign.C.String (newCAStringLen)
import Graphics.GL.Core33
import Graphics.GL.Types
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Artax" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
  renderer <- createRenderer window (-1) defaultRenderer

  vertexShader <- glCreateShader GL_VERTEX_SHADER
  vertexShaderSource <- readFile "shaders/vert.glsl"
  (sourceP, len) <- newCAStringLen vertexShaderSource
  linesPtrsPtr <- newArray [sourceP]
  lengthsP <- newArray [fromIntegral len]
  glShaderSource vertexShader 1 linesPtrsPtr lengthsP
  glCompileShader vertexShader

  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  fragmentShaderSource <- readFile "shaders/frag.glsl"
  (sourceP, len) <- newCAStringLen fragmentShaderSource
  linesPtrsPtr <- newArray [sourceP]
  lengthsP <- newArray [fromIntegral len]
  glShaderSource fragmentShader 1 linesPtrsPtr lengthsP
  glCompileShader fragmentShader

  shaderProgram <- glCreateProgram
  glAttachShader shaderProgram vertexShader
  glAttachShader shaderProgram fragmentShader
  glLinkProgram shaderProgram

  glDeleteShader vertexShader
  glDeleteShader fragmentShader

  glUseProgram shaderProgram

  let vertices = [
          -0.5, -0.5, 0.0,
          0.5, -0.5, 0.0,
          0.0,  0.5, 0.0
          ] :: [GLfloat]
  let vsize = fromIntegral $ sizeOf (0.0::GLfloat)*(length vertices)
  verticesP <- newArray vertices

  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao

  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vsize (castPtr verticesP) GL_STATIC_DRAW

  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral $ sizeOf (0.0::GLfloat)*3) nullPtr
  glEnableVertexAttribArray 0

  glBindVertexArray 0

  loop renderer vao
  destroyWindow window


loop :: Renderer -> GLuint -> IO ()
loop renderer vao = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT
  glBindVertexArray vao
  glDrawArrays GL_TRIANGLES 0 3
  glBindVertexArray 0
  present renderer
  unless qPressed (loop renderer vao)
