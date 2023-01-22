{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Artax.Shader
import Artax.Program
import Artax.Texture

import Control.Monad (unless)
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Foreign
import Foreign.C.String (newCString)
import Graphics.GL.Core33
import Graphics.GL.Types
import SDL hiding (glBindTexture, Texture)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Artax" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL, windowMode = FullscreenDesktop }

  glContext <- glCreateContext window

  _ <- createContext
  _ <- sdl2InitForOpenGL window glContext
  _ <- openGL3Init

  renderer <- createRenderer window (-1) defaultRenderer

  vertexShader   <- loadShader GL_VERTEX_SHADER "shaders/vert.glsl"
  fragmentShader <- loadShader GL_FRAGMENT_SHADER "shaders/frag.glsl"

  _ <- createProgram vertexShader fragmentShader

  texture <- newTextureFromImage "textures/wall.jpg"

  let vertices = [
          0.5, 0.5, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0,
          0.5, -0.5, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0,
          -0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
          -0.5, 0.5, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0
          ] :: [GLfloat]
  let vsize = fromIntegral $ sizeOf (0.0::GLfloat)*(length vertices)
  verticesP <- newArray vertices

  let indices = [
          0, 1, 3,
          1, 2, 3
          ] :: [GLuint]
  let isize = fromIntegral $ sizeOf (0::GLuint)*(length indices)
  indicesP <- newArray indices

  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao

  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vsize (castPtr verticesP) GL_STATIC_DRAW

  eboP <- malloc
  glGenBuffers 1 eboP
  ebo <- peek eboP
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER isize (castPtr indicesP) GL_STATIC_DRAW

  let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
      nFloatOffset = \n -> castPtr $ plusPtr nullPtr (fromIntegral $ n*floatSize)

  -- Position
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (8*floatSize) nullPtr
  glEnableVertexAttribArray 0

  -- Colour
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (8*floatSize) (nFloatOffset 3)
  glEnableVertexAttribArray 1

  -- Texture
  glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE (8*floatSize) (nFloatOffset 6)
  glEnableVertexAttribArray 2

  glBindVertexArray 0

  ourColor <- newCString "ourColor"

  loop renderer vao texture
  destroyWindow window


loop :: Renderer -> GLuint -> Texture -> IO ()
loop renderer vao texture = do
  openGL3NewFrame
  sdl2NewFrame
  newFrame
  
  withWindowOpen "Artax" do
    text "TODO" 

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
  render
  openGL3RenderDrawData =<< getDrawData

  bindTexture texture
  glBindVertexArray vao
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0
  present renderer
  unless qPressed (loop renderer vao texture)
