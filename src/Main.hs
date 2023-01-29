{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Artax.Shader
import Artax.Program
import Artax.Texture
import Artax.Uniform

import Control.Monad (unless)
import Data.StateVar
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Foreign
import Graphics.GL.Core33
import Graphics.GL.Types
import SDL hiding (glBindTexture, Texture)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Artax" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }

  glContext <- glCreateContext window
  _ <- createContext
  _ <- sdl2InitForOpenGL window glContext
  _ <- openGL3Init

  renderer <- createRenderer window (-1) defaultRenderer
  vertexShader   <- loadShader GL_VERTEX_SHADER "shaders/vert.glsl"
  fragmentShader <- loadShader GL_FRAGMENT_SHADER "shaders/frag.glsl"
  program <- createProgram vertexShader fragmentShader
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

  -- Set uniforms
  trans <- uniformMatrix4fv <$> (uniformLocation program "transform")
  model <- uniformMatrix4fv <$> (uniformLocation program "model")
  view <- uniformMatrix4fv <$> (uniformLocation program "view")
  projection <- uniformMatrix4fv <$> (uniformLocation program "projection")

  loop renderer vao texture trans
  destroyWindow window


loop :: 
  Renderer 
  -> GLuint 
  -> Texture 
  -> SettableStateVar (M44 Float) 
  -> IO ()
loop renderer vao texture trans = do
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

  -- Set rotation matrix
  timeValue <- time
  let rotQ = axisAngle (V3 (0::GLfloat) 0 1) timeValue 
  let rotM33 = fromQuaternion rotQ
  let rotM33' = rotM33 !!* 0.5
  let transformationMatrix = mkTransformationMat rotM33' (V3 0.5 (-0.5) 0)
  trans $= transpose transformationMatrix

  bindTexture texture
  glBindVertexArray vao
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0
  present renderer
  unless qPressed (loop renderer vao texture trans)
