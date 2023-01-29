{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Artax.Shader
import Artax.Program
import Artax.Texture
import Artax.Uniform

import Control.Monad (unless, forM_)
import Data.Bits
import Data.StateVar
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Foreign
import Graphics.GL.Core33
import Graphics.GL.Types
import SDL hiding (glBindTexture, Texture)

cubes :: [V3 GLfloat]
cubes = [
    V3 0 0 0,
    V3 2 5 (-15),
    V3 (-1.5) (-2.2) (-2.5),
    V3 (-3.8) (-2) (-12.3),
    V3 2.4 (-0.4) (-3.5),
    V3 (-1.7) 3 (-7.5),
    V3 1.3 (-2) (-2.5),
    V3 1.5 2 (-2.5),
    V3 1.5 0.2 (-1.5),
    V3 (-1.3) 1 (-1.5)
    ]

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Artax" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL, windowInitialSize = (V2 1800 1000) }

  glContext <- glCreateContext window
  _ <- createContext
  _ <- sdl2InitForOpenGL window glContext
  _ <- openGL3Init

  renderer <- createRenderer window (-1) defaultRenderer
  vertexShader   <- loadShader GL_VERTEX_SHADER "shaders/vert.glsl"
  fragmentShader <- loadShader GL_FRAGMENT_SHADER "shaders/frag.glsl"
  program <- createProgram vertexShader fragmentShader
  texture0 <- newTextureFromImage "textures/container.jpg"
  glEnable GL_DEPTH_TEST

  let vertices = [ 
          -0.5, -0.5, -0.5,  0.0, 0.0,
          0.5, -0.5, -0.5,  1.0, 0.0,
          0.5,  0.5, -0.5,  1.0, 1.0,
          0.5,  0.5, -0.5,  1.0, 1.0,
          -0.5,  0.5, -0.5,  0.0, 1.0,
          -0.5, -0.5, -0.5,  0.0, 0.0,
          -0.5, -0.5,  0.5,  0.0, 0.0,
          0.5, -0.5,  0.5,  1.0, 0.0,
          0.5,  0.5,  0.5,  1.0, 1.0,
          0.5,  0.5,  0.5,  1.0, 1.0,
          -0.5,  0.5,  0.5,  0.0, 1.0,
          -0.5, -0.5,  0.5,  0.0, 0.0,
          -0.5,  0.5,  0.5,  1.0, 0.0,
          -0.5,  0.5, -0.5,  1.0, 1.0,
          -0.5, -0.5, -0.5,  0.0, 1.0,
          -0.5, -0.5, -0.5,  0.0, 1.0,
          -0.5, -0.5,  0.5,  0.0, 0.0,
          -0.5,  0.5,  0.5,  1.0, 0.0,
          0.5,  0.5,  0.5,  1.0, 0.0,
          0.5,  0.5, -0.5,  1.0, 1.0,
          0.5, -0.5, -0.5,  0.0, 1.0,
          0.5, -0.5, -0.5,  0.0, 1.0,
          0.5, -0.5,  0.5,  0.0, 0.0,
          0.5,  0.5,  0.5,  1.0, 0.0,
          -0.5, -0.5, -0.5,  0.0, 1.0,
          0.5, -0.5, -0.5,  1.0, 1.0,
          0.5, -0.5,  0.5,  1.0, 0.0,
          0.5, -0.5,  0.5,  1.0, 0.0,
          -0.5, -0.5,  0.5,  0.0, 0.0,
          -0.5, -0.5, -0.5,  0.0, 1.0,
          -0.5,  0.5, -0.5,  0.0, 1.0,
          0.5,  0.5, -0.5,  1.0, 1.0,
          0.5,  0.5,  0.5,  1.0, 0.0,
          0.5,  0.5,  0.5,  1.0, 0.0,
          -0.5,  0.5,  0.5,  0.0, 0.0,
          -0.5,  0.5, -0.5,  0.0, 1.0
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

  let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
      nFloatOffset = \n -> castPtr $ plusPtr nullPtr (fromIntegral $ n*floatSize)

  -- Position
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (5*floatSize) nullPtr
  glEnableVertexAttribArray 0

  -- Texture
  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (5*floatSize) (nFloatOffset 3)
  glEnableVertexAttribArray 1

  glBindVertexArray 0

  -- Set uniforms
  tex0loc    <- uniform1i <$> uniformLocation program "ourTexture0"
  model      <- uniformMatrix4fv <$> (uniformLocation program "model")
  view       <- uniformMatrix4fv <$> (uniformLocation program "view")
  projection <- uniformMatrix4fv <$> (uniformLocation program "projection")

  loop renderer vao texture0 tex0loc model view projection 
  destroyWindow window


loop :: 
  Renderer 
  -> GLuint 
  -> Texture 
  -> SettableStateVar Int32
  -> SettableStateVar (M44 Float) 
  -> SettableStateVar (M44 Float)
  -> SettableStateVar (M44 Float)
  -> IO ()
loop renderer vao texture0 tex0loc model view projection = do
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  glClearColor 0.2 0.3 0.3 1.0
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  render
  openGL3RenderDrawData =<< getDrawData

  timeValue <- time
  let radius = 10
      camX   = sin timeValue * radius
      camZ   = cos timeValue * radius

  view $= lookAt (V3 camX 0 camZ) (V3 0 0 0) (V3 0 1 0)
  projection $= perspective (pi/4) (1800/1000) 0.1 100.0

  glActiveTexture GL_TEXTURE0
  bindTexture texture0
  tex0loc $= 0 
  glBindVertexArray vao

  forM_ (zip cubes [0..]) $ \(cube,i) -> do
    let angle = 20*i
    model $= mkTransformation (axisAngle (V3 (1::GLfloat) 0.3 0.5) angle) cube
    glDrawArrays GL_TRIANGLES 0 36

  glBindVertexArray 0
  present renderer
  unless qPressed (loop renderer vao texture0 tex0loc model view projection)
