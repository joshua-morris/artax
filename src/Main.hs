{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Artax.Camera
import Artax.Shader
import Artax.Program
import Artax.Texture
import Artax.Uniform

import Control.Monad (unless)
import Data.Bits
import Data.Default
import Data.Foldable
import Data.StateVar
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

vertices :: [GLfloat]
vertices = [ 
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
          ]

main :: IO ()
main = do
  initializeAll
  -- Create SDL window and set it as the GL context
  window <- createWindow "Artax" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL, windowInitialSize = (V2 1800 1000) }
  _      <- glCreateContext window

  renderer       <- createRenderer window (-1) defaultRenderer
  -- Load and compile shader program
  vertexShader   <- loadShader GL_VERTEX_SHADER "shaders/vert.glsl"
  fragmentShader <- loadShader GL_FRAGMENT_SHADER "shaders/frag.glsl"
  program        <- createProgram vertexShader fragmentShader

  -- Load and set texture uniforms
  texture0       <- newTextureFromImage "textures/container.jpg"
  tex0loc        <- uniform1i <$> uniformLocation program "ourTexture0"

  glEnable GL_DEPTH_TEST

  -- Vertex array
  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao

  -- Vertex buffer with vertices data
  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo

  let vsize = fromIntegral $ sizeOf (0.0::GLfloat)*(length vertices)
  verticesP <- newArray vertices
  glBufferData GL_ARRAY_BUFFER vsize (castPtr verticesP) GL_STATIC_DRAW

  let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
      nFloatOffset = \n -> castPtr $ plusPtr nullPtr (fromIntegral $ n*floatSize)

  -- Location 0: position
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (5*floatSize) nullPtr
  glEnableVertexAttribArray 0
  -- Location 1: texture
  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (5*floatSize) (nFloatOffset 3)
  glEnableVertexAttribArray 1

  glBindVertexArray 0

  -- Set uniform camera
  model      <- uniformMatrix4fv <$> (uniformLocation program "model")
  view       <- uniformMatrix4fv <$> (uniformLocation program "view")
  projection <- uniformMatrix4fv <$> (uniformLocation program "projection")

  let loop c = do
        events <- pollEvents
        let camera = foldl (updateCamera 0.2) c events
        let eventIsQPress event =
              case eventPayload event of
                KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
            qPressed = any eventIsQPress events

        glClearColor 0.2 0.3 0.3 1.0
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        glActiveTexture GL_TEXTURE0
        bindTexture texture0
        tex0loc $= 0 
        glBindVertexArray vao

        view       $= lookAt (_position camera) (_position camera + _front camera) (_up camera)
        projection $= perspective (pi/4) (1800/1000) 0.1 100.0
        forM_ (zip cubes [0..]) $ \(cube,i) -> do
          model $= mkTransformation (axisAngle (V3 (1::GLfloat) 0.3 0.5) (20*i)) cube
          glDrawArrays GL_TRIANGLES 0 36

        glBindVertexArray 0
        present renderer
        unless qPressed (loop camera)
  
  loop (def :: Camera)
  destroyWindow window
