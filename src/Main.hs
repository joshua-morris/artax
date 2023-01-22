{-# LANGUAGE OverloadedStrings #-}

module Main where

import Artax.Shader
import Artax.Program

import Codec.Picture
import Control.Monad (unless)
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C.String (newCString)
import Graphics.GL.Core33
import Graphics.GL.Types
import SDL hiding (glBindTexture)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Artax" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
  renderer <- createRenderer window (-1) defaultRenderer

  vertexShader   <- loadShader GL_VERTEX_SHADER "shaders/vert.glsl"
  fragmentShader <- loadShader GL_FRAGMENT_SHADER "shaders/frag.glsl"

  _ <- createProgram vertexShader fragmentShader

  eErrDI <- readImage "textures/wall.jpg"
  dyImage <- case eErrDI of
      Left e -> do
        return $ ImageRGB8 $ generateImage (\x y ->
            let x' = fromIntegral x in PixelRGB8 x' x' x') 800 600
      Right di -> return di

  let ipixelrgb8 = convertRGB8 dyImage
      iWidth     = fromIntegral $ imageWidth ipixelrgb8
      iHeight    = fromIntegral $ imageHeight ipixelrgb8
      iData      = imageData ipixelrgb8


  textureP <- malloc
  glGenTextures 1 textureP
  texture <- peek textureP
  glBindTexture GL_TEXTURE_2D texture
  VS.unsafeWith iData $ \dataP ->
      glTexImage2D GL_TEXTURE_2D 0 GL_RGB8 iWidth iHeight 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataP)

  glGenerateMipmap GL_TEXTURE_2D
  glBindTexture GL_TEXTURE_2D 0

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


loop :: Renderer -> GLuint -> GLuint -> IO ()
loop renderer vao texture = do
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
  glBindTexture GL_TEXTURE_2D texture
  glBindVertexArray vao
  glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0
  glBindTexture GL_TEXTURE_2D 0
  present renderer
  unless qPressed (loop renderer vao texture)
