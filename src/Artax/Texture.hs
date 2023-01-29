module Artax.Texture
  ( Texture(..)
  , newTexture
  , bindTexture
  , newTextureFromImage
  ) where

import Codec.Picture
import Control.Monad.IO.Class
import qualified Data.Vector.Storable as VS
import Foreign
import Graphics.GL.Types
import Graphics.GL.Core33

newtype Texture = Texture GLuint

bindTexture :: MonadIO m => Texture -> m ()
bindTexture (Texture texture) = glBindTexture GL_TEXTURE_2D texture

-- | @newTexture@ Binds a new texture with target GL_TEXTURE_2D
newTexture :: Storable a => VS.Vector a -> GLsizei -> GLsizei -> IO Texture
newTexture image height width = do
  textureP <- malloc
  glGenTextures 1 textureP
  texture <- peek textureP
  glBindTexture GL_TEXTURE_2D texture
  VS.unsafeWith image $ \dataP ->
      glTexImage2D GL_TEXTURE_2D 0 GL_RGB8 width height 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataP)

  glGenerateMipmap GL_TEXTURE_2D
  glBindTexture GL_TEXTURE_2D 0
  return $ Texture texture

-- | @newTextureFromImage@ Binds a new 2D texture from a filepath
newTextureFromImage :: String -> IO Texture
newTextureFromImage path = do
  eErrDI <- readImage path
  dyImage <- case eErrDI of
    Left err -> do
      putStrLn err
      return $ ImageRGB8 $ generateImage (\x y ->
          let x' = fromIntegral x in PixelRGB8 x' x' x') 800 600
    Right di -> return di

  let ipixelrgb8 = convertRGB8 dyImage
      width      = fromIntegral $ imageWidth ipixelrgb8
      height     = fromIntegral $ imageHeight ipixelrgb8
      image      = imageData ipixelrgb8

  texture <- newTexture image width height 
  return $ texture
