-- Textures.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

-- | This module is for loading textures
module Textures where

import           Data.Traversable
import           Data.Word
import           Foreign.Marshal.Alloc
import           Graphics.UI.GLUT
import           System.IO.Error

import           TGA

-- | Read a list of images and returns a list of textures
-- all images are assumed to be in the TGA image format.
getAndCreateTextures
  :: [String]
  -> IO [Maybe TextureObject]
getAndCreateTextures fileNames = do
  let fileNamesExts = map (("tga/" ++) . (++ ".tga")) fileNames
  texData <- for fileNamesExts readImageC
  for texData createTexture

-- | Read a single texture.
getAndCreateTexture
  :: String
  -> IO (Maybe TextureObject)
getAndCreateTexture fileName = do
  texData <- readImageC ("tga/" ++ fileName ++ ".tga")
  createTexture texData

-- | Read the image data.
readImageC
  :: String
  -> IO (Maybe (Size, PixelData Word8))
readImageC path = do
  catchIOError (readTga path) $ \_ -> do
    print ("missing texture: " ++ path)
    return Nothing

-- | Creates the texture.
createTexture
  :: Maybe (Size, PixelData a)
  -> IO (Maybe TextureObject)
createTexture (Just (Size x y, pixels@(PixelData _ _ ptr))) = do
  [texName] <- genObjectNames 1 -- generate our texture.
  --rowAlignment Unpack $= 1
  textureBinding Texture2D $= Just texName -- make our new texture the current texture.
  --generateMipmap Texture2D $= Enabled
  build2DMipmaps Texture2D RGBA' (fromIntegral x) (fromIntegral y) pixels
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  --textureWrapMode Texture2D S $= (Repeated, Repeat)
  --textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFunction $= Modulate
  free ptr
  return (Just texName)
createTexture Nothing = return Nothing
