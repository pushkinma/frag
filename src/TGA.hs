{-# LANGUAGE LambdaCase #-}
-- TGA.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

-- | This module was based on lesson 24 from neon helium productions
-- http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=24

-- The TGA format is a used bitmap image file format. They are
-- quite easy to load compared to other formats and have
-- good support in image editors. All that has to be done is
-- read the header to determine the dimensions and pixel format.
-- Then the bytes have to be swapped and can be used by OpenGL.

-- If you see a texture that is upside down, just open it in your
-- editor and flip it vertically. Sometimes the TGA file is stored
-- with its pixels upside down.
module TGA where

import           Control.Monad
import           Data.Foldable
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Graphics.UI.GLUT
import           System.IO

-- | Reads a *.tga file.
readTga
  :: FilePath
  -> IO (Maybe (Size, PixelData Word8))
readTga filePath = do
  withBinaryFile filePath ReadMode $ \handle -> do
    buf <- mallocBytes 6 :: IO (Ptr Word8)
    --the first 12 bytes of the header aren't used
    void $ hGetBuf handle buf 6
    void $ hGetBuf handle buf 6
    void $ hGetBuf handle buf 6
    header <- peekArray 6 buf
    let w1       = fromIntegral (header!!1) * 256 :: Int
        width    = w1 + fromIntegral (head header)
        h1       = fromIntegral (header!!3) * 256 :: Int
        height   = h1 + fromIntegral (header!!2)
        bitspp   = fromIntegral (header!!4)
        numBytes = (bitspp `div` 8) * width * height
    --allocate memory for the image
    image <- mallocBytes numBytes
    void $ hGetBuf handle image numBytes
    --define whether the pixels are in RGB or RGBA format.
    pixelFormat <- getFormat (fromIntegral bitspp)
    free buf
    --convert the pixels which are in BGR/BGRA to RGB/RGBA
    swapBytes' image (bitspp `div` 8) (width * height)
    print ("loaded " ++ filePath)
    return $ Just
      ( Size (fromIntegral width) (fromIntegral height)
      , PixelData pixelFormat UnsignedByte image
      )

-- | Converts the image from bgr/bgra to rgb/rgba
-- perhaps the OpenGL bgra extension could be
-- used to avoid this.
swapBytes'
  :: Ptr Word8
  -> Int
  -> Int
  -> IO ()
swapBytes' image bytespp size =
  case bytespp of
    3 -> for_ [0..(size - 1)] $ swapByteRGB.plusPtr image.(bytespp *)
    _ -> for_ [0..(size - 1)] {-4-} $ swapByteRGBA.plusPtr image.(bytespp *)

-- | Converts from bgr to rgb.
swapByteRGB
  :: Ptr Word8
  -> IO ()
swapByteRGB ptr = do
  [b, g, r] <- peekArray 3 ptr
  pokeArray ptr [r, g, b]

-- | Converts from bgra to rgba.
swapByteRGBA
  :: Ptr Word8
  -> IO ()
swapByteRGBA ptr = do
  [b, g, r, a] <- peekArray 4 ptr
  pokeArray ptr [r, g, b, a]

-- | Returns the pixel format given the bits per pixel.
getFormat
  :: Int
  -> IO PixelFormat
getFormat = \case
  32 -> return RGBA
  _  -> return RGB -- 24
