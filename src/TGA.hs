{- TGA.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

This module was based on lesson 24 from neon helium productions
http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=24

The TGA format is a used bitmap image file format. They are
quite easy to load compared to other formats and have
good support in image editors. All that has to be done is
read the header to determine the dimensions and pixel format.
Then the bytes have to be swapped and can be used by OPenGL.

If you see a texture that is upside down, just open it in your
editor and flip it vertically. Somtimes the TGA file is stored
with its pixels upside down.

-}



module TGA where

import Data.Word ( Word8 )
import Control.Exception ( bracket )
import System.IO ( Handle, IOMode(ReadMode), openBinaryFile, hGetBuf, hClose )
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (plusPtr, Ptr())
import Graphics.UI.GLUT -- (Size, PixelData, UnsignedByte, PixelFormat, RGBA, RGB)


withBinaryFile' :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile' filePath = bracket (openBinaryFile filePath ReadMode) hClose

-- reads a *.tga file
readTga :: FilePath -> IO (Maybe (Size, PixelData Word8))
readTga filePath =
   withBinaryFile' filePath $ \handle -> do
   buf <- mallocBytes 6 :: IO(Ptr Word8)
   --the first 12 bytes of the header aren't used
   hGetBuf handle buf 6
   hGetBuf handle buf 6
   hGetBuf handle buf 6
   header <- peekArray 6 buf
   let w1       = (fromIntegral (header!!1))*256 :: Int
   let width    = w1 + (fromIntegral (header!!0))
   let h1       = (fromIntegral (header!!3))*256 :: Int
   let height   = h1 + (fromIntegral (header!!2))
   let bitspp   = (fromIntegral (header!!4))
   let numBytes = (bitspp `div` 8) * width * height
   --allocate memory for the image
   image <- mallocBytes numBytes
   hGetBuf handle image numBytes
   --define whether the pixels are in RGB or RGBA format.
   pixelFormat <- getFormat (fromIntegral bitspp)
   free buf
   --convert the pixels which are in BGR/BGRA to RGB/RGBA
   swapBytes' image (bitspp `div` 8) (width * height)
   print ("loaded "++filePath)
   return $ Just (Size (fromIntegral width) (fromIntegral height),
               PixelData pixelFormat UnsignedByte image)

-- converts the image from bgr/bgra to rgb/rgba
-- perhaps the opengl bgra extension could be
-- used to avoid this
swapBytes' :: Ptr Word8 -> Int -> Int -> IO()
swapBytes' image bytespp size =
   case bytespp of
      3 -> do mapM_ (swapByteRGB.(plusPtr image).(bytespp*)) [0..(size-1)]
      _ -> do mapM_ (swapByteRGBA.(plusPtr image).(bytespp*)) [0..(size-1)] -- 4

-- converts from bgr to rgb
swapByteRGB :: Ptr Word8 -> IO()
swapByteRGB ptr = do
   [b,g,r] <- peekArray 3 ptr
   pokeArray ptr [r,g,b]

-- converts from bgra to rgba
swapByteRGBA :: Ptr Word8 -> IO()
swapByteRGBA ptr = do
   [b,g,r,a] <- peekArray 4 ptr
   pokeArray ptr [r,g,b,a]

-- returns the pixel format given the bits per pixel
getFormat :: Int ->  IO(PixelFormat)
getFormat bpp = do
   case bpp of
      32 ->  return RGBA
      _ ->  return RGB -- 24






