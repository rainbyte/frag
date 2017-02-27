{-
   ReadImage.hs (adapted from readImage.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This module has been modified to read both color and alpha data necessary for transparent textures in OpenGL.

   Support for reading a file of raw RGB data:
      4 bytes big-endian width
      4 bytes big-endian height
      width * height RGBA byte quadruples (the original module reads width * height RGB byte triples)
-}

module ReadImage ( readImage ) where

import Data.Word ( Word8, Word32 )
import Control.Exception ( bracket )
import Control.Monad ( when )
import System.IO ( Handle, IOMode(ReadMode), openBinaryFile, hGetBuf, hClose )
import System.IO.Error ( mkIOError, eofErrorType )
import Foreign ( Ptr, alloca, mallocBytes, Storable(..) )
import Graphics.UI.GLUT

-- This is probably overkill, but anyway...
newtype Word32BigEndian = Word32BigEndian Word32

word32BigEndianToGLsizei :: Word32BigEndian -> GLsizei
word32BigEndianToGLsizei (Word32BigEndian x) = fromIntegral x

instance Storable Word32BigEndian where
   sizeOf ~(Word32BigEndian x) = sizeOf x
   alignment ~(Word32BigEndian x) = alignment x
   peek ptr = do
      let numBytes = sizeOf (undefined :: Word32BigEndian)
      bytes <- mapM (peekByteOff ptr) [ 0 ..  numBytes - 1 ] :: IO [Word8]
      let value = foldl (\val byte -> val * 256 + fromIntegral byte) 0 bytes
      return $ Word32BigEndian value
   poke = error ""

-- This is the reason for all this stuff above...
readGLsizei :: Handle -> IO GLsizei
readGLsizei handle =
   alloca $ \buf -> do
      hGetBufFully handle buf (sizeOf (undefined :: Word32BigEndian))
      word32BigEndianToGLsizei <$> peek buf

-- A handy variant of hGetBuf with additional error checking
hGetBufFully :: Handle -> Ptr a -> Int -> IO ()
hGetBufFully handle ptr numBytes = do
   bytesRead <- hGetBuf handle ptr numBytes
   when (bytesRead /= numBytes) $
      ioError $ mkIOError eofErrorType "hGetBufFully" (Just handle) Nothing


-- Closing a file is nice, even when an error occurs during reading.
withBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile filePath = bracket (openBinaryFile filePath ReadMode) hClose

readImage :: FilePath -> IO (Maybe (Size, PixelData a))
readImage filePath =
   withBinaryFile filePath $ \handle -> do
      width <- readGLsizei handle
      height <- readGLsizei handle
      let numBytes = fromIntegral (4 * width * height)  -- changed the 3 to a 4 to make space for our alpha data.
      buf <- mallocBytes numBytes
      hGetBufFully handle buf numBytes
      return $ Just (Size width height, PixelData RGBA UnsignedByte buf)  -- changed the PixelFormat constructor here from RGB to RGBA, to account for our alpha data.
