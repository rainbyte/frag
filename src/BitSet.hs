{-
   An extremely simple and inefficient implementation of bit sets.
   Sven Panne 2000.   mailto:Sven.Panne@informatik.uni-muenchen.de
-}

module BitSet where

import Control.Monad (liftM)
import Data.Array.IO (IOUArray, newArray, writeArray, readArray, getBounds, rangeSize)

newtype BitSet = BitSet (IOUArray Int Bool)

emptyBS :: Int -> IO BitSet
emptyBS size = BitSet <$> newArray (0, size-1) False

clearBS :: BitSet -> Int -> IO ()
clearBS (BitSet bs) i = writeArray bs i False

clearAllBS :: BitSet -> IO ()
clearAllBS bs = sizeBS bs >>= \size -> mapM_ (clearBS bs) [0 .. size - 1]

setBS :: BitSet -> Int -> IO ()
setBS (BitSet bs) i = writeArray bs i True

isSetBS :: BitSet -> Int -> IO Bool
isSetBS (BitSet bs) = readArray bs

sizeBS :: BitSet -> IO Int
sizeBS (BitSet bs) = fmap rangeSize (getBounds bs)

