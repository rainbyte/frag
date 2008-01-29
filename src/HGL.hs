{-# LANGUAGE BangPatterns #-}

module HGL where

data Event
  = Char      { char :: !Char, isDown :: !Bool }
  | Button    { pt   :: !Point, isLeft, isDown :: !Bool }
  | MouseMove { pt   :: !Point }

 deriving Show

data Point = Point !(Int,Int) deriving Show
