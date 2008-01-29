{-# LANGUAGE TypeOperators, RankNTypes #-}
{- $Id: AFRPMiscellany.hs,v 1.4 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPMiscellany                                       *
*       Purpose:        Collection of entities that really should be part    *
*                       of the Haskell 98 prelude or simply have no better   *
*                       home.                                                *
*       Authors:        Henrik Nilsson and Antony Courtney                   *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPMiscellany (
-- Reverse function composition
    ( # ),      -- :: (a -> b) -> (b -> c) -> (a -> c), infixl 9

-- Arrow plumbing aids
    dup,        -- :: a -> (a,a)
    swap,       -- :: (a,b) -> (b,a)

-- Maps over lists of pairs
    mapFst,     -- :: (a -> b) -> [(a,c)] -> [(b,c)]
    mapSnd,     -- :: (a -> b) -> [(c,a)] -> [(c,b)]

-- Generalized tuple selectors
    sel3_1, sel3_2, sel3_3,
    sel4_1, sel4_2, sel4_3, sel4_4,
    sel5_1, sel5_2, sel5_3, sel5_4, sel5_5,

-- Floating point utilities
    fDiv,       -- :: (RealFrac a, Integral b) => a -> a -> b
    fMod,       -- :: RealFrac a => a -> a -> a
    fDivMod     -- :: (RealFrac a, Integral b) => a -> a -> (b, a)
) where

infixl 9 #
infixl 7 `fDiv`, `fMod`


------------------------------------------------------------------------------
-- Reverse function composition
------------------------------------------------------------------------------

( # ) :: (a -> b) -> (b -> c) -> (a -> c)
f # g = g . f


------------------------------------------------------------------------------
-- Arrow plumbing aids
------------------------------------------------------------------------------

dup :: a -> (a,a)
dup x = (x,x)

swap :: (a,b) -> (b,a)
swap ~(x,y) = (y,x)


------------------------------------------------------------------------------
-- Maps over lists of pairs
------------------------------------------------------------------------------

mapFst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapFst _ []             = []
mapFst f ((x, y) : xys) = (f x, y) : mapFst f xys

mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd _ []             = []
mapSnd f ((x, y) : xys) = (x, f y) : mapSnd f xys


------------------------------------------------------------------------------
-- Generalized tuple selectors
------------------------------------------------------------------------------

-- Triples

sel3_1 :: forall t t1 t2. (t, t1, t2) -> t

sel3_2 :: forall t t1 t2. (t, t1, t2) -> t1
sel3_1 (x,_,_) = x

sel3_3 :: forall t t1 t2. (t, t1, t2) -> t2
sel3_2 (_,x,_) = x
sel3_3 (_,_,x) = x


-- 4-tuples

sel4_1 :: forall t t1 t2 t3. (t, t1, t2, t3) -> t

sel4_2 :: forall t t1 t2 t3. (t, t1, t2, t3) -> t1
sel4_1 (x,_,_,_) = x

sel4_3 :: forall t t1 t2 t3. (t, t1, t2, t3) -> t2
sel4_2 (_,x,_,_) = x

sel4_4 :: forall t t1 t2 t3. (t, t1, t2, t3) -> t3
sel4_3 (_,_,x,_) = x
sel4_4 (_,_,_,x) = x


-- 5-tuples
sel5_1 ::(t, t1, t2, t3, t4) -> t
sel5_1 (x,_,_,_,_) = x
sel5_2 :: (t, t1, t2, t3, t4) -> t1
sel5_2 (_,x,_,_,_) = x
sel5_3 :: (t, t1, t2, t3, t4) -> t2
sel5_3 (_,_,x,_,_) = x
sel5_4 :: (t, t1, t2, t3, t4) -> t3
sel5_4 (_,_,_,x,_) = x
sel5_5 :: (t, t1, t2, t3, t4) -> t4
sel5_5 (_,_,_,_,x) = x


------------------------------------------------------------------------------
-- Floating point utilities
------------------------------------------------------------------------------

-- Floating-point div and modulo operators.

-- fDiv :: (RealFrac a, Integral b) => a -> a -> b
fDiv :: (RealFrac a) => a -> a -> Int
fDiv x y = fst (fDivMod x y)

fMod :: RealFrac a => a -> a -> a
fMod x y = snd $ fDivMod x y

fDivMod :: (RealFrac a) => a -> a -> (Int, a)
fDivMod x y = (q, r)
    where
        q = (floor (x/y))
        r = x - fromIntegral q * y
