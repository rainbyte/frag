{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}

{- $Id: AFRPPoint2.hs,v 1.3 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPPoint2                                           *
*       Purpose:        2D point abstraction (R^2).                          *
*       Authors:        Henrik Nilsson and Antony Courtney                   *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPPoint2 (
    module AFRPVectorSpace,
    module AFRPAffineSpace,
    Point2(..), -- Non-abstract, instance of AffineSpace
    point2X,    -- :: RealFloat a => Point2 a -> a
    point2Y     -- :: RealFloat a => Point2 a -> a
) where

import AFRPVectorSpace
import AFRPAffineSpace
import AFRPVector2
import AFRPForceable

------------------------------------------------------------------------------
-- 2D point, constructors and selectors.
------------------------------------------------------------------------------

data RealFloat a => Point2 a = Point2 !a !a deriving (Eq, Show)

point2X :: RealFloat a => Point2 a -> a
point2X (Point2 x _) = x

point2Y :: RealFloat a => Point2 a -> a
point2Y (Point2 _ y) = y


------------------------------------------------------------------------------
-- Affine space instance
------------------------------------------------------------------------------

instance RealFloat a => AffineSpace (Point2 a) (Vector2 a) a where
    origin = Point2 0 0

    (Point2 x y) .+^ v = Point2 (x + vector2X v) (y + vector2Y v)

    (Point2 x y) .-^ v = Point2 (x - vector2X v) (y - vector2Y v)

    (Point2 x1 y1) .-. (Point2 x2 y2) = vector2 (x1 - x2) (y1 - y2)


------------------------------------------------------------------------------
-- Forceable instance
------------------------------------------------------------------------------

instance RealFloat a => Forceable (Point2 a) where
     force = id
