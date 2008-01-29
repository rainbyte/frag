{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- $Id: AFRPPoint3.hs,v 1.3 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPPoint3                                           *
*       Purpose:        3D point abstraction (R^3).                          *
*       Authors:        Henrik Nilsson and Antony Courtney                   *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPPoint3 (
    module AFRPVector3,
    Point3(..), -- Non-abstract, instance of AffineSpace
    point3X,    -- :: RealFloat a => Point3 a -> a
    point3Y,    -- :: RealFloat a => Point3 a -> a
    point3Z     -- :: RealFloat a => Point3 a -> a
) where

import AFRPVectorSpace
import AFRPAffineSpace
import AFRPVector3
import AFRPForceable

------------------------------------------------------------------------------
-- 3D point, constructors and selectors.
------------------------------------------------------------------------------

data RealFloat a => Point3 a = Point3 !a !a !a deriving Eq

point3X :: RealFloat a => Point3 a -> a
point3X (Point3 x _ _) = x

point3Y :: RealFloat a => Point3 a -> a
point3Y (Point3 _ y _) = y

point3Z :: RealFloat a => Point3 a -> a
point3Z (Point3 _ _ z) = z


------------------------------------------------------------------------------
-- Affine space instance
------------------------------------------------------------------------------

instance RealFloat a => AffineSpace (Point3 a) (Vector3 a) a where
    origin = Point3 0 0 0

    (Point3 x y z) .+^ v =
        Point3 (x + vector3X v) (y + vector3Y v) (z + vector3Z v)

    (Point3 x y z) .-^ v =
        Point3 (x - vector3X v) (y - vector3Y v) (z - vector3Z v)

    (Point3 x1 y1 z1) .-. (Point3 x2 y2 z2) =
        vector3 (x1 - x2) (y1 - y2) (z1 - z2)


------------------------------------------------------------------------------
-- Forceable instance
------------------------------------------------------------------------------

instance RealFloat a => Forceable (Point3 a) where
     force = id
