{-# LANGUAGE BangPatterns #-}

{- $Id: AFRPVector3.hs,v 1.3 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:		AFRPVector3					     *
*       Purpose:	3D vector abstraction (R^3).			     *
*	Authors:	Henrik Nilsson and Antony Courtney		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module AFRPVector3 (
    module AFRPVectorSpace,
    Vector3,		-- Abstract, instance of VectorSpace
    vector3,		-- :: RealFloat a => a -> a -> a -> Vector3 a
    vector3X,		-- :: RealFloat a => Vector3 a -> a
    vector3Y,		-- :: RealFloat a => Vector3 a -> a
    vector3Z,		-- :: RealFloat a => Vector3 a -> a
    vector3XYZ,		-- :: RealFloat a => Vector3 a -> (a, a, a)
    vector3Spherical,	-- :: RealFloat a => a -> a -> a -> Vector3 a
    vector3Rho,		-- :: RealFloat a => Vector3 a -> a
    vector3Theta,	-- :: RealFloat a => Vector3 a -> a
    vector3Phi,		-- :: RealFloat a => Vector3 a -> a
    vector3RhoThetaPhi,	-- :: RealFloat a => Vector3 a -> (a, a, a)
    vector3Rotate 	-- :: RealFloat a => a -> a -> Vector3 a -> Vector3 a
) where

import AFRPVectorSpace
import AFRPForceable

------------------------------------------------------------------------------
-- 3D vector, constructors and selectors.
------------------------------------------------------------------------------

-- Restrict coefficient space to RealFloat (rather than Floating) for now.
-- While unclear if a complex coefficient space would be useful (and if the
-- result really would be a 3d vector), the only thing causing trouble is the
-- use of atan2 in vector3Theta and vector3Phi. Maybe atan2 can be generalized?

data RealFloat a => Vector3 a = Vector3 !a !a !a deriving Eq

vector3 :: RealFloat a => a -> a -> a -> Vector3 a
vector3 x y z = Vector3 x y z

vector3X :: RealFloat a => Vector3 a -> a
vector3X (Vector3 x _ _) = x

vector3Y :: RealFloat a => Vector3 a -> a
vector3Y (Vector3 _ y _) = y

vector3Z :: RealFloat a => Vector3 a -> a
vector3Z (Vector3 _ _ z) = z

vector3XYZ :: RealFloat a => Vector3 a -> (a, a, a)
vector3XYZ (Vector3 x y z) = (x, y, z)

vector3Spherical :: RealFloat a => a -> a -> a -> Vector3 a
vector3Spherical rho theta phi =
    Vector3 (rhoSinPhi * cos theta) (rhoSinPhi * sin theta) (rho * cos phi)
    where
	rhoSinPhi = rho * sin phi

vector3Rho :: RealFloat a => Vector3 a -> a
vector3Rho (Vector3 x y z) = sqrt (x * x + y * y + z * z)

vector3Theta :: RealFloat a => Vector3 a -> a
vector3Theta (Vector3 x y _) = atan2 y x

vector3Phi :: RealFloat a => Vector3 a -> a
vector3Phi v@(Vector3 _ _ z) = acos (z / vector3Rho v)

vector3RhoThetaPhi :: RealFloat a => Vector3 a -> (a, a, a)
vector3RhoThetaPhi (Vector3 x y z) = (rho, theta, phi)
    where
        rho   = sqrt (x * x + y * y + z * z)
        theta = atan2 y x
	phi   = acos (z / rho)


------------------------------------------------------------------------------
-- Vector space instance
------------------------------------------------------------------------------

instance RealFloat a => VectorSpace (Vector3 a) a where
    zeroVector = Vector3 0 0 0

    a *^ (Vector3 x y z) = Vector3 (a * x) (a * y) (a * z)

    (Vector3 x y z) ^/ a = Vector3 (x / a) (y / a) (z / a)

    negateVector (Vector3 x y z) = (Vector3 (-x) (-y) (-z))

    (Vector3 x1 y1 z1) ^+^ (Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)

    (Vector3 x1 y1 z1) ^-^ (Vector3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)

    (Vector3 x1 y1 z1) `dot` (Vector3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2


------------------------------------------------------------------------------
-- Additional operations
------------------------------------------------------------------------------

vector3Rotate :: RealFloat a => a -> a -> Vector3 a -> Vector3 a
vector3Rotate theta' phi' v =
    vector3Spherical (vector3Rho v)
		     (vector3Theta v + theta')
		     (vector3Phi v + phi')


------------------------------------------------------------------------------
-- Forceable instance
------------------------------------------------------------------------------

instance RealFloat a => Forceable (Vector3 a) where
     force = id
