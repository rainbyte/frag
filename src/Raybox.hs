{- Raybox.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

This module performs collision tests between AABBs, spheres and rays.

-}

module Raybox where

import Matrix

-- tests if a ray intersects a box
rayBox :: (Double,Double,Double) ->  (Double,Double,Double) ->
               (Double,Double,Double) -> (Double,Double,Double) -> Bool
rayBox (rayOx,rayOy,rayOz) (rayDx,rayDy,rayDz) (minx,miny,minz)(maxx,maxy,maxz)
   | (rayOx,rayOy,rayOz) == (rayDx,rayDy,rayDz) = False
   | otherwise =
       case ((txmin > tymax) || (tymin > txmax)) of
          True -> False
          _    -> intersectz
    where (txmin,txmax) = getT rayOx rayDx minx maxx
          (tymin,tymax) = getT rayOy rayDy miny maxy
          (tzmin,tzmax) = getT rayOz rayDz minz maxz
          intersectz    =
             not (((max txmin tymin) > tzmax) || (tzmin > (min txmax tymax)))

getT :: Double ->  Double -> Double -> Double -> (Double, Double)
getT rayO rayD mn mx
   | dd >= 0   = ((mn-rayO)*dd, (mx-rayO)*dd)
   | otherwise = ((mx-rayO)*dd, (mn-rayO)*dd)
   where dd = (1/(rayO-rayD))


-- tests if 2 AABBs intersect
boxBox :: Vec3 -> Vec3 -> Vec3 -> Vec3  -> Bool
boxBox (x1,y1,z1) (w1,h1,d1) (x2,y2,z2) (w2,h2,d2)
   | abs (x1 - x2) <= w1+w2 = True
   | abs (y1 - y2) <= h1+h2 = True
   | abs (z1 - z2) <= d1+d2 = True
   | otherwise = False


-- tests if an AABB intersects a sphere
boxSphere :: Vec3 -> Vec3 -> Vec3 -> Double  -> Bool
boxSphere (x1,y1,z1) (w1,h1,d1) (x2,y2,z2) rad
   | (abs (x1 - x2)) <= (w1+rad) = True
   | (abs (y1 - y2)) <= (h1+rad) = True
   | (abs (z1 - z2)) <= (d1+rad) = True
   | otherwise = False


-- test if two spheres intersect
sphereSphere ::  Vec3 -> Vec3 -> Double -> Double -> Bool
sphereSphere (x1,y1,z1) (x2,y2,z2) rad1 rad2 =
   (sqrt ((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2))+((z1-z2)*(z1-z2))) <= (rad1+rad2)


-- tests if a point lies within a sphere
spherePoint :: Vec3 -> Vec3 -> Double -> Bool
spherePoint (x1,y1,z1) (x2,y2,z2) rad =
   (sqrt $ ((x1-x2)*(x1-x2)) + ((y1-y2)*(y1-y2)) + ((z1-z2)*(z1-z2))) <= rad


-- test if a point lies in an AABB
pointBox :: Vec3 -> Vec3 -> Vec3   -> Bool
pointBox (x1,y1,z1) (x2,y2,z2) (w,h,d)
   | abs (x1 - x2) <= w && abs (y1 - y2) <= h && abs (z1 - z2) <= d = True
   | otherwise = False
