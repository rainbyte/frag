{-# LANGUAGE BangPatterns #-}

{- Collision.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

Collision detection with BSP

A tutorial for collision detection along with source code can be found at

http://www.devmaster.net/articles/quake3collision/
-}

module Collision (clipCamera, clipObject, clipRay, CollisionType(..)) where


import BSP
import Matrix
import Camera

data CollisionType = Box !Vec3 !Vec3 !Vec3 | SphereT !Double

epsilon :: Double
epsilon = 0.03125

-------------------------------------------------------------------------------
-- Movement clipping


-- clips a camera against the environment
clipCamera :: BSPMap -> Camera -> Camera -> (Camera, Bool)
clipCamera mp newcam oldcam =
   let
     (colPos,_,ground,nsteps) =
         traceStep (createBox (-20, -50, -20) (20, 50, 20))
            mp (cpos oldcam) (cpos newcam)
     diffp    = vectorSub colPos (cpos oldcam)
     newv     = vectorAdd diffp  (viewPos oldcam)
     stepPos  = vectorAdd colPos (0,nsteps,0)
     stepView = vectorAdd newv   (0,nsteps,0)
   in (Camera {cpos    = stepPos,
               viewPos = stepView,
               upVec   = (0,1,0)}, ground)

-- clips an object against the environment
clipObject :: BSPMap -> Vec3 -> Vec3 -> Vec3 -> (Vec3, Bool)
clipObject mp pos oldpos (sx,sy,sz) =
   let
     (colPos,_,ground,_) =
         traceStep (createBox (-sx, -sy, -sz) (sx,sy,sz)) mp oldpos pos
   in (colPos, ground)


-- clips a ray against the environment
clipRay :: BSPMap -> Vec3 -> Vec3 -> Vec3 -> (Vec3, Bool)
clipRay mp pos oldpos (_,_,_) =
   let
     (colPos,collided,_,_) =
         traceo (createSphere 0) mp oldpos pos
   in (colPos, collided)


-------------------------------------------------------------------------------
-- Tracing functions

-- Note: If your movement vector is too small you might move through walls
-- or fall through the floor. If your movement vector is too large you won't
-- be able to move at all.


-- allows an object to step across low obstacles
traceStep :: CollisionType -> BSPMap -> Vec3 -> Vec3 -> (Vec3,Bool,Bool,Double)
traceStep cType mp start end
   | (col && step) =
     let (nsteps,finalPos) = tryToStep cType mp newPos end 1 15
     in (finalPos, col, ground, nsteps)
   |otherwise = (newPos, col, ground, 0)
   where (newPos, col,step,ground,_) =  trace cType mp start end


-- returns the point where the collision occured, no sliding or stepping
traceo ::  CollisionType -> BSPMap -> Vec3 -> Vec3 -> (Vec3,Bool,Bool,Bool)
traceo cType mp start end
   |(newRatio /= 1.0) = (
       vectorAdd
          start
            (vectorMult (vectorSub end start) newRatio),hasCol,step,grounded)
   | otherwise = (end,False,step,grounded)
   where
      (hasCol,step,grounded,newRatio,(_,_,_)) =
          fixCheck $
            (checkNode cType
                  (False,False,False,1.0,(0.0,0.0,0.0))
                      (tree mp) 0.0 1.0 start end)


-- allows an object to slide against the environment
trace ::  CollisionType -> BSPMap -> Vec3 -> Vec3 -> (Vec3,Bool,Bool,Bool,Vec3)
trace  cType mp start end
   | (newRatio /= 1.0) =
      let newPos  = vectorAdd start (vectorMult (vectorSub end start) newRatio)
          moveVec = vectorSub end newPos
          dst    = dotProd moveVec newNorm
          end2    = vectorSub end $ vectorMult newNorm dst
          (newPos2,_,_,_,(n2x,n2y,n2z)) = trace cType mp newPos end2
      in case (n2y > 0.2 || (grounded)) of
          True -> (newPos2, True,step,True ,(n2x,n2y,n2z))
          _    -> (newPos2, True,step,False,(n2x,n2y,n2z))
   | otherwise = (end,False,step,grounded,newNorm)
   where
      (_,step,grounded,newRatio,newNorm@(_,_,_)) =
         fixCheck $
            (checkNode
                cType
                   (False,False,False,1.0, (0.0,0.0,0.0))
                      (tree mp) 0.0 1.0 start end)




-------------------------------------------------------------------------------


-- creates an AABB for use in collision detection
createBox :: Vec3 -> Vec3 -> CollisionType
createBox v1 v2 = Box v1 v2 (getBoxExtents v1 v2)


-- creates a sphere for collision detection
createSphere :: Double ->  CollisionType
createSphere rad = SphereT rad


-- gets the largest ends of the box
getBoxExtents :: Vec3 -> Vec3 -> Vec3
getBoxExtents (x,y,z) (x1,y1,z1) =
   (max (-x) x1,max (-y) y1, max (-z) z1)


getBoxOffs :: Vec3 -> Vec3 -> Double
getBoxOffs (x,y,z) (x1,y1,z1) =
   (abs (x*x1))+(abs (y*y1))+(abs (z*z1))


getOffset :: CollisionType -> Vec3 -> Double
getOffset (Box _ _ extents) plane = getBoxOffs extents plane
getOffset (SphereT rad) _ = rad


fixCheck ::
   Maybe (Bool, Bool, Bool, Double, Vec3) ->
      (Bool, Bool, Bool, Double, Vec3)
fixCheck (Just a) = a
fixCheck Nothing = (False,False,False,1.0, (0.0,0.0,0.0))


-- checks if we can step across a low obstacle
tryToStep ::
   CollisionType -> BSPMap ->
      Vec3 -> Vec3 ->
         Double -> Double  -> (Double,Vec3)
tryToStep  cType mp (x,y,z) (x1,y1,z1) i maxheight
   | i < maxheight =
     let (pos,col,_,_,_) = trace cType mp  (x,y+i,z) (x1,y1+i,z1)
     in case (col) of
           False -> (i,pos)
           _     -> tryToStep cType mp (x,y,z) (x1,y1,z1) (i+1) maxheight
   | otherwise = (0,(x,y,z))

-------------------------------------------------------------------------------
-- checks for collisions with brushes

checkBrushes ::
   CollisionType -> (Bool, Bool, Bool, Double,Vec3) ->
      Tree -> Vec3 -> Vec3 ->
         Maybe (Bool, Bool, Bool, Double, Vec3)
checkBrushes cType _ (Leaf leaf) start end =
   case (map (checkBrush start end cType) (leafBrushes leaf)) of
      []     -> Nothing
      [r]    -> r
      (r:rs) -> foldr brushCompare r rs
checkBrushes _ _ (Branch _ _ _) _ _ = Nothing

checkBrush ::
   Vec3-> Vec3 -> CollisionType ->
      BSPBrush -> Maybe (Bool,Bool,Bool,Double,Vec3)
checkBrush  start end cType brush
   |((numOfBrushSides brush) > 0) && ((textureType brush)==1) =
     let colout =
            checkBrushSides
               start end cType
                  False False False False
                     (-1.0) (1.0) (0,0,0) (brushSides brush)
     in case colout of
           Just (out,collided,step,grounded,startR,endR,newNorm) ->
              case (startR < endR && startR > -1  && out) of
                 True -> Just (collided,step,grounded,fixRatio startR,newNorm)
                 _    -> Nothing
           _  -> Nothing
   |otherwise = Nothing
   where
      fixRatio x
        | x < 0.0 = 0.0
        | otherwise = x


-- we use this function to compare the results
-- of a brush check so that we get the closest
-- collision
brushCompare ::
   Maybe (Bool, Bool, Bool, Double, Vec3) ->
      Maybe (Bool, Bool, Bool, Double, Vec3) ->
         Maybe (Bool, Bool, Bool, Double, Vec3)
brushCompare (Just (a1,b1,c1,d1,e1)) (Just(a2,b2,c2,d2,e2))
     | d1 < d2   = Just (a1 || a2, b1 || b2, c1 || c2, d1, e1)
     | otherwise = Just (a1 || a2, b1 || b2, c1 || c2, d2, e2)
brushCompare (Just a) Nothing = Just a
brushCompare Nothing (Just b) = Just b
brushCompare Nothing Nothing = Nothing


-- this checks if we have collided with a brush
checkBrushSides ::
   Vec3 -> Vec3 -> CollisionType->
      Bool -> Bool -> Bool -> Bool -> Double ->
         Double -> Vec3 -> [BSPBrushSide] ->
            (Maybe (Bool,Bool,Bool,Bool,Double,Double,Vec3))
checkBrushSides  (_,_,_) (_,_,_)
   _ out collided step ground startR endR cNorm [] =
      Just (out,collided,step,ground,startR,endR,cNorm)
checkBrushSides  start@(x,_,z) end@(x1,_,z1)
   cType out collided step ground startR endR cNorm (b:bs)
   | startDist >  0 && endDist >  0 = Nothing
   | startDist <= 0 && endDist <= 0 = continue
   | startDist > endDist =
         case (ratio1 > startR) of
            True -> (checkBrushSides start end cType checkout
                        True mayStep grounded ratio1 endR (bsPlaneNorm b) bs)
            _    -> continue
   | otherwise =
         case (ratio2 < endR) of
            True -> (checkBrushSides start end cType checkout
                        collided step ground startR ratio2 cNorm bs)
            _    -> continue
   where
      checkout
          | startDist > 0 = True
          | otherwise = out
      mayStep
          | ((x /= x1 || z /= z1) && planey /= 1) = True
          | otherwise = step
      grounded
          | planey >= 0.2 = True
          | otherwise = ground
      get2nd3 (_,q,_) = q
      planey    = get2nd3 (bsPlaneNorm b)
      ratio1    = (startDist - epsilon) / (startDist - endDist)
      ratio2    = (startDist + epsilon) / (startDist - endDist)
      continue  = checkBrushSides start end cType checkout
                        collided step ground startR endR cNorm bs
      startDist = vDist start cType (bsPlaneNorm b) (bsPlaneDist b)
      endDist   = vDist end   cType (bsPlaneNorm b) (bsPlaneDist b)


vDist :: Vec3 -> CollisionType -> Vec3 -> Double  ->  Double
vDist vec box@(Box _ _ _) pnorm pdist =
   (dotProd (vectorAdd vec (getVOffs pnorm box)) pnorm) - pdist
vDist vec (SphereT rad) pnorm pdist =
   (dotProd vec pnorm) - (pdist + rad)


getVOffs:: Vec3 -> CollisionType  -> Vec3
getVOffs (p1,p2,p3) (Box (x,y,z) (x1,y1,z1) _) =
   (chooseMin' p1 x x1,chooseMin' p2 y y1, chooseMin' p3 z z1)
   where
      chooseMin' p mn mx
         | p < 0 = mx
         | otherwise = mn
getVOffs (_, _, _) (SphereT _) = (0,0,0)

-------------------------------------------------------------------------------
-- recurse down the bsp tree checking for collisions

checkNode ::
   CollisionType -> (Bool,Bool,Bool,Double,Vec3) ->
      Tree -> Double-> Double -> Vec3 -> Vec3 ->
         Maybe (Bool,Bool,Bool,Double,Vec3)
checkNode cType cState (Leaf leaf) _ _ start end =
   checkBrushes cType cState (Leaf leaf)  start end
checkNode cType cState (Branch node left right) startRatio endRatio start end =
   let
     sDist = (dotProd (planeNormal node) start) - (dist node)
     eDist = (dotProd (planeNormal node) end)   - (dist node)
   in recurse sDist eDist
   where
      boffset = getOffset cType (planeNormal node)
      recurse sDist eDist
         |(sDist >= boffset && eDist >= boffset) =
            checkNode cType cState left  sDist eDist start end
         |(sDist < ((-1)*boffset) && eDist < ((-1)*boffset)) =
            checkNode cType cState right sDist eDist start end
         |otherwise = split cType cState sDist eDist
                         startRatio endRatio
                            start end (Branch node left right)


-- splits the movement vector if it lies on both sides of a splitting plane
split ::
   CollisionType -> (Bool,Bool,Bool,Double,Vec3) ->
      Double -> Double -> Double -> Double ->
         Vec3 -> Vec3 -> Tree -> Maybe (Bool,Bool,Bool,Double,Vec3)
split cType cState startDist endDist startRatio endRatio
   start end (Branch node left right)
   | startDist < endDist =
       let
          result1 =
             checkNode cType cState
                right startRatio (middleR r1) start (middleV r1)
          result2 =
             checkNode cType cState
                left (middleR r2) endRatio (middleV r2) end
       in brushCompare result1 result2
   | startDist > endDist =
       let
          result1 =
             checkNode cType cState
                left  startRatio (middleR r2) start (middleV r2)
          result2 =
             checkNode cType cState
                right (middleR r1) endRatio (middleV r1) end
       in brushCompare result1 result2
   | otherwise =
       let
          result1 =
             checkNode cType cState
                left  startRatio (middleR 1.0) start (middleV 1.0)
          result2 =
             checkNode cType cState
                right (middleR 0.0) endRatio (middleV 0.0) end
     in brushCompare result1 result2
   where
      inverseDist = 1.0/(startDist - endDist)
      boffset = getOffset cType (planeNormal node)
      r1 = fixDouble $ (startDist - boffset - epsilon)*inverseDist
      r2 = fixDouble $ (startDist + boffset + epsilon)*inverseDist
      middleR = getMiddleRatio startRatio endRatio
      middleV = getHalfVec start end
      fixDouble x
         | x < 0.0 = 0.0
         | x > 1.0 = 1.0
         | otherwise = x
split _ _ _ _ _ _ _ _ (Leaf _) = Nothing

getMiddleRatio :: Double -> Double -> Double -> Double
getMiddleRatio startRatio endRatio ratio =
   startRatio + (ratio * (endRatio - startRatio))


getHalfVec :: Vec3 -> Vec3 -> Double -> Vec3
getHalfVec start end ratio =
   vectorAdd start (mapTup (ratio*) (vectorSub end start))


