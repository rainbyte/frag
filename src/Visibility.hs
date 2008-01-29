{- Visibility.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

Visibility tests with BSP

(A lot of the code is rewritten from the collision detection module)

-}

module Visibility where

import Matrix (Vec3,
               crossProd,
               dotProd,
               mapTup,
               normalise,
               vectorAdd,
               vectorMult,
               vectorSub)
import BSP -- (bsPlaneNorm, bsPlaneDist, numOfBrushSides, textureType, brushside, BSPBrushSide(..), tree, planeNormal, dist, Tree(..))
import Collision(CollisionType(..))


epsilon :: Double
epsilon = 0.03125

getOffset :: CollisionType -> Vec3 -> Double
getOffset (Box _ _ extents) plane = getBoxOffs extents plane
getOffset (SphereT rad) _ = rad

getBoxOffs :: Vec3 -> Vec3 -> Double
getBoxOffs (x,y,z) (x1,y1,z1) =
   (abs (x*x1))+(abs (y*y1))+(abs (z*z1))


-------------------------------------------------------------------------------
-- visibility tests

aiVisTest :: BSPMap -> Vec3 -> Double -> Vec3 -> Int -> Bool
aiVisTest bsp currentPos angle targetPos range =
  case (fieldTest currentPos angle targetPos range) of
     False -> False
     _ -> rayTest bsp currentPos targetPos


-- test if the objct lies wihitn the field of view
fieldTest :: (Double,Double,Double) ->Double ->
  (Double,Double,Double) -> Int -> Bool
fieldTest  (x,y,z) angle (ox,oy,oz) range =
  (distance < (realToFrac range)  &&
  (horizangle <= 30) &&
  (abs verticalangle) <= 60 ) || distance < 300
  where
    distance = sqrt (((x-ox)^(2 :: Int))+((y-oy)^(2 :: Int))+((z-oz)^(2 :: Int)))
    horizanglei =
       let ha=acos $ dotProd (normalise $ vectorSub (ox,0,oz) (x,0,z)) (1,0,0)
       in case (oz > z) of
          False -> (ha*180/pi)
          True  -> (360 - (ha*180/pi))
    horizangle =
       min (abs (horizanglei - angle))  (abs (horizanglei - (angle + 360)))
    verticalangle =
       let va=acos $ dotProd (normalise $ vectorSub (ox,oy,oz) (x,y,z)) (0,1,0)
       in ((va*180/pi)-90)


-- fires rays to the target to determine visibility
rayTest :: BSPMap ->  Vec3 -> Vec3 -> Bool
rayTest bsp (x,y,z) vec2@(_,_,_) =
  let
   (x1,y1,z1) = normalise $ crossProd (vectorSub vec2 (x,y+30,z)) (0,1,0)
   (x2,y2,z2) = normalise $ crossProd (vectorSub vec2 (x,y+30,z)) (x1,y1,z1)
   v1 = vectorAdd vec2 (vectorMult (x1-x2,y1-y2,z1-z2) 45)
   v2 = vectorAdd vec2 (vectorMult (x1+x2,y1+y2,z1+z2) 45)
   v3 = vectorAdd vec2 (vectorMult (-x1-x2,-y1-y2,-z1-z2) 45)
   in case (snd $ clipRay2 bsp vec2 (x,y,z) (0,0,0)) of
       False -> True
       _ -> case (snd $ clipRay2 bsp v1 (x,y+30,z) (0,0,0)) of
              False -> True
              _ -> case (snd $ clipRay2 bsp v2 (x,y+30,z)(0,0,0)) of
                     False -> True
                     _ -> case (snd $ clipRay2 bsp v3 (x,y+30,z)(0,0,0)) of
                            False -> True
                            _ ->case(snd $ clipRay2 bsp v3 (x,y+30,z)(0,0,0))of
                                    False -> True
                                    True -> False


createSphere :: Double ->  CollisionType
createSphere rad = SphereT rad


clipRay2 :: BSPMap -> Vec3 -> Vec3 ->  Vec3 -> (Vec3, Bool)
clipRay2 mp pos oldpos (_,_,_) =
  let (colPos,collided,_,_) = traceo (createSphere 0) mp oldpos pos
  in (colPos, collided)


traceo ::  CollisionType -> BSPMap ->
   Vec3 -> Vec3 -> (Vec3,Bool,Bool,Bool)
traceo cType mp start end
  | (newRatio /= 1.0) =
        (vectorAdd start
            (vectorMult (vectorSub end start) newRatio),hasCol,step,grounded)
  | otherwise = (end,False,step,grounded)
  where
     (hasCol,step,grounded,newRatio,(_,_,_)) =
         fixCheck $ (checkNode cType (False,False,False,1.0, (0.0,0.0,0.0))
            (tree mp) 0.0 1.0 start end)
     fixCheck x =
        case x of
          (Just a) ->  a
          Nothing  -> (False,False,False,1.0, (0.0,0.0,0.0))


{-
drawTest :: BSPMap ->  Vec3 -> Vec3 ->IO()
drawTest bsp vec1@(x,y,z) vec2@(ox,oy,oz) = do
  let (x1,y1,z1) = normalise $ crossProd (vectorSub vec2 (x,y+30,z)) (0,1,0)
  let (x2,y2,z2) = normalise $ crossProd (vectorSub vec2 (x,y+30,z)) (x1,y1,z1)
  let (a1,b1,c1) = vectorAdd vec2 (vectorMult (x1-x2,y1-y2,z1-z2) 45)
  let (a2,b2,c2) = vectorAdd vec2 (vectorMult (x1+x2,y1+y2,z1+z2) 45)
  let (a3,b3,c3) = vectorAdd vec2 (vectorMult (-x1-x2,-y1-y2,-z1-z2) 45)
  let (a4,b4,c4) = vectorAdd vec2 (vectorMult (-x1+x2,-y1+y2,-z1+z2) 45)
  pointSize $= 13
  unsafeRenderPrimitive Points $ do
     vertex (Vertex3 a1 b1 c1)
     vertex (Vertex3 a2 b2 c2)
     vertex (Vertex3 a3 b3 c3)
     vertex (Vertex3 a4 b4 c4)
-}

-------------------------------------------------------------------------------
--  check for collisions with brushes


checkBrushes:: CollisionType -> (Bool, Bool, Bool, Double,Vec3)  ->
   Tree -> Vec3 -> Vec3 -> Maybe (Bool, Bool, Bool, Double, Vec3)
checkBrushes cType _ (Leaf leaf) start end =
   checkBrush start end cType (leafBrushes leaf)


checkBrush :: Vec3-> Vec3 -> CollisionType ->
   [BSPBrush] -> Maybe (Bool,Bool,Bool,Double,Vec3)
checkBrush _ _ _ [] = Just (False,False,False,0,(0,0,0))
checkBrush start end cType (brush:brushes) =
     let res = checkBrush'  start end cType brush
     in case (res) of
         Just (True,_,_,_,_) -> res
         _ -> checkBrush start end cType brushes


checkBrush' :: Vec3-> Vec3 -> CollisionType ->
   BSPBrush -> Maybe (Bool,Bool,Bool,Double,Vec3)
checkBrush'  start end cType brush
     |((numOfBrushSides brush) > 0) && ((textureType brush)==1) =
       let colout =
             checkBrushSides start end
                cType False False False False (-1.0)
                   (1.0) (0,0,0) (brushSides brush)
       in case colout of
          Just (out,collided,step,grounded,startR,endR,newNorm) ->
              case (startR < endR && startR > -1  && out) of
                 True -> Just (collided,step,grounded,fixRatio startR,newNorm)
                 _    -> Nothing
          _ -> Nothing
     |otherwise = Nothing
     where
        fixRatio x
           | x < 0.0 = 0.0
           | otherwise = x


checkBrushSides ::  Vec3 -> Vec3 -> CollisionType-> Bool -> Bool ->
   Bool -> Bool -> Double -> Double -> Vec3 -> [BSPBrushSide] ->
     (Maybe (Bool,Bool,Bool,Bool,Double,Double,Vec3))
checkBrushSides  (_,_,_) (_,_,_) _
  out collided step ground startR endR cNorm [] =
     Just (out,collided,step,ground,startR,endR,cNorm)
checkBrushSides  start@(x,_,z) end@(x1,_,z1) cType
  out collided step ground startR endR cNorm (b:bs)
     | startDist >  0 && endDist >  0 = Nothing
     | startDist <= 0 && endDist <= 0 = continue
     | startDist > endDist =
           case (ratio1 > startR) of
              True -> (checkBrushSides start end cType
                          checkout True mayStep grounded
                              ratio1 endR (bsPlaneNorm b) bs)
              _    -> continue
     | otherwise =
           case (ratio2 < endR) of
              True -> (checkBrushSides start end cType
                          checkout collided step ground
                              startR ratio2 cNorm bs)
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
        planey = get2nd3 (bsPlaneNorm b)
        ratio1     = (startDist - epsilon) / (startDist - endDist)
        ratio2     = (startDist + epsilon) / (startDist - endDist)
        continue  = checkBrushSides start end cType
                       checkout collided step ground
                           startR endR cNorm bs
        startDist = vDist start cType (bsPlaneNorm b) (bsPlaneDist b)
        endDist = vDist end cType  (bsPlaneNorm b) (bsPlaneDist b)
        get2nd3 (_,a,_) = a


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




-------------------------------------------------------------------------------


-- recurse down a nodes
checkNode :: CollisionType -> (Bool,Bool,Bool,Double,Vec3) ->
   Tree -> Double-> Double -> Vec3 -> Vec3 -> Maybe (Bool,Bool,Bool,Double,Vec3)
checkNode cType cState (Leaf leaf) _ _ start end =
   checkBrushes cType cState (Leaf leaf)  start end
checkNode cType cState (Branch node left right) startRatio endRatio start end =
   let sDist = (dotProd (planeNormal node) start) - (dist node)
       eDist = (dotProd (planeNormal node) end)   - (dist node)
   in recurse sDist eDist
   where
      boffset = getOffset cType (planeNormal node)
      recurse sDist eDist
         | (sDist >= boffset && eDist >= boffset)             =
            checkNode cType cState left  sDist eDist start end
         | (sDist < ((-1)*boffset) && eDist < ((-1)*boffset)) =
            checkNode cType cState right sDist eDist start end
         | otherwise =
            split cType cState sDist eDist
               startRatio endRatio start end
                  (Branch node left right)


-- splits a vector if it lies on both side of
-- the splitting plane
split :: CollisionType -> (Bool,Bool,Bool,Double,Vec3) ->
   Double -> Double -> Double -> Double -> Vec3 -> Vec3 ->
      Tree -> Maybe (Bool,Bool,Bool,Double,Vec3)
split cType cState startDist endDist startRatio
   endRatio start end (Branch node left right)
       |startDist < endDist =
         let result1 = checkNode cType cState
                          right startRatio (middleR r1) start (middleV r1)
         in case (result1) of
             Just (True,_,_,_,_) -> result1
             _ -> let result2 =
                        checkNode  cType cState left
                         (middleR r2) endRatio (middleV r2) end
                  in case (result2) of
                       Just (True,_,_,_,_) -> result2
                       _                   -> Nothing
       |startDist > endDist =
         let result1 = checkNode  cType cState
                          left  startRatio (middleR r2) start (middleV r2)
         in case (result1) of
             Just (True,_,_,_,_) -> result1
             _ -> let result2 =
                        checkNode  cType cState right
                         (middleR r1) endRatio   (middleV r1) end
                  in case (result2) of
                       Just (True,_,_,_,_) -> result2
                       _                   -> Nothing
       |otherwise =
         let result1 = checkNode  cType cState
                          left  startRatio (middleR 1.0) start (middleV 1.0)
         in case (result1) of
             Just (True,_,_,_,_) -> result1
             _ -> let result2 =
                        checkNode  cType cState right
                         (middleR 0.0) endRatio   (middleV 0.0) end
                  in case (result2) of
                       Just (True,_,_,_,_) -> result2
                       _                   -> Nothing
       where
          inverseDist = 1.0/(startDist - endDist)
          boffset     = getOffset cType (planeNormal node)
          r1          = fixDouble $ (startDist - boffset - epsilon)*inverseDist
          r2          = fixDouble $ (startDist + boffset + epsilon)*inverseDist
          middleR     = getMiddleRatio startRatio endRatio
          middleV     = getHalfVec start end
          fixDouble x
             | x < 0.0 = 0.0
             | x > 1.0 = 1.0
             | otherwise = x


-- gets the middle of 2 ratios
getMiddleRatio :: Double -> Double -> Double -> Double
getMiddleRatio startRatio endRatio ratio =
       startRatio + (ratio * (endRatio - startRatio))


-- gets half a vector
getHalfVec :: Vec3 -> Vec3 -> Double -> Vec3
getHalfVec start end ratio =
       vectorAdd start (mapTup (ratio*) (vectorSub end start))

