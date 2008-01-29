{- Frustum.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

Provides a function to extract the frustum and test
whether an AABB intersects it

-}

module Frustum where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL.CoordTrans


type FPlane =  (Double, Double, Double, Double)
type Frustum = (FPlane,FPlane,FPlane,FPlane,FPlane,FPlane)


normalisePlane :: FPlane -> IO FPlane
normalisePlane (x,y,z,d) = do
   let reciMag = (1/(sqrt(x*x+y*y+z*z)))
   return (x*reciMag, y*reciMag, z*reciMag, d*reciMag)


-- gets the frustum from the current view
getFrustum :: IO Frustum
getFrustum = do

   mvMatrix <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLdouble)
   [m00,m01,m02,m03,
    m10,m11,m12,m13,
    m20,m21,m22,m23,
    m30,m31,m32,m33] <- getMatrixComponents ColumnMajor mvMatrix
   pjMatrix <- get (matrix (Just (Projection))) :: IO (GLmatrix GLdouble)
   [p00,p01,p02,p03,
    p10,p11,p12,p13,
    p20,p21,p22,p23,
    p30,p31,p32,p33] <- getMatrixComponents ColumnMajor pjMatrix

   let clip00 = m00*p00 + m01*p10 + m02*p20 + m03*p30
   let clip01 = m00*p01 + m01*p11 + m02*p21 + m03*p31
   let clip02 = m00*p02 + m01*p12 + m02*p22 + m03*p32
   let clip03 = m00*p03 + m01*p13 + m02*p23 + m03*p33

   let clip10 = m10*p00 + m11*p10 + m12*p20 + m13*p30
   let clip11 = m10*p01 + m11*p11 + m12*p21 + m13*p31
   let clip12 = m10*p02 + m11*p12 + m12*p22 + m13*p32
   let clip13 = m10*p03 + m11*p13 + m12*p23 + m13*p33

   let clip20 = m20*p00 + m21*p10 + m22*p20 + m23*p30
   let clip21 = m20*p01 + m21*p11 + m22*p21 + m23*p31
   let clip22 = m20*p02 + m21*p12 + m22*p22 + m23*p32
   let clip23 = m20*p03 + m21*p13 + m22*p23 + m23*p33

   let clip30 = m30*p00 + m31*p10 + m32*p20 + m33*p30
   let clip31 = m30*p01 + m31*p11 + m32*p21 + m33*p31
   let clip32 = m30*p02 + m31*p12 + m32*p22 + m33*p32
   let clip33 = m30*p03 + m31*p13 + m32*p23 + m33*p33

   let rightX = clip03 - clip00
   let rightY = clip13 - clip10
   let rightZ = clip23 - clip20
   let rightD = clip33 - clip30

   let leftX  = clip03 + clip00
   let leftY  = clip13 + clip10
   let leftZ  = clip23 + clip20
   let leftD  = clip33 + clip30

   let bottomX  = clip03 + clip01
   let bottomY  = clip13 + clip11
   let bottomZ  = clip23 + clip21
   let bottomD  = clip33 + clip31

   let topX  = clip03 - clip01
   let topY  = clip13 - clip11
   let topZ  = clip23 - clip21
   let topD  = clip33 - clip31

   let backX  = clip03 - clip02
   let backY  = clip13 - clip12
   let backZ  = clip23 - clip22
   let backD  = clip33 - clip32

   let frontX  = clip03 + clip02
   let frontY  = clip13 + clip12
   let frontZ  = clip23 + clip22
   let frontD  = clip33 + clip32

   rightPlane  <- normalisePlane (rightX ,rightY ,rightZ ,rightD)
   leftPlane   <- normalisePlane (leftX  ,leftY  ,leftZ  ,leftD)
   bottomPlane <- normalisePlane (bottomX,bottomY,bottomZ,bottomD)
   topPlane    <- normalisePlane (topX   ,topY   ,topZ   ,topD)
   backPlane   <- normalisePlane (backX  ,backY  ,backZ  ,backD)
   frontPlane  <- normalisePlane (frontX ,frontY ,frontZ,frontD)

   return (rightPlane,leftPlane,bottomPlane,topPlane,backPlane,frontPlane)


-- tests if a box intersects a plane
testBox :: (Double,Double,Double)->(Double,Double,Double) -> FPlane ->Bool
testBox (x,y,z) (x2,y2,z2) (a,b,c,d)
   | (a * x  + b * y  + c * z  + d > 0) = True
   | (a * x2 + b * y  + c * z  + d > 0) = True
   | (a * x  + b * y2 + c * z  + d > 0) = True
   | (a * x2 + b * y2 + c * z  + d > 0) = True
   | (a * x  + b * y  + c * z2 + d > 0) = True
   | (a * x2 + b * y  + c * z2 + d > 0) = True
   | (a * x  + b * y2 + c * z2 + d > 0) = True
   | (a * x2 + b * y2 + c * z2 + d > 0) = True
   | otherwise = False


-- tests if an AABB lies within a frustum
boxInFrustum :: Frustum -> (Double,Double,Double) -> (Double,Double,Double) -> Bool
boxInFrustum (a,b,c,d,e,f) mn mx
   | not(test a) || not(test b) || not(test c)
     || not(test d) || not(test e) || not(test f) = False
   | otherwise = True
   where test = testBox mn mx

