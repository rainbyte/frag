{- Quaternion.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

This module just performs some basic
converions between quaternions and matrices

-}


module Quaternion where

import Graphics.UI.GLUT -- (GLmatrix, GLfloat, newMatrix, ColumnMajor)

type Quaternion = (Float,Float,Float,Float)

type Matrix3x3 = ((Float,Float,Float),
                  (Float,Float,Float),
                  (Float,Float,Float))

-- converts from quaternion to matrix
quat2Mat :: Quaternion -> (Float,Float,Float) -> IO (GLmatrix GLfloat)
quat2Mat (x,y,z,w) (t1,t2,t3)=
   newMatrix ColumnMajor [(r00 :: GLfloat),r01,r02,r03,
                          r10,r11,r12,r13,
                          r20,r21,r22,r23,
                          r30,r31,r32,r33]
   where r00 = 1 - (2*((y*y)+(z*z)))
         r01 = 2 * ((x*y)-(w*z))
         r02 = 2 * ((x*z)+(w*y))
         r03 = 0

         r10 = 2 * ((x*y)+(w*z))
         r11 = 1 - (2*((x*x)+(z*z)))
         r12 = 2 * ((y*z)-(w*x))
         r13 = 0

         r20 = 2 * ((x*z)-(w*y))
         r21 = 2 * ((y*z)+(w*x))
         r22 = 1 - (2*((x*x)+(y*y)))
         r23 = 0

         r30 = t1
         r31 = t2
         r32 = t3
         r33 = 1

-- converts from matrix to quaternion
mat2Quat :: Matrix3x3 -> Quaternion
mat2Quat ((r00,r01,r02),
          (r10,r11,r12),
          (r20,r21,r22))
   | diag > 0.00000001      = ((r21-r12)/scale0,
                              (r02-r20)/scale0,
                              (r10-r01)/scale0,
                              0.25*scale0)
   | r00 > r11 && r00 > r22 = (0.25*scale1,
                              (r10+r01)/scale1,
                              (r02+r20)/scale1,
                              (r21-r12)/scale1)
   | r11 > r22              = ((r10+r01)/scale2,
                              0.25*scale2,
                              (r21+r12)/scale2,
                              (r02-r20)/scale2)
   | otherwise              = ((r02+r20)/scale3,
                              (r21+r12)/scale3,
                              0.25*scale3,
                              (r10-r01)/scale3)
   where
      diag   = r00+r11+r22+1
      scale0 = 2*(sqrt diag)
      scale1 = 2*(sqrt (r00-r11-r22+1))
      scale2 = 2*(sqrt (r11-r00-r22+1))
      scale3 = 2*(sqrt (r22-r00-r11+1))


-- does not really perform spherical linaer interpolation
-- but the difference isn't really noticeable
slerp :: Quaternion -> Quaternion -> Float -> Quaternion
slerp q1@(x0,y0,z0,w0) q2@(x1,y1,z1,w1) t
   | q1 == q2 = q1
   |otherwise = ((scale0*x0)+(scale1*x1),
                 (scale0*y0)+(scale1*y1),
                 (scale0*z0)+(scale1*z1),
                 (scale0*w0)+(scale1*w1))
   where
      scale0 = 1 - t
      scale1 = t




