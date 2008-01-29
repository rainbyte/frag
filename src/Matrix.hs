-- Matrix.hs; Hugh Rayner (hnra275@cse.unsw.edu.au) 2004

-- This module performs several linear algebra functions which are necessary for the game.
-- The are used for functions such as rotation of the player and collision detection.

module Matrix where

import Graphics.UI.GLUT

--type Vec3 = (GLdouble,GLdouble,GLdouble)
type Vec3 = (Double,Double,Double)
type Mat3 = (Vec3,Vec3,Vec3)

mapTup::(a->b)->(a,a,a)->(b,b,b)
mapTup f (a,b,c) = (f a, f b, f c)

-- Finds the dot product of two vectors
dotProd :: Vec3->Vec3->Double
dotProd (a,b,c) (aa,bb,cc) = a*aa + b*bb + c*cc

-- Finds the cross product of two 3-vectors
crossProd::Vec3->Vec3->Vec3
crossProd (a,b,c) (aa,bb,cc) = (b*cc-c*bb,c*aa-a*cc,a*bb-b*aa)

-- Finds the transpose of a 3*3 matrix
transpose3 :: Mat3->Mat3
transpose3 ((a1,a2,a3),(b1,b2,b3),(c1,c2,c3)) = ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3))

-- Multiplies a 3*3 matrix with a 3-vector
matrixVectorMult ::Mat3->Vec3->Vec3
matrixVectorMult m v = mapTup (dotProd v) m

-- As above, but arguments swapped
matrixVectorMult2 ::Vec3->Mat3->Vec3
matrixVectorMult2 v m = mapTup (dotProd v) m

-- Multiplies two 3*3 matrices
matrixMult :: Mat3->Mat3->Mat3
matrixMult m1 m2 = (mapTup (matrixVectorMult (transpose3 m1)) m2)

-- As above, but arguments swapped
matrixMult2 :: Mat3->Mat3->Mat3
matrixMult2 m2 m1 = (mapTup (matrixVectorMult (transpose3 m1)) m2)

-- Adds two vectors
vectorAdd :: Vec3->Vec3->Vec3
vectorAdd (a1,a2,a3) (b1,b2,b3) = (a1+b1,a2+b2,a3+b3)

-- Subtracts two vectors
vectorSub :: Vec3->Vec3->Vec3
vectorSub (a1,a2,a3) (b1,b2,b3) = (a1-b1,a2-b2,a3-b3)

-- As above, but arguments swapped
vectorSub2 :: Vec3->Vec3->Vec3
vectorSub2 a b = vectorSub b a

-- Multiplies a vecotor by a scalar
vectorMult :: Vec3->Double->Vec3
vectorMult v s = mapTup (s*) v

-- finds the normal of a flat polygon
findNormal :: [Vec3] -> Vec3
findNormal (a:(b:(c:_))) = normalise (crossProd (vectorSub a b) (vectorSub c b))
findNormal _ = (0,0,0)

-- Normalises a vector
normalise :: Vec3 -> Vec3
normalise a = vectorMult a (sqrt (1/len a))
    where
       len (va,vb,vc) = va*va+vb*vb+vc*vc

-- Makes a Vertex3 (GLdouble) from a Vec3
vert::Vec3->Vertex3(GLdouble)
vert (x,y,z) = Vertex3 x y z

-- Makes a TexCoord2 (GLdouble) from a Vec3
coord::(Double,Double)->TexCoord2(GLdouble)
coord (x,y) = TexCoord2 x y

-- Makes a Vector3 (GLdouble) from a Vec3
vect::Vec3->Vector3(GLdouble)
vect (x,y,z) = Vector3 x y z

-- Makes a Normal3 (GLdouble) from a Vec3
norm3::Vec3->Normal3(GLdouble)
norm3 (x,y,z) = Normal3 x y z
