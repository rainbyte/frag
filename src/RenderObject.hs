

module RenderObject (
    renderObjects
) where

import MD3
import Object
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import Data.Maybe
import Camera
import Matrix
import Frustum
import BSP
import Data.HashTable
import Visibility

renderObjects :: IORef(Camera) -> HashTable String Model -> Frustum -> BSPMap  -> ObsObjState -> IO()
renderObjects camRef models frust map oos
  | isRay oos = renderRay oos
  | isProjectile oos = renderProjectile oos
  | isAICube oos = renderEnemy camRef models frust map oos
  | otherwise = return()


renderRay :: ObsObjState -> IO()
renderRay (OOSRay {rayStart = (x1,y1,z1),
                            rayEnd   = (x2,y2,z2),
                            clipped  = cl}) = do
    cullFace               $= Nothing
    color $ Color4 255 0 0 (255 :: GLubyte)
    depthFunc              $= Just Always
    unsafeRenderPrimitive Quads $ do
          vertex (Vertex3 x2 (y2+0.3) z2)
          vertex (Vertex3 x2 (y2-0.3) z2)
          vertex (Vertex3 x1 (y1-0.3) z1)
          vertex (Vertex3 x1 (y1+0.3) z1)
    color $ Color4 255 255 255 (255 :: GLubyte)
    unsafeRenderPrimitive Quads $ do
          vertex (Vertex3 x2 (y2+0.12) z2)
          vertex (Vertex3 x2 (y2-0.12) z2)
          vertex (Vertex3 x1 (y1-0.12) z1)
          vertex (Vertex3 x1 (y1+0.12) z1)
    depthFunc              $= Just Less
    cullFace               $= Just Front
    case cl of
          True -> do
                cullFace                    $= Nothing
                unsafePreservingMatrix $ do
                   translate (Vector3 x2 y2 z2)
                   renderQuadric
                         (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
                            (Sphere 3 12 12)
                   cullFace               $= Just Front
          _ -> return()
    color $ Color4 255 255 255 (255 :: GLubyte)


renderProjectile :: ObsObjState -> IO()
renderProjectile (OOSProjectile {projectileOldPos = (x,y,z)}) = do
   unsafePreservingMatrix $ do
         translate (Vector3 x y z)
         depthFunc              $= Just Always
         cullFace               $= Nothing
         color $ Color4 0 0 80 (255 :: GLubyte)
         renderQuadric
            (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
                   (Sphere 4 12 12)
         color $ Color4 50 50 160 (255 :: GLubyte)
         renderQuadric
            (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
                   (Sphere 3.5 12 12)
         color $ Color4 255 255 255 (255 :: GLubyte)
         renderQuadric
            (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
                   (Sphere 2.5 12 12)
         cullFace               $= Just Front
         depthFunc              $= Just Less


renderEnemy ::
  IORef(Camera) -> HashTable String Model ->
        Frustum -> BSPMap  -> ObsObjState -> IO()
renderEnemy camRef models frust bspmap (OOSAICube {oosOldCubePos = (x,y,z),
                                                                            oosCubeSize   = (sx,sy,sz),
                                                                            oosCubeAngle  = angle,
                                                                            oosCubePitch  = p,
                                                                            target         = targ,
                                                                            upperAnim      = ua,
                                                                            fade                   = f,
                                                                            lowerAnim      = la,
                                                                            modelName      = name}) = do

   --perform a test to see if the object is visible from the player's location
   cam    <- readIORef camRef
   clustVis <- isObjectVisible bspmap (cpos cam) (x,y,z)
   case (clustVis) of
         False -> return ()
         True -> do
         -- a second check to see if the object is within the player's frustum
         let frusTest = boxInFrustum frust
                                    (vectorAdd (x,y,z) (-sx,-sy,-sz))
                                    (vectorAdd (x,y,z) (sx,sy,sz))
         case (frusTest) of
            True -> do
                  -- a third check to see if a ray can be fired to
                  --the objects position without colliding
                  let rayVis = rayTest bspmap (cpos cam) (x,y,z)
                  case (rayVis) of
                        False -> return()
                        _ -> do
                           unsafePreservingMatrix $ do
                                 lineWidth $= 5.0
                                 translate (Vector3 x y z)
                                 Just model <- Data.HashTable.lookup models name
                                 writeIORef (pitch model)
                                    (Just $ do
                                                    cullFace $=  Nothing
                                                    cullFace $=  Just Front
                                                    (rotate p (Vector3 0 1 0)))
                                 writeIORef (lowerState model)  la
                                 writeIORef (upperState model)  ua
                                 currentColor $= Color4 (f*60) (f*60) (f*60) (1 :: Float)
                                 unsafePreservingMatrix $ do
                                    rotate ((-90) :: GLdouble) (Vector3 1 0 0)
                                    rotate (angle) (Vector3 0 0 1)
                                    translate (Vector3 (-10) 0 (-10 :: Double))
                                    scale 1.5 1.5 (1.5 :: GLfloat)
                                    drawModel (modelRef model,lowerState model)
                                 currentColor $= Color4 1 1 1 (1 :: Float)
                                 writeIORef (pitch model) Nothing
            False -> return()



