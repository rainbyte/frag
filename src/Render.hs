{-# LANGUAGE BangPatterns #-}

module Render (
    renderObjects,
    renderGun,
    renderHud,
    GameData(..)
) where

import MD3
import Object
import Graphics.Rendering.OpenGL
import Data.IORef
import Data.Maybe
import Camera
import Matrix
import Frustum
import BSP
import qualified Data.HashTable.IO as HT
import Visibility
import TextureFonts

data GameData = GameData {
                            gamemap        :: IORef(BSPMap),
                            models         :: HT.BasicHashTable String Model,
                            textures       :: HT.BasicHashTable String (Maybe TextureObject),
                            camera         :: IORef(Camera),
                            lastDrawTime   :: IORef(Int),
                            lastDrawTime2  :: IORef(Int),
                            hasReacted     :: IORef(Bool),
                            fonts          :: (Maybe TextureObject,DisplayList),
                            nbase          :: DisplayList,
                            lock           :: IORef(Bool),
                            fpsc           :: IORef(Double,Double),
                            fpss           :: IORef(Double,Double,Double),
                            nems           :: !Int
                         }

-- renders the playerstate, gun and crosshairs
renderHud :: GameData -> ObsObjState -> Int -> Int -> IO()
renderHud gd playerState noos tme = do
   setUpOrtho $ do
          --show the framerate
          lastTime3 <- readIORef (lastDrawTime2 gd)
          let dt = ((realToFrac(tme - lastTime3))/1000)
          color $ Color4 255 255 255 (255 :: GLubyte)
          printFonts' 0 464 (fonts gd) 1 $
                "framerate = " ++ (show $ ((truncate ((1/dt) :: Double)) :: Int))

          --print the player's score
          printFonts' 0 448 (fonts gd) 1
                ("ezpks = " ++(show (score playerState)))

          --print the position of the player
          let (q,w,e) = (cpos (oldCam playerState))
          printFonts' 0 432 (fonts gd) 1
                ("position = " ++(show ((truncate q :: Int),(truncate w :: Int),(truncate e :: Int))))

          --print the number of objects
          printFonts' 0 416 (fonts gd) 1 ("No. of objs = " ++(show noos))

          --print a message if the player has eliminated all enemies
          color $ Color4 0 255 0 (255 :: GLubyte)
          case ((score playerState) == (nems gd) && (nems gd) > 0) of
                True ->  do
                   printFonts' 96 272 (fonts gd) 1 ("You've killed everybody! (You monster.)")
                   --printFonts' 248 256 (fonts gd) 1 ("Happy Now?")
                _ -> return ()

          --print a message if the player has died
          case (health playerState <= 0) of
                True -> do
                            color $ Color4 255 0 0 (255 :: GLubyte)
                            printFonts' 210 240 (fonts gd) 1 ("Oh, botheration. You died.")
                            color $ Color4 255 255 255 (255 :: GLubyte)
                _ -> return ()


          --render the health and score of the player with big fonts
          color $ Color4 255 255 255 (255 :: GLubyte)
          printFonts' 4 50 (fonts gd) 1 ("health")
          printFonts' 540 50 (fonts gd) 1 ("score")
          color $ Color4 232 192 0 (255 :: GLubyte)
          case (health playerState > 0) of
            True        -> renderNum  4 1 (nbase gd) (truncate(health playerState))
            False  -> renderNum 4 1 (nbase gd) (0)
          color $ Color4 232 192 0 (255 :: GLubyte)
          renderNum  540 1      (nbase gd) (score playerState)

          --render a smiley at the middle of the screen
          unsafePreservingMatrix $ do
            scale 20 10 (20 :: GLfloat)
            color $ Color4 255 255 255 (255 :: GLubyte)
            printLife (fonts gd) (truncate(health playerState))
            color $ Color4 255 255 255 (255 :: GLubyte)

          --render the crosshair
          renderCrosshair (textures gd)

          depthFunc              $= Just Less



--print a smiley representing the health of the player
printLife :: (Maybe TextureObject,DisplayList) -> Int -> IO()
printLife font life
   | life <= 0  = do printf ("(x_x)")
   | life <= 19  = do printf ("(T_T)")
   | life <= 19  = do printf ("(~_~)")
   | life <= 39  = do printf ("(-_-)")
   | life <= 59  = do printf ("(o_0)")
   | life <= 79  = do printf ("(o_o)")
   | otherwise = do printf ("(^_^)")
   where printf str = printFonts' 292 32 font 1 (str)


renderObjects :: IORef(Camera) -> HT.BasicHashTable String Model ->
   Frustum -> BSPMap  -> ObsObjState -> IO()
renderObjects camRef mdels frust mp oos
   | isRay oos        = renderRay oos
   | isProjectile oos = renderProjectile oos
   | isAICube oos     = renderEnemy camRef mdels frust mp oos
   | otherwise        = return ()


renderGun :: Camera -> HT.BasicHashTable String Model -> IO()
renderGun cam mdels = do
   Just weapon <- HT.lookup mdels "railgun"
   let (x,y,z)    = cpos cam
   let (vx,vy,vz) = viewPos cam
   unsafePreservingMatrix $ do
          --clear the depth buffer so the gun will appear
          --on top of the graphics in the scene
          clear [DepthBuffer ]

          --translate and rotate the gun so it is aligned with players view vector
          translate (Vector3 x (y+30) (z :: Double))
          let angle2 =
                    acos $ dotProd (normalise $ vectorSub (vx,0,vz) (x,0,z)) (1,0,0)
          case (vz > z ) of
                False -> rotate ((angle2*180/pi) :: GLdouble) (Vector3 0 1 0)
                True  -> rotate ((360 - (angle2*180/pi)) :: GLdouble) (Vector3 0 1 0)
          let angle1 =
                    acos $ dotProd (normalise $ vectorSub (vx,vy,vz) (x,y,z)) (0,1,0)
          rotate (90-(angle1*180/pi) :: GLdouble) (Vector3 0 0 1)
          rotate (-90 :: Double) (Vector3 1 0 0)
          translate (Vector3 (4.8) (-9.5) ((-20) :: Double))
          scale 2 2 (2 :: GLfloat)

          --setup the animation state and drw the model
          writeIORef (auxFunc2 (modelRef weapon)) Nothing
          drawModel (modelRef weapon,lowerState weapon)
   depthFunc              $= Just Always


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
                cullFace                    $= Just Front
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
  IORef(Camera) -> HT.BasicHashTable String Model ->
        Frustum -> BSPMap  -> ObsObjState -> IO()
renderEnemy camRef mdels frust bspmap (OOSAICube {oosOldCubePos = (x,y,z),
                                                                            oosCubeSize = (sx,sy,sz),
                                                                            oosCubeAngle        = angle,
                                                                            oosCubePitch        = p,
                                                                            upperAnim           = ua,
                                                                            fade                        = f,
                                                                            lowerAnim           = la,
                                                                            modelName           = name}) = do

   --perform a test to see if the object is visible from the player's location
   cam    <- readIORef camRef
   clustVis <- isObjectVisible bspmap (cpos cam) (x,y,z)
   case (clustVis) of
         False -> return()
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
                                 Just model <- HT.lookup mdels name
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
            False -> return ()