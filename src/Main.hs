{- Main.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

Main module

-}

module Main where


import TextureFonts
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import Data.Maybe
import Control.DeepSeq (force)
import Control.Monad
import qualified HGL
import FRP.Yampa
import Game
import Parser
import Object
import BSP
import Camera
import System.Exit (exitSuccess)
import Matrix
import MD3
import qualified Data.HashTable.IO as HT
import Frustum
import Data.List (find)
import Textures
import MapCfg
import Render

msInterval :: Int
msInterval = 16

clkRes :: Double
clkRes = 1000

data Input
  = KBMInput  { key :: Key,
                         keyState :: KeyState,
                         modifiers :: Modifiers,
                         pos :: Position}
  | MouseMove { pos :: Position }
 deriving Show

type OGLInput = Maybe Input
type WinInput = Event HGL.Event

main :: IO ()
main = do
   (progName,names) <-getArgsAndInitialize
   case names of
    []     -> printUsage progName
    [name] -> do createAWindow "FRAG" name
                 mainLoop
    _      -> printUsage progName

printUsage :: String -> IO ()
printUsage n = do putStrLn $ "Usage: "   ++ n ++ " <level>"
                  putStrLn $ "Example: " ++ n ++ " leveleg"

createAWindow :: String -> String -> IO ()
createAWindow windowName level = do
   initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBAMode]
   drawBuffer             $= BackBuffers
   initialWindowSize  $= Size 640 480
   createWindow windowName
   clear [ColorBuffer]
   viewport               $= (Position 0 0, Size 640 480)
   matrixMode             $= Projection
   loadIdentity
   perspective 70.0 (640/480) 10.0  4000.0
   matrixMode             $= Modelview 0
   loadIdentity
   depthFunc              $= Just Less
   texture Texture2D  $=  Enabled
   cullFace               $= Just Front
   cursor                 $= None

   --load our level objects from the *.cfg file
   iobjs <- readMapCfg (level ++ ".cfg")

   let cam = initCamera (80::Int,61::Int,60::Int) (80::Int,611::Int,59::Int) (0::Int,1::Int,0::Int)
   camRef <- newIORef cam

   --read the BSP files and player models specified in the *.med files
   (mapRef,modls) <- readMapMedia (level ++ ".med")

   listModels <- HT.toList modls
   animList   <- mapM getAnims listModels

   --complete the objects
   let objs = toCompleteObjects animList iobjs

   --build the fonts
   (tex,base)<- buildFonts
   numbase <- buildBigNums

   --create a hashmap  for textures
   texs <- HT.fromList []

   --create the crosshair
   crosshair <- getAndCreateTexture "crosshaira"
   HT.insert texs "crosshair" crosshair

   --set up the variables needed by our callbacks and game loop
   tme            <- get elapsedTime
   lasttime        <- newIORef tme
   lastDTime       <- newIORef tme
   lastDTime2      <- newIORef tme
   fpsc1                   <- newIORef(0,0)
   fps1            <- newIORef(0,0,0)
   newIORef(0::Int)
   _      <- newIORef tme
   --hold new keyboard input
   newInput        <- newIORef Nothing
   inpState        <- newIORef False
   --hold the new mouse input
   newMouseInput  <- newIORef Nothing
   --lock the mouse or not
   lck             <- newIORef True
   (_, _)       <- getWinInput
                                    (lasttime, (newInput,newMouseInput)) inpState True tme
   hasReact        <- newIORef False
   mp             <- readIORef mapRef

   let gd = GameData {
          gamemap        = mapRef,
          models         = modls,
          textures       = texs,
          camera         = camRef,
          lastDrawTime   = lastDTime,
          lastDrawTime2  = lastDTime2,
          hasReacted     = hasReact,
          fonts          = (tex,base),
          nbase          = numbase,
          lock           = lck,
          fpsc           = fpsc1,
          fpss           = fps1,
          nems           = length objs - 1
   }


   rh <-
         reactInit
            (initr lasttime (newInput,newMouseInput) inpState)
            (actuate gd)
            (repeatedly 0.016 () &&& (parseWinInput >>> game mp objs))


   --set up the callbacks
   displayCallback              $= display
   keyboardMouseCallback $= Just (keyboardMouse newInput newMouseInput lck)
   motionCallback               $= Just (dragMotion    newMouseInput)
   passiveMotionCallback $= Just (mouseMotion   newMouseInput)
   idleCallback         $=
        Just (idle lasttime (newInput,newMouseInput)
           hasReact (tex,base) inpState rh)

   where getAnims (x,y) = do
            us <- readIORef (upperState y)
            ls <- readIORef (lowerState y)
            return (x,us,ls)




-------------------------------------------------------------------------------
-- functions to connect Haskell and Yampa


actuate :: GameData -> ReactHandle a b ->
   Bool -> (Event (), [ObsObjState]) -> IO Bool
actuate  gd _ _ (e, noos) = do
   when (force noos `seq` isEvent e)
        (render gd noos)
   return False


initr :: IORef Int -> (IORef OGLInput, IORef OGLInput) -> IORef Bool
      -> IO (WinInput,WinInput)
initr lasttime newInput inpState = do
         tme     <- get elapsedTime
         writeIORef lasttime 1
         (_, inp) <- getWinInput (lasttime, newInput)  inpState True tme
         case inp of
            Just i -> return i
            Nothing -> return (noEvent,noEvent)

-------------------------------------------------------------------------------
-- graphics

render :: GameData -> [ObsObjState] -> IO()
render gd oos = do
  -- get the last time we drew the screen
  lastime <- readIORef (lastDrawTime gd)
  -- the current time
  tme   <- get elapsedTime
  l <- readIORef (lock gd)

  -- if the last time is at least greater than 0.016
  -- seconds and the mouse is locked reset the position
  -- to the middle of the screen
  when (realToFrac ((tme - lastime) :: Int) / 1000 >= (1/60) && l) $ do
      pointerPosition $= Position 320 240
      writeIORef (lastDrawTime gd) tme

  _ <- readIORef (lastDrawTime2 gd)
  _ <- readIORef (hasReacted gd)

  --case (((realToFrac (time - lastTime2))/1000) <= (1/60)) of
  if True
        then do
                    -- initial setup
                    clear [ ColorBuffer, DepthBuffer ]
                    loadIdentity

                    --find the camera and set our view
                    let playerState = findCam oos
                    case cood playerState of
                         [] -> return ()
                         _ -> print (getPos (cood playerState))
                    let cam = setCam playerState
                    writeIORef (camera gd) cam
                    cameraLook cam

                    --render the map
                    renderBSP (gamemap gd) (cpos cam)

                    --render the objects
                    mp <- readIORef (gamemap gd)
                    frust <- getFrustum
                    mapM_ (renderObjects (camera gd) (models gd) frust mp) oos

                    --render the gun
                    renderGun cam (models gd)

                    --set up orthographics mode so we can draw the fonts
                    renderHud gd playerState (length oos) tme

                    writeIORef (lastDrawTime2 gd) tme
                    writeIORef (hasReacted gd) False
                    swapBuffers
        else do
                    writeIORef (lastDrawTime2 gd) tme
                    return()


getPos :: [(Double,Double,Double)] -> [(Int,Int,Int)]
getPos coords = fmap ints l
        where
          l = fmap (vectorAdd (0,90,0)) coords
          ints (x,y,z)= (truncate x,truncate y,truncate z)


findCam :: [ObsObjState] -> ObsObjState
findCam states = fromJust $ find isCamera states


setCam :: ObsObjState -> Camera
setCam OOSCamera {oldCam = cam, newCam = _} =  cam


-------------------------------------------------------------------------------
--callbacks

display :: (Monad t) => t ()
display = return ()


keyboardMouse ::
   IORef OGLInput -> IORef OGLInput -> IORef Bool -> KeyboardMouseCallback
keyboardMouse _ _ lck (Char 'z') _ _ _  = do
   readIORef lck
   writeIORef lck False
keyboardMouse _ _ lck (Char 'x') _ _ _  = do
   _ <- readIORef lck
   writeIORef lck True
keyboardMouse _ _ _ (Char '\27') _ _ _  = exitSuccess
keyboardMouse _ newMouse _ newKey@(MouseButton _)
   newKeyState newModifiers newPosition =
         writeIORef  newMouse (Just KBMInput{
                                                        key             = newKey,
                                                        keyState        = newKeyState,
                                                        modifiers = newModifiers,
                                                        pos             = newPosition})
keyboardMouse newInput _ _ newKey
   newKeyState newModifiers newPosition =
         writeIORef newInput (Just KBMInput{
                                                        key              = newKey,
                                                        keyState         = newKeyState,
                                                        modifiers  = newModifiers,
                                                        pos              = newPosition})


mouseMotion :: IORef OGLInput -> MotionCallback
mouseMotion newInput newCursorPos = do
   lst <- readIORef newInput
   case lst of
         (Just inp) -> writeIORef newInput (Just inp)
         _          -> writeIORef newInput (Just MouseMove {pos=newCursorPos})


dragMotion :: IORef OGLInput -> MotionCallback
dragMotion newInput newCursorPos = do
   lst <- readIORef newInput
   case lst of
         (Just inp) -> writeIORef newInput (Just inp)
         _          -> writeIORef newInput (Just MouseMove {pos=newCursorPos})


idle :: IORef Int -> (IORef OGLInput, IORef OGLInput) -> IORef Bool
     -> (Maybe TextureObject, DisplayList) -> IORef Bool
     -> ReactHandle (WinInput, WinInput) (Event (), [Object.ObsObjState])
     -> IO ()
idle lasttime newInput hasreacted _ inputState rh = do
   lTime <- readIORef lasttime
   currenttime <- get elapsedTime
   when (currenttime - lTime >= 16) $ do
            (dt, input) <-
                  getWinInput (lasttime,newInput) inputState True currenttime
            react rh (dt,input)
            writeIORef hasreacted True
            writeIORef lasttime currenttime
            return ()





-------------------------------------------------------------------------------
-- input handling
-- mimic HGL so the parser from Space Invaders can be used
getWinInput :: (IORef Int, (IORef OGLInput, IORef OGLInput)) -> IORef Bool
            -> Bool -> Int-> IO (DTime, Maybe (WinInput, WinInput))
getWinInput (lasttime, (newInput,newMouseInput)) inpState _ currenttime = do
   lTime <- readIORef lasttime
   newIn <- readIORef newInput
   newMouseIn <- readIORef newMouseInput
   writeIORef newInput Nothing
   writeIORef newMouseInput Nothing

   -- we try to get rid of redundant events
   hasReset <- readIORef inpState
   mmin <-
         case (coalesce newIn, coalesce newMouseIn,hasReset) of
           (NoEvent, NoEvent,_) -> return Nothing
           (NoEvent, Event HGL.MouseMove {HGL.pt =HGL.Point (320,240)},False)-> do
                  writeIORef inpState False
                  return Nothing
           (NoEvent, Event HGL.MouseMove {HGL.pt =HGL.Point (320,240)},True) -> do
                  writeIORef inpState False
                  return $  Just (coalesce newIn,coalesce newMouseIn)
           (NoEvent, Event HGL.MouseMove {HGL.pt = _ },True) -> do
                  writeIORef inpState True
                  return $ Just (coalesce newIn,coalesce newMouseIn)
           (NoEvent, Event HGL.MouseMove {HGL.pt = _ },False) -> do
                  writeIORef inpState True
                  return $ Just (coalesce newIn,coalesce newMouseIn)
           (Event _, Event HGL.MouseMove {HGL.pt =HGL.Point (320,240)},False)-> do
                  writeIORef inpState False
                  return $ Just (coalesce newIn,noEvent)
           (Event _, Event HGL.MouseMove {HGL.pt =HGL.Point (320,240)},True) -> do
                  writeIORef inpState False
                  return $ Just (coalesce newIn,coalesce newMouseIn)
           (Event _, Event HGL.MouseMove {HGL.pt = _},True) -> do
                  writeIORef inpState True
                  return $ Just (coalesce newIn,coalesce newMouseIn)
           (Event _, Event HGL.MouseMove {HGL.pt = _},False) -> do
                  writeIORef inpState True
                  return $ Just (coalesce newIn,coalesce newMouseIn)
           (_,_,_) -> do
                  writeIORef inpState True
                  return $ Just (coalesce newIn,coalesce newMouseIn)

   return (fromIntegral (currenttime - lTime) / clkRes, mmin)


coalesce :: OGLInput -> WinInput
coalesce Nothing = NoEvent
coalesce (Just KBMInput {key      = (MouseButton button),
                         keyState = ks,
                         pos      = p}) =
   Event HGL.Button { HGL.pt     = pos2Point p,
                      HGL.isLeft = isMBLeft (MouseButton button),
                      HGL.isDown = isKeyDown ks}
coalesce (Just KBMInput {key      = (Char a),
                         keyState = ks}) =
   Event HGL.Char {HGL.char = a, HGL.isDown = isKeyDown ks}
coalesce (Just MouseMove {pos= p}) =
   Event HGL.MouseMove { HGL.pt = pos2Point p }
coalesce _ = NoEvent


pos2Point :: Position -> HGL.Point
pos2Point (Position a b) = HGL.Point (fromIntegral a, fromIntegral b)


isMBLeft :: Key -> Bool
isMBLeft (MouseButton LeftButton) = True
isMBLeft _ = False


isKeyDown :: KeyState -> Bool
isKeyDown Down = True
isKeyDown _     = False


filter :: Eq a => a -> a -> Maybe a
filter a b =
   if a == b then
         Just a
   else
         Nothing


