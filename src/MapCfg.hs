-- MapCfg.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

{-

reads the level configuration files and map media files

-}


module MapCfg where

import BSP
import Camera
import Control.Exception ( bracket )
import qualified Data.HashTable.IO as HT
import Data.IORef
import Data.List (find)
import Data.Maybe
import IdentityList
import MD3
import Object
import Object
import ObjectBehavior
import Prelude
import System.IO hiding (withBinaryFile)


data ObjectConstructor =
   ConsCamera Camera |
   ConsAICube {startPosition :: (Double,Double,Double),
               size          :: (Double,Double,Double),
               wayPoints     :: [(Double,Double,Double)],
               modlName      :: String}
               deriving (Read,Show)


readMapCfg :: FilePath -> IO [IntermediateObject]
readMapCfg filepath = withBinaryFile filepath $ \handle -> do
   lnes <- readLines handle
   print lnes
   let objects  = map lines2ObjectCons lnes
   return $ map objectCons2IntermediateObjects objects

readMapMedia :: FilePath -> IO (IORef BSPMap,(HT.BasicHashTable String Model))
readMapMedia filepath = withBinaryFile filepath $ \handle -> do
   lnes <- readLines handle
   print lnes
   let levelModels = lines2LevelModels lnes
   let (MMap lvlName) = head levelModels
   bsp <- readBSP lvlName
   hash <- HT.fromList []
   mapM_ (readLevelModels hash) (tail levelModels)
   return (bsp,hash)


readLevelModels :: HT.BasicHashTable String Model -> LevelModel -> IO ()
readLevelModels hash (MWeapon name) =
   getWeaponModel hash name
readLevelModels hash (MPlayerModel name weaponName) =
   getModel hash name weaponName


getModel :: HT.BasicHashTable String Model -> String -> String -> IO ()
getModel hash name weaponName = do
   getWeaponModel hash weaponName
   Just weapon <- HT.lookup hash weaponName
   model       <- readModel name weapon
   HT.insert hash name model


getWeaponModel :: HT.BasicHashTable String Model -> String -> IO ()
getWeaponModel hash name = do
   model <- HT.lookup hash name
   case model of
      Just _ -> return ()
      Nothing -> do
                weaponModel <-
                  readWeaponModel
                    ("tga/models/weapons/"++name++".md3") ("tga/models/weapons/"++name++".shader")
                HT.insert hash name weaponModel


readLines :: Handle -> IO [String]
readLines handle = do
         eof <- hIsEOF handle
         case (eof) of
           False -> do
                     line <- hGetLine handle
                     lnes <- readLines handle
                     return (line:lnes)
           _     -> return []

withBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile filePath = bracket (openBinaryFile filePath ReadMode) hClose


data LevelModel = MWeapon String |
                  MPlayerModel String String |
                  MMap String deriving (Read,Show)


data IntermediateObject =
   ICamera ([(String, AnimState, AnimState)] ->
      [(ILKey, Message)] -> ILKey -> Object) |
   IAICube ((AnimState,AnimState) -> ILKey -> Object) String


lines2ObjectCons :: String -> ObjectConstructor
lines2ObjectCons str
 | head (words str) == "ConsCamera" = read str :: ObjectConstructor
 | head (words str) == "ConsAICube" = read str :: ObjectConstructor


lines2LevelModels :: [String] -> [LevelModel]
lines2LevelModels [] = []
lines2LevelModels (str:strs) = (read str): (lines2LevelModels  strs)


objectCons2IntermediateObjects :: ObjectConstructor -> IntermediateObject
objectCons2IntermediateObjects (ConsCamera cam) =
   ICamera (camera cam)
objectCons2IntermediateObjects c@(ConsAICube {}) =
   IAICube
     (aicube (startPosition c) (size c) (wayPoints c)(modlName c)) (modlName c)


toCompleteObjects ::
   [(String, AnimState, AnimState)] ->
      [IntermediateObject] -> [ILKey->Object]
toCompleteObjects animList iobjs =
   map (toCompleteObject animList) iobjs


toCompleteObject ::
   [(String, AnimState, AnimState)] ->
      IntermediateObject -> (ILKey->Object)
toCompleteObject animList (ICamera func) =
   func animList []
toCompleteObject animList (IAICube func modelname) =
   func (findModelAnim modelname animList)


findModelAnim ::
   String ->
      [(String,AnimState,AnimState)] ->
          (AnimState,AnimState)
findModelAnim name anms = (ua,la)
   where
      (_,ua,la) =
          fromJust $ find (\(x,_,_)->(x==name)) anms



