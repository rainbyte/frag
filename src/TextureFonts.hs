{- TextureFonts.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

This module handles the fonts and crosshairs of the game

--credits go to NeHe tutorials for their texturefonts tutorial

-}

module TextureFonts where

import Graphics.UI.GLUT
import Textures
import Data.Maybe
import Data.Char
import qualified Data.HashTable.IO as HT


-- build a display list for the fonts
buildFonts :: IO(Maybe TextureObject,DisplayList)
buildFonts = do
  lists <- genObjectNames $ fromIntegral (256 :: Int)
  let lists2 = concat $ splitList lists
  fontTex <- getAndCreateTexture "font"
  textureBinding Texture2D $= fontTex
  let cxcys = [((realToFrac(x `mod` 16))/16 ,
        ((realToFrac (x `div` 16))/16))| x<-[0..(255 :: Int)]]
  mapM_ genFontList (zip cxcys lists2)
  return (fontTex,head lists)


splitList :: [DisplayList] -> [[DisplayList]]
splitList [] = []
splitList list = (splitList (drop 16 list))++[(take 16 list)]


-- the steps needed to display every font
genFontList :: ((Float,Float),DisplayList) -> IO()
genFontList ((cx,cy),list) = do
   defineList list Compile $ do
     unsafeRenderPrimitive Quads $ do
         texCoord (TexCoord2 cx (1-cy-0.0625))
         vertex   (Vertex2 0 (16 :: Float))
         texCoord (TexCoord2 (cx+0.0625) (1-cy-0.0625))
         vertex   (Vertex2 16 (16 :: Float))
         texCoord (TexCoord2 (cx+0.0625) (1-cy-0.001))
         vertex   (Vertex2 16 (0 :: Float))
         texCoord (TexCoord2 cx (1-cy-0.001))
         vertex   (Vertex2 0 (0 :: Float))
     translate (Vector3 (14 :: Float) 0 0)


-- generates a displaylist for displaying large digits
buildBigNums :: IO DisplayList
buildBigNums = do
  lists <- genObjectNames $ fromIntegral (11 :: Int)
  texs <- getAndCreateTextures ["0","1","2","3","4","5","6","7","8","9","hyphen"]
  mapM_ genBigNumList (zip texs lists)
  return $ head lists


-- steps needed to render a big digit
genBigNumList :: (Maybe TextureObject,DisplayList) -> IO()
genBigNumList (tex,list) = do
   defineList list Compile $ do
      textureBinding Texture2D $= tex
      unsafeRenderPrimitive Quads $ do
         texCoord (TexCoord2  0 ( 1 :: Float))
         vertex   (Vertex2    0 ( 0 :: Float))
         texCoord (TexCoord2  0 ( 0 :: Float))
         vertex   (Vertex2    0 (45 :: Float))
         texCoord (TexCoord2  1 ( 0 :: Float))
         vertex   (Vertex2   30 (45 :: Float))
         texCoord (TexCoord2  1 ( 1 :: Float))
         vertex   (Vertex2   30 ( 0 :: Float))
      translate   (Vector3 (32 :: Float) 0 0)


-- renders a large digit
renderNum :: Float -> Float -> DisplayList -> Int -> IO()
renderNum x y (DisplayList base) n = unsafePreservingMatrix $ do
   loadIdentity
   texture Texture2D $= Enabled
   alphaFunc $= Just (Greater,0.1:: Float)
   let list = fmap toDList (show n)
   unsafePreservingMatrix $ do
      translate (Vector3 x y (0::Float))
      mapM_ callList list
   alphaFunc $= Nothing
   texture Texture2D $= Disabled
   where
      toDList c = DisplayList (base +(fromIntegral((ord c)-48)))


-- print a string starting at a 2D screen position
printFonts' :: Float -> Float ->
   (Maybe TextureObject,DisplayList)->
       Int-> String -> IO()
printFonts' x y (fontTex,DisplayList _) st string =
   unsafePreservingMatrix $ do
      loadIdentity
      texture Texture2D $= Enabled
      textureBinding Texture2D $= fontTex
      translate (Vector3 x y (0::Float))
      let lists = fmap (toDisplayList (128*(fromIntegral st))) string
      alphaFunc $= Just (Greater,0.1:: Float)
      mapM_ callList lists --(map DisplayList [17..(32:: GLuint)])
      alphaFunc $= Nothing
      texture Texture2D $= Disabled


-- sets up the orthographic mode so we can
-- draw at 2D screen coordinates
setUpOrtho :: IO a -> IO()
setUpOrtho func = do
   matrixMode   $= Projection
   unsafePreservingMatrix $ do
     loadIdentity
     ortho 0 640 0 480 (-1) 1
     matrixMode $= Modelview 0
     func
     matrixMode $= Projection
   matrixMode   $= Modelview 0

-- just renders the crosshair
renderCrosshair :: HT.BasicHashTable String (Maybe TextureObject) -> IO()
renderCrosshair texs = do
   Just crosshairTex <- HT.lookup texs "crosshair"
   texture Texture2D $= Enabled
   textureBinding Texture2D $= crosshairTex
   unsafePreservingMatrix $ do
      loadIdentity
      translate (Vector3 304 224 (0::Float))
      alphaFunc $= Just (Greater,0.1:: Float)
      unsafeRenderPrimitive Quads $ do
         texCoord (TexCoord2 0 (1 :: Float))
         vertex   (Vertex2 0 (0 :: Float))
         texCoord (TexCoord2 0 (0 :: Float))
         vertex   (Vertex2 0 (32 :: Float))
         texCoord (TexCoord2 1 (0 :: Float))
         vertex   (Vertex2 32 (32 :: Float))
         texCoord (TexCoord2 1 (1 :: Float))
         vertex   (Vertex2 32 (0 :: Float))
      alphaFunc $= Nothing
   texture Texture2D $= Disabled

toDisplayList ::  GLuint -> Char -> DisplayList
toDisplayList _ c = DisplayList (fromIntegral (ord c) - 31)



