{-# LANGUAGE BangPatterns #-}

{- BSP.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

A module for loading Quake 3 BSP files

source code in C++ can be found at

http://www.paulsprojects.net/opengl/q3bsp/q3bsp.html

credits also go to Ben Humphrey for his excellent BSP tutorial

I might split this module up. Perhaps rendering performance
could be increased if i used Vertex Buffer Objects instead of
vertex arrays?

-}

module BSP (
   BSPMap(..),
   readBSP,
   renderBSP,
   Tree(..),
   BSPNode(..),
   BSPLeaf(..),
   BSPBrush(..),
   BSPBrushSide(..),
   isObjectVisible
  ) where

import Data.IORef
import Control.Exception ( bracket )
import Control.Monad ( when, unless )
import System.IO hiding (withBinaryFile)
import System.IO.Error ( mkIOError, eofErrorType )
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.List
import Data.Typeable
import Graphics.UI.GLUT
import BitSet
import Textures
import Data.Array
import qualified Data.Array.MArray  as Arr (readArray, newListArray)
import qualified Data.Array.IO as IOArr hiding (readArray, newListArray)
import Frustum
import Matrix
import Curves
import Data.Maybe

-------------------------------------------------------------------------------
-- lump directory indices

-- Stores texture information
kTextures :: Int
kTextures    = 1

-- Stores the splitting planes
kPlanes     :: Int
kPlanes      = 2

-- Stores the BSP nodes
kNodes      :: Int
kNodes       = 3

-- Stores the leafs of the nodes
kLeafs :: Int
kLeafs = 4

-- Stores the leaf's indices into the faces
kLeafFaces :: Int
kLeafFaces = 5

-- Stores the leaf's indices into the brushes
kLeafBrushes :: Int
kLeafBrushes = 6

-- Stores the brushes info (for collision)
kBrushes :: Int
kBrushes = 8

-- Stores the brush surfaces
kBrushSides :: Int
kBrushSides = 9

-- Stores the level vertices
kVertices :: Int
kVertices = 10

-- Stores the level indices
kIndices :: Int
kIndices = 11

-- Stores the faces for the level
kFaces :: Int
kFaces = 13

-- Stores the lightmaps for the level
kLightmaps :: Int
kLightmaps = 14

-- Stores PVS and cluster info (visibility)
kVisData      :: Int
kVisData    = 16

-- A constant to store the number of lumps
kMaxLumps     :: Int
kMaxLumps   = 17

-------------------------------------------------------------------------------
-- types used in this module

data BSPMap = BSPMap {
     vertexData :: !VertexArrays,
     vindices   :: !(Ptr GLint),
     leaves     :: ![BSPLeaf],
     tree     :: !Tree,
     visData    :: !(Maybe BSPVisData),
     bitset     :: !BitSet
}

type VertexArrays = (Ptr Float,Ptr Float,Ptr Float,Ptr Float,Ptr Word8)

data BSPLeaf = BSPLeaf {
    cluster    :: !Int,
    area       :: Int,
    leafMin    :: (Double,Double,Double),
    leafMax    :: (Double,Double,Double),
    leafface     :: Int,
    numOfLeafFaces   :: Int,
    leafBrush    :: Int,
    numOfLeafBrushes :: Int,
    leafFaces    :: [BSPFace],
    leafBrushes  :: [BSPBrush]
} deriving Show

data BSPFace = BSPFace {
    -- The index into the texture array
    textureObj     :: Maybe TextureObject,
    -- The index for the effects (or -1 = n/a)
    effect         :: Int,
    -- 1=polygon, 2=patch, 3=mesh, 4=billboard
    faceType       :: Int,
    -- The starting index into this face's first vertex
    startVertIndex :: Int,
    -- The number of vertices for this face
    numOfVerts     :: Int,
    -- The starting index into the indices array for this face
    startIndex     :: Int,
    -- The number of indices for this face
    numOfIndices   :: GLint,
    -- The texture index for the lightmap
    lightmapObj    :: Maybe TextureObject,
    -- The face's lightmap corner in the image
    lMapCorner     :: (Int,Int),
    -- The size of the lightmap section
    lMapSize       :: (Int,Int),
    -- The 3D origin of lightmap.
    lMapPos        :: (Float,Float,Float),
    -- The 3D space for s and t unit vectors.
    lMapVecs       :: [(Float,Float,Float)],
    -- The face normal.
    vNormal        :: (Float,Float,Float),
    -- The bezier patch dimensions.
    size           :: (Int,Int),
    faceNo         :: Int,
    patch          :: [BSPPatch],
    arrayPtrs      :: VertexPointers
} deriving Show

data BSPBrush = BSPBrush {
    brushSide      :: Int,
    numOfBrushSides    :: Int,
    brushSides     :: [BSPBrushSide],
    bTextureID     :: Int,
    textureType    :: Int
} deriving Show

data BSPBrushSide = BSPBrushSide {
    bsPlane     :: Int,
    bsPlaneNorm    :: (Double,Double,Double),
    bsPlaneDist    :: Double,
    bsTextureID    :: Int
} deriving Show

data Tree  = Leaf BSPLeaf | Branch BSPNode Tree Tree

data BSPNode = BSPNode {
    planeNormal :: (Double,Double,Double),
    dist     :: Double,
    front    :: Int,
    back     :: Int,
    nodeMin  :: (Int,Int,Int),
    nodeMax  :: (Int,Int,Int)
} deriving Show

data BSPVisData = BSPVisData {
    numOfClusters :: Int,
    bytesPerCluster :: Int,
    bitSets   :: IOArr.IOUArray Int Bool
}

data BSPLump = BSPLump {
    offset :: Int,
    len  :: Int
}  deriving Show

data BSPHeader = BSPHeader {
    strID   :: String,
    version :: Int
}  deriving Show

data BSPTexInfo = BSPTexInfo {
    strName  :: String,
    flags    :: Int,
    contents :: Int
} deriving Show

type VertexData = ([Float],[Float],[Float],[Float],[Word8])

type VertexPointers = (Ptr GLfloat, Ptr GLfloat, Ptr GLfloat, Ptr GLint)

type BSPLeafFace = Int

data BSPPlane = BSPPlane {
    pNormal  :: (Double,Double,Double),
    distance :: Double
} deriving Show

-------------------------------------------------------------------------------
--BSP rendering

renderBSP :: IORef(BSPMap) -> (Double,Double,Double)-> IO()
renderBSP mapRef (x,y,z) = do
   activeTexture $= TextureUnit 0
   clientActiveTexture $= TextureUnit 0
   clientState TextureCoordArray $= Enabled
   texture Texture2D $= Enabled

   activeTexture $= TextureUnit 1
   clientActiveTexture $= TextureUnit 1
   clientState TextureCoordArray $= Enabled
   texture Texture2D $= Enabled

   mp <- readIORef mapRef
   leaf <- findLeaf (x,y,z) (tree mp)
   renderBSP' leaf mp
   return ()


-- given a position finds a in the tree where the position lies in
findLeaf :: (Double, Double,Double) -> Tree -> IO BSPLeaf
findLeaf (x,y,z) (Branch node left right) =
   let (px,py,pz) = planeNormal node
       d = dist node
       dstnc = (px*x)+(py*y)+(pz*z)-d
       branch = if dstnc >= 0 then left else right
   in findLeaf (x,y,z) branch
findLeaf (_,_,_) (Leaf leaf) = return leaf


-- we are actually going across all the leaves in the tree
-- instead of walking the tree and pushing the leaves that
-- we want to render into a stack
renderBSP' :: BSPLeaf -> BSPMap -> IO()
renderBSP' leaf mp = do
   sze <- sizeBS $ bitset mp
   newbs <- emptyBS sze
   frstm <- getFrustum
   mapM_ (renderLeaves frstm newbs visFunc mp) (leaves mp)
   renderBSPCleanUp
       where visFunc = (isClusterVisible (visData mp) (cluster leaf))


-- we have to reset the openGL state after rendering
renderBSPCleanUp :: IO()
renderBSPCleanUp = do
    activeTexture $= TextureUnit 1
    clientState TextureCoordArray $= Disabled
    texture Texture2D $= Disabled
    activeTexture $= TextureUnit 0
    clientActiveTexture $= TextureUnit 0
    clientState TextureCoordArray $= Disabled
    texture Texture2D $= Disabled


-- renders a BSP leaf if it is visible
renderLeaves ::
   Frustum -> BitSet -> (Int -> IO Bool) -> BSPMap -> BSPLeaf -> IO()
renderLeaves  frstm bitSet func mp leaf  = do
   clusterVisible <- func (cluster leaf)
   case (clusterVisible) of
      True -> case ((boxInFrustum frstm (leafMin leaf) (leafMax leaf))) of
         True -> renderFaces bitSet mp (leafFaces leaf)
         _ -> return ()
      _   -> return()


-- is an object visible
isObjectVisible :: BSPMap -> Vec3 -> Vec3 -> IO Bool
isObjectVisible bsp (x,y,z) (ox,oy,oz) = do
   currentLeaf <- findLeaf (x,y,z) (tree bsp)
   objectLeaf  <- findLeaf (ox,oy,oz) (tree bsp)
   isVis   <- isClusterVisible
     (visData bsp) (cluster currentLeaf) (cluster objectLeaf)
   return (isVis)

isClusterVisible ::Maybe BSPVisData -> Int -> Int -> IO Bool
isClusterVisible (Just visdata) current target
    | current < 0 = do
     return True
    | target  < 0 = do
     return False
    | otherwise   = do
     Arr.readArray
       (bitSets visdata)
          (((bytesPerCluster visdata)*current*8) + target)
isClusterVisible _ _ _ = return False

renderFaces :: BitSet -> BSPMap -> [BSPFace] -> IO()
renderFaces _ _ [] = return ()
renderFaces bitSet mp (face:faces) = do
    isSet <- isSetBS bitSet (faceNo face)
    unless isSet $ do
        setBS bitSet (faceNo face)
        case faceType face of
            1 -> renderPolygonFace face (vertexData mp) (vindices mp)
            2 -> renderPatches face
            3 -> renderMeshFace face (vertexData mp) (vindices mp)
            _ -> pure ()
    renderFaces bitSet mp faces




-------------------------------------------------------------------------------
-- surface rendering

-- renders a polygon surface
renderPolygonFace :: BSPFace -> VertexArrays -> Ptr GLint -> IO ()
renderPolygonFace face (_,_,_,_,_) _ =  do
   let (a,b,c,d) = arrayPtrs face
   arrayPointer VertexArray $=
      VertexArrayDescriptor 3 Float 0 a
   clientState VertexArray $= Enabled

   activeTexture $= TextureUnit 0
   clientActiveTexture $= TextureUnit 0
   arrayPointer TextureCoordArray $=
      VertexArrayDescriptor 2 Float 0 b
   textureBinding Texture2D $= (textureObj face)

   activeTexture $= TextureUnit 1
   clientActiveTexture $= TextureUnit 1
   arrayPointer TextureCoordArray $=
      VertexArrayDescriptor 2 Float 0 c
   textureBinding Texture2D $= (lightmapObj face)

   drawRangeElements Triangles (0,(numOfIndices face))
      (numOfIndices face) UnsignedInt d
   --drawElements Triangles (numOfIndices face) UnsignedInt d


-- renders a mesh face
renderMeshFace :: BSPFace -> VertexArrays -> Ptr GLint -> IO ()
renderMeshFace face (vertexPtr,texturePtr,c,_,_)  vIndex =  do
   startVIndex <- return (startVertIndex face)
   arrayPointer VertexArray $=
      VertexArrayDescriptor 3 Float 0
         (plusPtr vertexPtr (12*(startVIndex)))
   clientState VertexArray  $= Enabled

   activeTexture $= TextureUnit 0
   clientActiveTexture $= TextureUnit 0
   arrayPointer TextureCoordArray $=
      VertexArrayDescriptor 2 Float 0
         (advancePtr texturePtr (2*(startVertIndex face)))
   clientState TextureCoordArray $= Enabled
   texture Texture2D $= Enabled
   textureBinding Texture2D $= (textureObj face)

   activeTexture $= TextureUnit 1
   clientActiveTexture $= TextureUnit 1
   arrayPointer TextureCoordArray $=
      VertexArrayDescriptor 2 Float 0 (plusPtr c (8*(startVIndex)))
   clientState TextureCoordArray $= Enabled
   texture Texture2D $= Enabled
   textureBinding Texture2D $= (lightmapObj face)

   drawRangeElements Triangles (0,fromIntegral (numOfVerts face))
      (numOfIndices face ) UnsignedInt (plusPtr vIndex (4*(startIndex face)))


-- renders patch surfaces
renderPatches :: BSPFace -> IO()
renderPatches face = do
   mapM_ (renderPatch face) (patch face)


renderPatch :: BSPFace -> BSPPatch -> IO()
renderPatch face bsppatch=  do
   arrayPointer VertexArray $=
      VertexArrayDescriptor 3 Float 28 (patchPtr bsppatch)
   clientState VertexArray $= Enabled

   activeTexture $= TextureUnit 0
   clientActiveTexture $= TextureUnit 0
   arrayPointer TextureCoordArray $=
      VertexArrayDescriptor 2 Float 28 (plusPtr (patchPtr bsppatch) 12)
   clientState TextureCoordArray $= Enabled
   texture Texture2D $= Enabled
   textureBinding Texture2D $= (textureObj face)

   activeTexture $= TextureUnit 1
   clientActiveTexture $= TextureUnit 1
   arrayPointer TextureCoordArray $=
      VertexArrayDescriptor 2 Float 28 (plusPtr (patchPtr bsppatch) 20)
   clientState TextureCoordArray $= Enabled
   texture Texture2D $= Enabled
   textureBinding Texture2D $= (lightmapObj face)

   multiDrawElements TriangleStrip (numIndexPtr bsppatch)
      UnsignedInt (indexPtrPtr bsppatch)
         (fromIntegral (patchLOD bsppatch))



-------------------------------------------------------------------------------
-- reading functions

-- reads a BSP file
readBSP :: FilePath -> IO(IORef(BSPMap))
readBSP filePath = withBinaryFile filePath $ \handle -> do
   readHeader handle
   lumps           <- mapM (readLump handle)
                        [ 0 .. (kMaxLumps -1)] :: IO [BSPLump]
   (a,b,c,d,e)     <- readVertices handle lumps
   indcs           <- readIndices  handle lumps
   newbitset       <- createBitset lumps
   newVertexArrays <- dataToPointers (a,b,c,d,e)
   indexPtr        <- newArray indcs
   newNodes        <- readNodes handle lumps
   newLeaves       <- readLeaves handle lumps newVertexArrays indexPtr
   newVisData      <- readVisData handle lumps
   let leafArray = listArray (0,((length newLeaves)-1)) newLeaves
   let nodeArray = listArray (0,((length newNodes)-1)) newNodes
   ntree <- constructTree nodeArray leafArray 0
   bsp <-  (newIORef ( BSPMap  {
                          vertexData = newVertexArrays,
                          vindices   = indexPtr,
                          leaves     = (reverse newLeaves),
                          tree       = ntree,
                          visData    = newVisData,
                          bitset     = newbitset
   }))
   return bsp

constructTree :: Array Int BSPNode -> Array Int BSPLeaf -> Int -> IO(Tree)
constructTree nodes lvs ind = do
   case (ind >= 0) of
      True  -> do
         let currentNode = (nodes ! ind)
         leftNode  <- constructTree nodes lvs (front currentNode)
         rightNode <- constructTree nodes lvs (back currentNode)
         return (Branch currentNode leftNode rightNode)
      False -> do
         let currentLeaf = (lvs ! ((-1)*(ind+1)))
         return (Leaf currentLeaf)


createBitset :: [BSPLump] -> IO BitSet
createBitset lumps = do
   (_,lngth) <- (getLumpData (lumps !! kFaces))
   newbitset  <- emptyBS (lngth `div` 104)
   return newbitset




-- - - - - - - - - - - - - - - - - - -
-- reads the BSP files header information


readHeader :: Handle -> IO BSPHeader
readHeader handle = do
   buf <- mallocBytes 4
   hGetBuf handle buf 4
   iD <- mapM (peekByteOff buf) [ 0 ..  3] :: IO [CChar]
   hGetBuf handle buf cIntSize
   ver <- (peek (castPtr buf :: Ptr CInt)) :: IO CInt
   free buf
   return (BSPHeader {
              strID   = map castCCharToChar iD,
              version = (fromIntegral ver)})




-- - - - - - - - - - - - - - - - - - -
-- reads the lumps in our bsp

readLump :: Handle -> Int -> IO BSPLump
readLump handle _ = do
   buf  <- mallocBytes cIntSize
   hGetBuf handle buf cIntSize
   offs <- (peek (castPtr buf :: Ptr CInt)) :: IO CInt
   hGetBuf handle buf cIntSize
   l    <- (peek (castPtr buf :: Ptr CInt)) :: IO CInt
   free buf
   return (BSPLump {offset = (fromIntegral offs),
                    len    = (fromIntegral l)})

getLumpData :: BSPLump -> IO (Int, Int)
getLumpData lump = return (offset lump,len lump)

-- - - - - - - - - - - - - - - - - - -
-- reads the nodes

readNodes :: Handle -> [BSPLump] -> IO [BSPNode]
readNodes handle lumps = do
   planes <- readPlanes handle lumps
   let planeArray = listArray (0,((length planes)-1)) planes
   (offst,lngth) <- (getLumpData (lumps !! kNodes))
   offs <- getOffsets lngth offst 36
   nodes <- mapM (readNode handle planeArray) offs
   return  nodes

readNode :: Handle -> Array Int BSPPlane -> Int -> IO (BSPNode)
readNode handle planeArray offst = do
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes 4
   let getCInt  =
          getAndPeek handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
   let getCInts =
          getAndPeeks handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
   let ints    = fmap toInts (getCInts 3)
   let get3Ints   = fmap get3t ints
   plnIndex  <- getCInt
   frt  <- getCInt
   bck  <- getCInt
   nMin <- get3Ints
   nMax <- get3Ints
   let pln = planeArray ! (fromIntegral plnIndex)
   return $ BSPNode {
               planeNormal = (pNormal pln),
               dist        = (distance pln),
               front       = fromIntegral frt,
               back        = fromIntegral bck,
               nodeMin     = nMin,
               nodeMax     = nMax
            }




-- - - - - - - - - - - - - - - - - - -
-- reads the planes in the nodes

readPlanes :: Handle -> [BSPLump] -> IO [BSPPlane]
readPlanes handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kPlanes))
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes lngth
   hGetBuf handle buf lngth
   let ptrs = getPtrs buf lngth 16
   planes <- mapM readPlane ptrs
   free buf
   return planes

readPlane :: Ptr a -> IO (BSPPlane)
readPlane ptr = do
   [e1,e2,e3,e4] <- getFloats ptr 4
   return $ BSPPlane {
        pNormal  = (fromRational (toRational e1),
           fromRational (toRational e3),
           fromRational (toRational ((-1)*e2))),
        distance = (fromRational (toRational e4))
   }




-- - - - - - - - - - - - - - - - - - -
-- reads the leaves

readLeaves :: Handle -> [BSPLump] -> VertexArrays -> Ptr GLint -> IO [BSPLeaf]
readLeaves handle lumps vertArrays indcs = do
   faces              <- readFaces handle lumps vertArrays  indcs
   let faceArray      =  listArray (0,((length faces)-1))  faces
   leaffaces          <- readLeafFaces handle lumps
   let leafFaceArray  =  listArray (0,((length leaffaces)-1)) leaffaces
   brushes            <- readBrushes handle lumps
   let brushArray     =  listArray (0,((length brushes)-1)) brushes
   leafbrushes        <- readLeafBrushes handle lumps
   let leafBrushArray =  listArray (0,((length leafbrushes)-1)) leafbrushes
   (offst,lngth)    <- getLumpData (lumps !! kLeafs)
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes lngth
   hGetBuf handle buf lngth
   let ptrs = getPtrs buf lngth 48
   nodes <-
      mapM (readLeaf leafFaceArray faceArray leafBrushArray brushArray) ptrs
   free buf
   return nodes


readLeaf ::
   Array Int Int -> Array Int BSPFace ->
      Array Int Int -> Array Int BSPBrush ->Ptr a ->IO (BSPLeaf)
readLeaf leafFaceArray faceArray leafBrushArray brushArray ptr = do
   [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12] <- getInts ptr 12
   let leafIndices  = map (leafFaceArray !) [((e9+e10)-1),((e9+e10)-2)..e9]
   let faceList     = map (faceArray !) leafIndices
   let brushIndices = map (leafBrushArray !) [e11..(e11+e12-1)]
   let brushList    = map (brushArray !) brushIndices
   return $ BSPLeaf {
        cluster          = e1,
        area             = e2,
        leafMin          = (realToFrac e3,
                            realToFrac e5,
                            realToFrac ((-1)*e4)),
        leafMax          = (realToFrac e6,
                            realToFrac e8,
                            realToFrac ((-1)*e7)),
        leafface         = e9,
        numOfLeafFaces   = e10,
        leafBrush        = e11,
        numOfLeafBrushes = e12,
        leafFaces        = faceList,
        leafBrushes      = brushList
   }




-- - - - - - - - - - - - - - - - - - -
-- huge function for reading the faces in our leaves


readFaces :: Handle -> [BSPLump] -> VertexArrays -> Ptr GLint -> IO [BSPFace]
readFaces handle lumps vertArrays indcs = do
   lightMaps         <- readLightMaps handle lumps
   let lightMapArray =  listArray (0,((length lightMaps)-1)) lightMaps
   texInfos          <- readTexInfos handle lumps
   texFileNames      <- return (map strName texInfos)
   texObjs           <- getAndCreateTextures texFileNames
   let texObjArray   =  listArray (0,((length texObjs)-1)) texObjs
   (offst,lngth)   <- (getLumpData (lumps !! kFaces))
   offs              <- getOffsets lngth offst 104
   faces             <- mapM
     (readFace handle offst lightMapArray texObjArray vertArrays indcs) offs
   return  faces


readFace ::
  Handle -> Int -> Array Int TextureObject -> Array Int (Maybe TextureObject)->
     VertexArrays -> Ptr GLint -> Int ->  IO (BSPFace)
readFace handle origin  lightmaps textures
  vertArrays@(a1,b1,c1,_,_) indcs offst = do
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes 4
   let getCInts   =
          getAndPeeks handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
   let getCFloats =
          getAndPeeks handle (castPtr buf :: Ptr CFloat) (undefined :: CFloat)
   let ints       = fmap toInts (getCInts 4)
   let get4Ints   = fmap get4t ints
   let floats     = fmap toFloats (getCFloats 3)
   let get3Floats = fmap get3t floats
   let twoInts    = fmap toInts (getCInts 2)
   let get2Ints   = fmap get2t twoInts
   (a,b,c,d) <- get4Ints
   (e,f,g,h) <- get4Ints
   (i,j,k,l) <- get4Ints
   lMPos     <- get3Floats
   lMVec1    <- get3Floats
   lMVec2    <- get3Floats
   norms     <- get3Floats
   sz        <- get2Ints
   free buf
   bspPatch <- checkForPatch c d sz vertArrays
   return BSPFace {
    textureObj     = textures ! a,
    effect         = b,
    faceType       = c,
    startVertIndex = d,
    numOfVerts     = e,
    startIndex     = f,
    numOfIndices   = fromIntegral g,
    lightmapObj    = fixLightmap h lightmaps,
    lMapCorner     = (i, j),
    lMapSize       = (k, l),
    lMapPos        = lMPos,
    lMapVecs       = [lMVec1,lMVec2],
    vNormal        = norms,
    size           = sz,
    faceNo         = (offst - origin)`div` 104,
    patch          = bspPatch,
    arrayPtrs      = (plusPtr a1 (12*d),
                      plusPtr b1 (8*d),
                      plusPtr c1 (8*d),
                      plusPtr indcs (4*f))
    }


-- - - - - - - - - - - - - - - - - - -
-- reads the leafaces that refer to the faces

readLeafFaces :: Handle -> [BSPLump] -> IO [BSPLeafFace]
readLeafFaces handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kLeafFaces))
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes lngth
   hGetBuf handle buf lngth
   leaffaces <- getInts buf (lngth `div` 4)
   free buf
   return leaffaces

-- - - - - - - - - - - - - - - - - - -
-- reads the brushes

readBrushes :: Handle -> [BSPLump] -> IO [BSPBrush]
readBrushes handle lumps = do
   brushsides         <- readBrushSides handle lumps
   let brushSideArray =  listArray (0,((length brushsides)-1)) brushsides
   texInfos           <- readTexInfos handle lumps
   let texInfoArray   =  listArray (0,((length texInfos)-1)) texInfos
   (offst,lngth)    <- (getLumpData (lumps !! kBrushes))
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf                <- mallocBytes (lngth)
   hGetBuf handle buf (lngth)
   let ptrs           = getPtrs buf (lngth) 12
   brushes            <- mapM (readBrush brushSideArray texInfoArray) ptrs
   free buf
   return brushes

readBrush ::  Array Int BSPBrushSide ->
   Array Int BSPTexInfo -> Ptr a ->IO (BSPBrush)
readBrush brushSideArray texInfos ptr = do
   [e1,e2,e3] <- getInts ptr 3
   let bSides = map (brushSideArray !) [e1..(e1+e2-1)]
   return $ BSPBrush {
      brushSide        = e1,
      numOfBrushSides  = e2,
      brushSides       = bSides,
      bTextureID       = e3,
      textureType      = (contents (texInfos ! e3))
   }




-- - - - - - - - - - - - - - - - - - -
-- reads the brush sides in our brushes

readBrushSides :: Handle -> [BSPLump] -> IO [BSPBrushSide]
readBrushSides handle lumps = do
   planes          <- readPlanes handle lumps
   let planeArray  =  listArray (0,((length planes)-1)) planes
   (offst,lngth) <- (getLumpData (lumps !! kBrushSides))
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf             <- mallocBytes (lngth)
   hGetBuf handle buf (lngth)
   let ptrs        =  getPtrs buf (lngth) 8
   brushsides      <- mapM (readBrushSide planeArray) ptrs
   free buf
   return brushsides

readBrushSide ::  Array Int BSPPlane -> Ptr a ->IO (BSPBrushSide)
readBrushSide planeArray ptr = do
   [e1,e2] <- getInts ptr 2
   let pln = planeArray ! (fromIntegral e1)
   return $ BSPBrushSide {
      bsPlane     = e1,
      bsPlaneNorm = (pNormal pln),
      bsPlaneDist = (distance pln),
      bsTextureID = e2
   }

-- - - - - - - - - - - - - - - - - - -
-- reads the leaf brushes that refer to the brushes

readLeafBrushes :: Handle -> [BSPLump] -> IO [BSPLeafFace]
readLeafBrushes handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kLeafBrushes))
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes lngth
   hGetBuf handle buf lngth
   leafbrushes <- getInts buf (lngth `div` 4)
   free buf
   return leafbrushes

-- - - - - - - - - - - - - - - - - - -
-- read the PVS visibility information

readVisData :: Handle -> [BSPLump] -> IO (Maybe BSPVisData)
readVisData handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kVisData))
   case lngth of
     0  -> return Nothing
     _  -> do
        hSeek handle AbsoluteSeek (fromIntegral offst)
        buf <- mallocBytes lngth
        hGetBuf handle buf lngth
        cInts <- peekArray 2 (castPtr buf :: Ptr CInt)
        let [numC, bytesPerC] = toInts cInts
        bitst <- peekArray (numC*bytesPerC) $ plusPtr (castPtr buf :: Ptr Word8) 8
        bs <-
           Arr.newListArray (0 ,(numC*bytesPerC*8-1)) (toBools bitst)
        return (Just BSPVisData {
                        numOfClusters   = numC,
                        bytesPerCluster = bytesPerC,
                        bitSets         = bs
               })




-- - - - - - - - - - - - - - - - - - -
-- reads vertex information


readVertices :: Handle -> [BSPLump] -> IO VertexData
readVertices handle lumps = do
   (offst,lngth) <- getLumpData (lumps !! kVertices)
   offs            <- getOffsets lngth offst 44
   verts           <- mapM (readVertex handle) offs
   (v,t,l,n,r)     <- seperateArrays verts
   return $ toVertexData (concat v, concat t, concat l, concat n, concat r)


readVertex :: Handle -> Int -> IO ([CFloat],[CFloat],[CFloat],[CFloat],[Word8])
readVertex handle offst = do
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes 4
   let getCFloats =
          getAndPeeks handle (castPtr buf :: Ptr CFloat) (undefined :: CFloat)
   let getWord8s  =
          getAndPeeks handle (castPtr buf :: Ptr Word8) (undefined :: Word8)
   let floats = (getCFloats 3)
   let get3Floats = fmap get3t floats
   (x,y,z)        <- get3Floats
   texCoords      <- getCFloats 2
   lightMapCoords <- getCFloats 2
   normals        <- getCFloats 3
   rgbaVal           <- getWord8s  4
   free buf
   return ([x,z,(-1)*y],texCoords,lightMapCoords,normals,rgbaVal)

dataToPointers :: VertexData -> IO VertexArrays
dataToPointers (a,b,c,d,e) = do
   a1 <- (newArray a)
   b1 <- (newArray b)
   c1 <- (newArray c)
   d1 <- (newArray d)
   e1 <- (newArray e)
   return (a1,b1,c1,d1,e1)

seperateArrays :: [([CFloat],[CFloat],[CFloat],[CFloat],[Word8])] ->
   IO ([[CFloat]],[[CFloat]],[[CFloat]],[[CFloat]],[[Word8]])
seperateArrays verts = return (unzip5 verts)

toVertexData :: ([CFloat],[CFloat],[CFloat],[CFloat],[Word8]) -> VertexData
toVertexData (a,b,c,d,e) = (toFloats a,toFloats b,toFloats c,toFloats d,e)




-- - - - - - - - - - - - - - - - - - -
-- reads lightmaps


readLightMaps :: Handle -> [BSPLump] -> IO [TextureObject]
readLightMaps handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kLightmaps))
   offs <- getOffsets lngth offst 49152
   mapM (readLightMap handle) offs

readLightMap :: Handle -> Int -> IO TextureObject
readLightMap handle offst = do
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes 49152 :: IO (Ptr Word8)
   hGetBuf handle buf 49152
   mapM (adjustRGB buf 5.0) [0..((16384)-1)]
   texObj <-createLightmapTexture buf
   return texObj

createLightmapTexture :: Ptr Word8 -> IO TextureObject
createLightmapTexture ptr = do
   [texName] <- genObjectNames 1
   rowAlignment  Unpack $= 1
   textureBinding Texture2D $= Just texName
   build2DMipmaps
     Texture2D RGB'
        (fromIntegral (128 :: Int)) (fromIntegral (128 :: Int))
            (PixelData RGB UnsignedByte ptr)
   textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
   textureFunction $= Modulate
   free ptr
   return texName


-- adjusts the brightness of the lightmap
adjustRGB :: Ptr Word8 -> Float -> Int -> IO ()
adjustRGB lightMap factor offst = do
   ptr <- return (advancePtr lightMap (3*offst))
   [r,g,b] <- (peekArray 3 ptr)
   (r2,tempr) <- scaleRGB (((realToFrac r)*factor)/255) 1
   (g2,tempg) <- scaleRGB (((realToFrac g)*factor)/255) tempr
   (b2,tempb) <- scaleRGB (((realToFrac b)*factor)/255) tempg
   byter2 <- return $ fromIntegral $ (truncate (r2 *  tempb * 255.0) :: Int)
   byteg2 <- return $ fromIntegral $ (truncate (g2 * tempb * 255.0) :: Int)
   byteb2 <- return $ fromIntegral $ (truncate (b2 * tempb * 255.0) :: Int)
   pokeArray (advancePtr lightMap (3*offst)) [byter2,byteg2,byteb2]


scaleRGB :: Float -> Float -> IO (Float,Float)
scaleRGB clr scl = do
                     if ((clr > 1.0) && ((1.0/clr) < scl))
                        then return (clr, 1.0/clr)
                      else
                         return (clr, scl)

fixLightmap ::
    Int -> Array Int TextureObject -> Maybe TextureObject
fixLightmap ind arr
    | ind < 0 = Nothing
    | otherwise = Just (arr ! ind)




-- - - - - - - - - - - - - - - - - - -
-- reads the texture information


readTexInfos :: Handle -> [BSPLump] -> IO [BSPTexInfo]
readTexInfos handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kTextures))
   offs <- getOffsets lngth offst 72
   mapM (readTexInfo handle) offs

readTexInfo :: Handle -> Int -> IO (BSPTexInfo)
readTexInfo handle offst = do
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf <- mallocBytes 64 :: IO (Ptr CChar)
   hGetBuf handle buf 64
   str <- peekCAString buf
   hSeek handle AbsoluteSeek ((fromIntegral offst) + 64)
   let getCInt =
         getAndPeek handle (castPtr buf :: Ptr CInt) (undefined :: CInt)
   flgs <- getCInt
   cons <- getCInt
   free buf
   return BSPTexInfo {
      strName  = str,
      flags  = (fromIntegral flgs),
      contents = (fromIntegral cons)
    }




-- - - - - - - - - - - - - - - - - - -
-- reads the indices to the vertex array


readIndices :: Handle -> [BSPLump] -> IO [GLint]
readIndices handle lumps = do
   (offst,lngth) <- (getLumpData (lumps !! kIndices))
   hSeek handle AbsoluteSeek (fromIntegral offst)
   buf   <- mallocBytes lngth
   hGetBuf handle buf lngth
   indces <- mapM
     (peekElemOff (castPtr buf :: Ptr CInt))
         [ 0 .. ((lngth `div` 4)-1)] :: IO [CInt]
   free buf
   return $ map fromIntegral indces




-- - - - - - - - - - - - - - - - - - -

getAndPeek :: (Storable a, Typeable a) => Handle -> Ptr a -> a -> IO a
getAndPeek handle buf be = do
   bytesRead <- hGetBuf handle buf (sizeOf be)
   when (bytesRead /= (sizeOf be)) $
      ioError $ mkIOError eofErrorType "hGetBufFully" (Just handle) Nothing
   val <- (peek buf)
   return val

getAndPeeks :: (Storable a, Typeable a) =>
   Handle -> Ptr a -> a -> Int -> IO [a]
getAndPeeks handle buf be i =
   mapM (\_ -> getAndPeek handle buf be) [1..i]

withBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile filePath = bracket (openBinaryFile filePath ReadMode) hClose

getOffsets :: Int -> Int -> Int -> IO [Int]
getOffsets lngth off sze = return $ map ((off+) . (sze*)) [0.. ((lngth `div` sze)-1)]

toInts :: (Integral a)=>[a] -> [Int]
toInts a = map fromIntegral a

toFloats :: (Real a) => [a] -> [Float]
toFloats a = map realToFrac a

get2t :: [a] -> (a, a)
get2t list = (list !! 0, list !! 1)

get3t :: [a] -> (a, a, a)
get3t list = (list !! 0, list !! 1, list !! 2)

get4t :: [a] -> (a, a, a, a)
get4t list = (list !! 0, list !! 1, list !! 2, list !! 3)

toBools :: [Word8] -> [Bool]
toBools list =
   [ y | x<-list, y <- map (testBit x) [0..7]]

getInts :: Ptr a -> Int -> IO [Int]
getInts ptr n = do
   ints <- peekArray n (castPtr ptr:: Ptr CInt)
   return $ toInts ints

getFloats :: Ptr a -> Int -> IO [Float]
getFloats ptr n = do
   floats <- peekArray n (castPtr ptr :: Ptr CFloat)
   return $ toFloats floats

cIntSize :: Int
cIntSize = (sizeOf (undefined :: CInt))

getPtrs :: Ptr a -> Int -> Int -> [Ptr a]
getPtrs ptr lngth sze = map ((plusPtr ptr) . (sze *)) [0.. ((lngth `div` sze) - 1)]


