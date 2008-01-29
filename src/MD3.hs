{-# LANGUAGE BangPatterns #-}

{- MD3.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

This module has functions to read and animate MD3 models.

credits go to Ben Humphrey who wrote the MD3 tutorial

Yet another module where i'm thinking of using
vertex buffer objects instead of vertex arrays

-}

module MD3 (
   readModel,
   readWeaponModel,
   updateAnim,
   setAnim,
   MD3Model(..),
   Model(..),
   MD3Animation(..),
   AnimState(..),
   drawModel,
   death1,
   dead1,
   death2,
   dead2,
   death3,
   dead3,
   gesture,
   attack1,
   attack2,
   dropWeap,
   raiseWeap,
   stand,
   stand2,
   walkcr,
   walk,
   run,
   back,
   swim,
   jump,
   land,
   jumpb,
   landb,
   idleLegs,
   idlecrLegs,
   turn
  )where

import Graphics.UI.GLUT
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO hiding (withBinaryFile)
import Control.Exception ( bracket )
import Textures
import Data.HashTable
import Data.Maybe
import Data.List
import Data.Array
import Quaternion
import Data.IORef
import Foreign.Storable
import Foreign.Marshal.Array



-------------------------------------------------------------------------------
-- Types

data MD3Bone =
        MD3Bone {
           minPos  :: (Float,Float,Float),
           maxPos  :: (Float,Float,Float),
           bonePos :: (Float,Float,Float),
           bscale  :: Float,
           creator :: String
        }  deriving Show

data MD3Header =
        MD3Header {
           fileID       :: String,
           version      :: Int,
           md3FileName :: String,
           numFrames    :: Int,
           numTags      :: Int,
           numMeshes    :: Int,
           numMaxSkins :: Int,
           headerSize   :: Int,
           tagStart     :: Int,
           tagEnd       :: Int,
           fileSize     :: Int
        } deriving Show

data MD3Tag =
        MD3Tag {
           tagName  :: String,
           tagPos   :: (Float,Float,Float),
           rotation :: (Float,Float,Float,Float)
        } deriving Show

data MD3MeshHeader =
        MD3MeshHeader {
           meshID         :: String,
           strName        :: String,
           numMeshFrames :: Int,
           numSkins       :: Int,
           numVertices   :: Int,
           numTriangles  :: Int,
           triStart       :: Int,
           meshHeaderSize:: Int,
           uvStart        :: Int,
           vertexStart   :: Int,
           meshSize       :: Int
        } deriving Show

data MD3Vertex =
        MD3Vertex {
           vert :: (Float,Float,Float),
           norm :: (CUChar,CUChar)
} deriving Show

data Model =
        Model {
           modelRef   :: !MD3Model,
           weapFire   :: IORef (Maybe (IO())),
           pitch            :: IORef (Maybe (IO())),
           upperState :: IORef AnimState,
           lowerState :: IORef AnimState
}

data MD3Model =
        MD3Model {
           numOfTags     :: Int,
           modelObjects :: [MeshObject],
           links                 :: [(MD3Model,IORef(AnimState))],
           auxFunc       :: IORef(Maybe (IO())),
           auxFunc2      :: IORef(Maybe (IO())),
           tags          :: Array Int [((Float,Float,Float),
                                                          (Float,Float,Float,Float))]
        } |
        MD3Weapon {
           wmodelObjects :: IORef [MeshObject]
        }

data MeshObject =
        MeshObject {
           numOfVerts    :: Int,
           numOfFaces    :: NumArrayIndices,
           numTexVertex :: Int,
           materialID    :: Maybe TextureObject,
           bHasTexture  :: Bool,
           objName       :: String,
           verticesp     :: Array Int (Ptr Float),
           normals       :: [(Float,Float,Float)],
           texCoordsl    :: [((Float,Float),(Float,Float),(Float,Float))],
           texCoords     :: Ptr Float,
           vertPtr       :: Ptr Float,
           numIndices    :: GLsizei,
           vertIndex     :: Ptr CInt,
           indexBuf      :: BufferObject,
           texBuf        :: BufferObject,
           vertBuf       :: BufferObject
}

data MD3Animation =
        MD3Animation {
           animName   :: String,
           startFrame :: Int,
           endFrame   :: Int,
           loopFrames :: Int,
           fp      :: Float
} deriving Show

data AnimState =
        AnimState {
           anims                   :: !(Array Int MD3Animation),
           currentAnim    :: !MD3Animation,
           currentFrame   :: !Int,
           nextFrame       :: !Int,
           currentTime    :: !Float,
           lastTime        :: !Float
        }

type MD3Face = (Int,Int,Int)

type MD3TexCoord = (Float,Float)


-------------------------------------------------------------------------------
-- A list of animations stored in MD3 files

animList :: [String]
animList = ["BOTH_DEATH1",      --The first twirling death animation
                  "BOTH_DEAD1", --The end of the first twirling death animation
                  "BOTH_DEATH2",        --The second twirling death animation
                  "BOTH_DEAD2", --The end of the second twirling death animation
                  "BOTH_DEATH3",        --The back flip death animation
                  "BOTH_DEAD3", --The end of the back flip death animation
                  "TORSO_GESTURE",      --The torso's gesturing animation
                  "TORSO_ATTACK",       --The torso's attack1 animation
                  "TORSO_ATTACK2",      --The torso's attack2 animation
                  "TORSO_DROP", --The torso's weapon drop animation
                  "TORSO_RAISE",        --The torso's weapon pickup animation
                  "TORSO_STAND",        --The torso's idle stand animation
                  "TORSO_STAND2",       --The torso's idle stand2 animation
                  "LEGS_WALKCR",        --The legs's crouching walk animation
                  "LEGS_WALK",  --The legs's walk animation
                  "LEGS_RUN",           --The legs's run animation
                  "LEGS_BACK",  --The legs's running backwards animation
                  "LEGS_SWIM",  --The legs's swimming animation
                  "LEGS_JUMP",  --The legs's jumping animation
                  "LEGS_LAND",  --The legs's landing animation
                  "LEGS_JUMPB", --The legs's jumping back animation
                  "LEGS_LANDB", --The legs's landing back animation
                  "LEGS_IDLE",  --The legs's idle stand animation
                  "LEGS_IDLECR",        --The legs's idle crouching animation
                  "LEGS_TURN"]  --The legs's turn animation

-- animation index

death1 :: Int
death1 = 0

dead1 :: Int
dead1 = 1

death2 :: Int
death2 = 2

dead2 :: Int
dead2 = 3

death3 :: Int
death3 = 4

dead3 :: Int
dead3 = 5

gesture :: Int
gesture = 6

attack1 :: Int
attack1 = 7

attack2 :: Int
attack2 = 8

dropWeap :: Int
dropWeap = 9

raiseWeap :: Int
raiseWeap = 10

stand :: Int
stand = 11

stand2 :: Int
stand2 = 12

walkcr :: Int
walkcr = 6

walk :: Int
walk = 7

run :: Int
run = 8

back :: Int
back = 9

swim :: Int
swim = 10

jump :: Int
jump = 11

land :: Int
land = 12

jumpb :: Int
jumpb = 13

landb :: Int
landb = 14

idleLegs :: Int
idleLegs = 15

idlecrLegs :: Int
idlecrLegs = 16

turn :: Int
turn = 17


-------------------------------------------------------------------------------
-- Functions for updating animations


-- sets the animation in the animation state
setAnim :: (Int,AnimState) -> AnimState
setAnim (animIndex,animState)
   | (animName newAnim) == (animName (currentAnim animState)) = animState
   | otherwise = AnimState {
                        anims      = anims animState,
                        currentAnim  = newAnim,
                        currentFrame = startFrame newAnim,
                        nextFrame    = nextFrame animState,
                        currentTime  = currentTime animState,
                        lastTime           = lastTime animState
         }
         where newAnim = (anims animState)!  animIndex


-- updates the animation
updateAnim :: (Int,Double,AnimState) -> (Bool,AnimState)
updateAnim (animIndex,time,animState)
   | snd(Data.Array.bounds(anims animState)) == 0 =
         let
           (haslooped,nextNF) =
                  cycleFrame cAnim 0 1 (currentFrame animStateN)
           (t,lastT,nextCF) =
                  updateTime (lastTime animStateN)
                        (currentFrame animStateN) nextNF cAnim time
         in (haslooped,AnimState {
                   anims                 = anims animStateN ,
                   currentAnim  = currentAnim animStateN,
                   currentFrame = nextCF + 0,
                   nextFrame     = nextNF + 0,
                   currentTime  = t + 0,
                   lastTime      = lastT + 0
         })
   | otherwise =
         let
          (haslooped,nextNF) =
                 cycleFrame cAnim (startFrame cAnim)
                    (endFrame cAnim) (currentFrame animStateN)
          (t,lastT,nextCF) =
                 updateTime (lastTime animStateN)
                    (currentFrame animStateN) nextNF cAnim time
         in (haslooped,AnimState {
                   anims                 = anims animStateN,
                   currentAnim  = currentAnim animStateN ,
                   currentFrame = nextCF + 0,
                   nextFrame     = nextNF + 0,
                   currentTime  = t + 0,
                   lastTime      = lastT + 0
         })
   where
         animStateN = setAnim (animIndex,animState)
         cAnim    = (currentAnim animStateN)

-- increment the frame
cycleFrame :: MD3Animation -> Int -> Int -> Int -> (Bool,Int)
cycleFrame _ startframe endframe currentframe
   | currentframe == (endframe-2) = (True,nextFrme)
   | nextFrme    == 0 = (False,startframe)
   | otherwise    = (False,nextFrme)
   where
    nextFrme = (currentframe + 1) `mod` endframe

updateTime :: Float -> Int -> Int -> MD3Animation-> Double ->(Float,Float,Int)
updateTime lasttime currentframe nextframe anim presentTime =
   let
        animSpeed    = (fp anim)
        presentTimef = 1000*(realToFrac presentTime)
        elapsedtime  = presentTimef - lasttime
        t            = elapsedtime/animSpeed
   in case ((realToFrac elapsedtime) >= animSpeed) of
            True -> (t,presentTimef ,nextframe)
            _    -> (t,lasttime,currentframe)

-------------------------------------------------------------------------------
-- renders the model
drawModel :: (MD3Model,IORef(AnimState)) -> IO( )
drawModel (model,stateRef) = do
   texture Texture2D                    $= Enabled
   --texture Texture2D                  $= Disabled
   clientState TextureCoordArray        $= Enabled
   clientState VertexArray              $= Enabled
   --clientState VertexArray            $= Disabled
   --clientState TextureCoordArray $= Disabled
   animState <- readIORef stateRef
   mapM (drawObject animState) (modelObjects model)
   let currentTag = (tags model)!(currentFrame animState)
   let nextTag    = (tags model)!(nextFrame animState)
   aux  <- readIORef (auxFunc model)
   aux2 <- readIORef (auxFunc2 model)
   case aux2 of
         Just func -> func
         Nothing -> return ()
   recurseDraw (currentTime animState) aux (links model) currentTag nextTag
   texture Texture2D $= Disabled

recurseDraw :: Float ->
   Maybe (IO())-> [(MD3Model,IORef(AnimState))] ->
         [((Float,Float,Float),(Float,Float,Float,Float))] ->
            [((Float,Float,Float),(Float,Float,Float,Float))] ->  IO()
recurseDraw _ _ [] _ _  = return ()
recurseDraw t func ((model,state):mss)
   (((c1,c2,c3),quat1):ccqs) (((n1,n2,n3),quat2):ncqs) = do
                  let (i1,i2,i3) = (c1+(t*(n1-c1)), c2+(t*(n2-c2)), c3+(t*(n3-c3)))
                  let iquat = slerp quat1 quat2 t
                  mat <- quat2Mat iquat (i1,i2,i3)
                  unsafePreservingMatrix $ do
                         multMatrix mat
                         case func of
                           Just f -> f
                           Nothing -> return ()
                         drawModel (model,state)
                  recurseDraw t func mss ccqs ncqs


-- draws a mesh object with vertex arrays
drawObject :: AnimState -> MeshObject -> IO ()
drawObject animState obj = do
   let curindex = (currentFrame animState)
   let nextIndex =        (nextFrame animState)
   case (curindex /= nextIndex) of
          True -> do
                convertToVertArray
                   (currentTime animState)
                          ((verticesp obj)!curindex)
                                 ((verticesp obj)!nextIndex)
                                         (vertPtr obj) 0 (numOfVerts obj)
                arrayPointer VertexArray $=
                   VertexArrayDescriptor 3 Float 0 (vertPtr obj)
          _       -> do
                arrayPointer VertexArray $=
                   VertexArrayDescriptor 3 Float 0 ((verticesp obj)!curindex)

   {-clientState VertexArray            $= Enabled
        lockArrays                              $= (Just (0, (numOfFaces obj)))-}

   arrayPointer TextureCoordArray       $=
         VertexArrayDescriptor 2 Float 0 (texCoords obj)

   {-clientState TextureCoordArray $= Enabled
        texture Texture2D                       $= Enabled-}

   textureBinding Texture2D             $= (materialID obj)

   {-lockArrays                         $= (Just (0, (numOfFaces obj)))
        drawElements Triangles  (numOfFaces obj) UnsignedInt (vertIndex obj)-}

   drawRangeElements Triangles (0,(numOfFaces obj))
         (numOfFaces obj) UnsignedInt (vertIndex obj)

   {-lockArrays $= Nothing
        clientState VertexArray $= Disabled
        clientState TextureCoordArray $= Disabled
        texture Texture2D $= Disabled-}


convertToVertArray ::
   Float -> Ptr Float -> Ptr Float -> Ptr Float -> Int -> Int ->IO()
convertToVertArray t cs ns arr ind limit
    | ind == limit= return()
    | otherwise = do
         c <- peekElemOff cs ind
         n <- peekElemOff ns ind
         pokeElemOff arr ind (i c n)
         convertToVertArray t cs ns arr (ind+1) limit
   where i x y = x+(t*(y-x))


-------------------------------------------------------------------------------
-- reads the MD3 files

readMD3Header :: Handle -> IO MD3Header
readMD3Header handle = do
   buf <- mallocBytes 108
   hGetBuf handle buf 108
   fID <- getString buf 4
   ver <- peek (plusPtr (castPtr buf :: Ptr CInt) 4)
   mfilename <- getString (plusPtr buf 8) 68
   [i1,i2,i3,i4,i5,i6,i7,i8] <-  getInts (plusPtr buf 76) 8
   free buf
   return $ MD3Header {
                        fileID    = fID,
                        version   = ver,
                        md3FileName = mfilename,
                        numFrames   = i1,
                        numTags   = i2,
                        numMeshes   = i3,
                        numMaxSkins = i4,
                        headerSize  = i5,
                        tagStart          = i6,
                        tagEnd    = i7,
                        fileSize          = i8
                  }




-- - - - - - - - - - - - - - - - - - -
-- reads the .skin files


readMD3Skin :: FilePath -> IO [(String,String)]
readMD3Skin filepath = withBinaryFile filepath $ \handle -> do
            contents <- hGetContents handle
            let filteredStr =  (words (replace contents))
            let files = findfiles (stripTags filteredStr)
            case (files == []) of
                    True -> return []
                    False -> return files

stripTags :: [String] -> [String]
stripTags [] = []
stripTags (s:ss)
                | (head (words(map (replace' ['_']) s))) == "tag" = stripTags ss
                | otherwise = s:(stripTags ss)




-- - - - - - - - - - - - - - - - - - -
-- reads the shader file for the weapon


readMD3Shader :: FilePath -> IO [String]
readMD3Shader filepath = withBinaryFile filepath $ \handle -> do
   contents <- hGetContents handle
   let filteredStr =  (words (replace contents))
   let files = map stripExt filteredStr
   case (files == []) of
         True -> return []
         False -> return files




-- - - - - - - - - - - - - - - - - - -
-- used by readShader and readSkin


stripExt :: String -> String
stripExt str = (head (words(map (replace' ['.']) str)))


findfiles :: [String] -> [(String,String)]
findfiles [] = []
findfiles (s:ss) = (s,(stripExt(stripPath (head ss)))):(findfiles (tail ss))

replace :: String -> String
replace str = map (replace' [',','\n','\r']) str

replace' ::  [Char] -> Char -> Char
replace' list char
          | elem char list = ' '
          | otherwise = char


stripPath :: String -> String
stripPath str = splitPath!!((length splitPath)-1)
                where splitPath = (words (map (replace' ['/']) str))




-- - - - - - - - - - - - - - - - - - -
--  reads the textures


readMD3Textures ::
   [FilePath] -> String ->
         IO (HashTable String (Maybe TextureObject))
readMD3Textures files dir = do
                texs <- mapM readMD3Skin files
                let texF = concat texs
                let unqtex = nub (map snd texF)
                textures <- mapM getAndCreateTexture (map (dir++) unqtex)
                let nmobj = concat $ map (assoc texF) (zip unqtex textures)
                fromList hashString nmobj

assoc ::
   [(String,String)] -> (String,Maybe TextureObject) ->
         [(String,Maybe TextureObject)]
assoc list (c,d) = zip (map fst (filter ((c ==).snd) list)) (cycle[d])




-- - - - - - - - - - - - - - - - - - -
-- reads the entire model



readModel :: String -> Model -> IO (Model)
readModel modelname weaponModel = do
   hash <- readMD3Textures
        (map (("tga/models/players/"++modelname)++)
           ["/head_default.skin",
            "/upper_default.skin",
            "/lower_default.skin"])
                  ("models/players/"++modelname++"/")
   get elapsedTime
   weaponAS <- noAnims
   headAS   <- noAnims
   (upperanims,loweranims) <-
          readAnimations ("tga/models/players/"++modelname++"/animation.cfg")
   let lowerS = AnimState {
                                 anims      = loweranims,
                                 currentAnim  = loweranims!8,
                                 currentFrame = (startFrame (loweranims!8)),
                                 nextFrame    = 0,
                                 currentTime  = 0,
                                 lastTime     = 0
                          }
   let upperS = AnimState {
                                 anims      = upperanims,
                                 currentAnim  = upperanims!6,
                                 currentFrame = (startFrame (upperanims!6)),
                                 nextFrame    = 0,
                                 currentTime  = 0,
                                 lastTime     = 0
                          }
   lowerstate <- newIORef lowerS
   upperstate <- newIORef upperS
   hed  <- readMD3 ("tga/models/players/"++modelname++"/head.md3") hash []
   let weapon = modelRef weaponModel
   upper <- readMD3
         ("tga/models/players/"++modelname++"/upper.md3")
            hash [("tag_weapon",(weapon,weaponAS)),("tag_head",(hed,headAS))]
   lower <- readMD3
         ("tga/models/players/"++modelname++"/lower.md3")
            hash [("tag_torso",(upper,upperstate))]
   return Model {
         modelRef   = lower,
         pitch    = auxFunc lower,
         weapFire   = auxFunc2 weapon,
         upperState = upperstate,
         lowerState = lowerstate
   }

-- just returns an empty animation
noAnims :: IO (IORef(AnimState))
noAnims = do
   let noanim =
            MD3Animation {
                  animName   = "",
                  startFrame = 0,
                  endFrame   = 0,
                  loopFrames = 0,
                  fp         = 0
            }
   let noanimState =
                AnimState {
                   anims                 = listArray (0,0) [],
                   currentAnim  = noanim,
                   currentFrame = 0,
                   nextFrame     = 0,
                   currentTime  = 0,
                   lastTime      = 0
   }
   newIORef noanimState




-- - - - - - - - - - - - - - - - - - -
-- reads a .MD3 file


readMD3 :: FilePath ->
   (HashTable String (Maybe TextureObject))->
          [(String,(MD3Model,IORef(AnimState)))] -> IO MD3Model
readMD3 filePath hashtable lns  = withBinaryFile filePath $ \handle -> do
                header <- readMD3Header handle
                readBones handle header
                tag    <- readTags handle header
                objs   <- readMeshes handle header hashtable
                let splittedTags = splitTags (numTags header) tag
                orderedlinks      <- scanTag lns tag
                let trimmedTags  = trimTags (map fst orderedlinks) splittedTags
                let trimmedArray = listArray (0,((length trimmedTags)-1)) trimmedTags
                aux     <- newIORef (Nothing)
                aux2 <- newIORef (Nothing)
                return MD3Model {
                                 numOfTags    = numTags header,
                                 modelObjects = objs,
                                 links      = (map snd orderedlinks),
                                 auxFunc            = aux,
                                 auxFunc2     = aux2,
                                 tags       = trimmedArray
                }


scanTag :: [(String,(MD3Model,IORef(AnimState)))] -> [MD3Tag] -> IO [(Int,(MD3Model,IORef(AnimState)))]
scanTag [] _ = return []
scanTag ((s,m):sms) tgs = do
   case (findIndex ((s==) . tagName)tgs) of
         Just x -> do
            rest <- (scanTag sms tgs)
            return ((x,m):rest)

splitTags :: Int -> [MD3Tag] -> [[MD3Tag]]
splitTags _ [] = []
splitTags n tgs = (take n tgs):(splitTags n $ drop n tgs)

trimTags ::
   [Int] ->
          [[MD3Tag]] ->
                [[((Float,Float,Float),(Float,Float,Float,Float))]]
trimTags _ [] = []
trimTags n (t:ts) = (map (getTagpos.(t!!)) n):(trimTags n ts)
   where getTagpos u = (tagPos u, rotation u)





-- - - - - - - - - - - - - - - - - - -
-- read the weapon models


readWeaponModel :: FilePath -> FilePath -> IO Model
readWeaponModel filePath shader = do
   weapon <- readWeapon filePath shader
   anim <- noAnims
   p <- newIORef (Nothing)
   wf <- newIORef (Nothing)
   return Model {
         modelRef   = weapon,
         pitch    = p,
         weapFire   = wf,
         upperState =anim,
         lowerState =anim
   }


readWeapon :: FilePath  -> FilePath -> IO MD3Model
readWeapon filePath shader = withBinaryFile filePath $ \handle -> do
   header    <- readMD3Header handle
   weaponTex <- (readMD3Shader shader)
   texObj    <- mapM getAndCreateTexture (map ("tga/models/weapons/"++) weaponTex)
   readBones handle header
   readTags handle header
   hash1           <- (fromList hashString [])
   objs    <- readMeshes handle header hash1
   let objs2      = map attachTex (zip texObj objs)
   let emptyList = listArray (0,0) []
   aux     <- newIORef (Nothing)
   aux2    <- newIORef (Nothing)
   return MD3Model {
                   numOfTags     = 0,
                   modelObjects = objs2,
                   links                 = [],
                   auxFunc       = aux,
                   auxFunc2      = aux2,
                   tags          = emptyList
   }

-- attaches the texture to the weapon
attachTex :: (Maybe TextureObject,MeshObject) -> MeshObject
attachTex (texObj,object) =
   MeshObject {
         numOfVerts     = numOfVerts    object,
         numOfFaces     = numOfFaces    object,
         numTexVertex   = numTexVertex object,
         materialID     = texObj,
         bHasTexture    = True,
         objName                = objName    object,
         verticesp      = verticesp  object,
         normals                = normals    object,
         texCoordsl     = texCoordsl object,
         texCoords      = texCoords  object,
         vertPtr                = vertPtr    object,
         numIndices     = numIndices object,
         vertIndex      = vertIndex  object,
         indexBuf       = indexBuf   object,
         texBuf         = texBuf           object,
         vertBuf                = vertBuf    object
   }




-- - - - - - - - - - - - - - - - - - -
-- reads the mesh information


readMeshes ::
   Handle -> MD3Header ->
         (HashTable String (Maybe TextureObject)) -> IO [MeshObject]
readMeshes handle header hashTable= do
                 posn <- hTell handle
                 meshObjects <- readMeshData handle posn (numMeshes header) hashTable
                 return meshObjects


readMeshData ::
   Handle -> Integer -> Int ->
         (HashTable String (Maybe TextureObject)) -> IO [MeshObject]
readMeshData handle posn meshesLeft hashTable
    | meshesLeft <= 0 = return []
    | otherwise = do
                header <- readMD3MeshHeader handle
                readSkins handle header
                faces <- readFaces handle posn header
                texcoords <- readTexCoords handle posn header
                vertices <- readVertices handle posn header
                hSeek handle AbsoluteSeek (posn+(fromIntegral (meshSize header)))
                object <- convertMesh header faces texcoords vertices hashTable
                objects <-
                   readMeshData handle
                        (posn+(fromIntegral (meshSize header))) (meshesLeft-1) hashTable
                return (object:objects)




-- - - - - - - - - - - - - - - - - - -
-- converts the vertex, texture, face information into
-- a meshobject


convertMesh :: MD3MeshHeader ->
   [MD3Face] -> [MD3TexCoord] -> [MD3Vertex] ->
          (HashTable String (Maybe TextureObject)) -> IO MeshObject
convertMesh header faceIndex texcoords vertices hashTable = do
    let verts           = map vert vertices
    let scaledVerts = map devideBy64 verts
    let keyframes       = devideIntoKeyframes (numVertices  header) scaledVerts

    imPTR <- mapM (Foreign.Marshal.Array.newArray) (map convertVert keyframes)
    let facesArrayp = listArray (0,((length imPTR)-1)) imPTR

    uvs     <- convertTex faceIndex texcoords
    uvptr   <- Foreign.Marshal.Array.newArray (convertTex2 texcoords)
    indces  <- Foreign.Marshal.Array.newArray (convertInd faceIndex)
    vPtr          <- mallocBytes ((length (head keyframes))*12)

    [a] <- genObjectNames 1
    {-bindBuffer ArrayBuffer $= Just a
         bufferData ArrayBuffer $=
          (fromIntegral (3*((length (head keyframes))*3)*4),
                 facesArrayp!0 , StaticDraw)
         arrayPointer VertexArray $=
           VertexArrayDescriptor 3 Float 0 nullPtr-}

    [b] <- genObjectNames 1
    {-bindBuffer ArrayBuffer $= Just b
         bufferData ArrayBuffer $=
            (fromIntegral (4*(length (convertTex2 texcoords))),
                    uvptr, StaticDraw)
         arrayPointer TextureCoordArray $=
            VertexArrayDescriptor 2 Float 0 nullPtr-}

    [c] <- genObjectNames 1
    {-bindBuffer ElementArrayBuffer $= Just c
         bufferData ElementArrayBuffer $=
            (fromIntegral ((fromIntegral (length (head faces)))*12),
                   indices, StaticDraw)
         arrayPointer TextureCoordArray $=
            VertexArrayDescriptor 2 Float 0 nullPtr-}

    tex <- (Data.HashTable.lookup hashTable (strName header))
    return MeshObject {
            numOfVerts    = (length (head keyframes))*3,
            numOfFaces    = 3*(fromIntegral (numTriangles header)),
            numTexVertex  = numVertices  header,
            materialID    = fromJust tex,
            bHasTexture   = False,
            objName        = strName header,
            verticesp      = facesArrayp,
            normals        = [],
            texCoords      = uvptr,
            texCoordsl    = uvs,
            vertPtr        = vPtr,
            numIndices    = fromIntegral  ((numTriangles header)*3),
            vertIndex      = indces,
            indexBuf = c,
            texBuf = b,
            vertBuf = a
        }


convertInd :: [(Int,Int,Int)] -> [CInt]
convertInd [] = []
convertInd ((i1,i2,i3):is) =
   [fromIntegral i1,fromIntegral  i2,fromIntegral  i3]++(convertInd is)


convertTex2 :: [(Float,Float)] -> [Float]
convertTex2 [] = []
convertTex2 ((u,v):uvs) = [u,v]++(convertTex2 uvs)


convertVert :: [(Float,Float,Float)] -> [Float]
convertVert [] = []
convertVert ((x,y,z):xyzs) = [x,y,z]++(convertVert xyzs)


convertTex ::
   [(Int,Int,Int)] ->
          [(Float,Float)] ->
                IO [((Float,Float),(Float,Float),(Float,Float))]
convertTex indces uvs = do
   let uvarray = listArray (0,((length uvs)-1)) uvs
   let uv = map (getUVs uvarray) indces
   return uv


getUVs ::
   Array Int (Float,Float) ->
         (Int,Int,Int) ->
            ((Float,Float),(Float,Float),(Float,Float))
getUVs uvs (i1,i2,i3) = (uvs ! i1, uvs ! i2 , uvs ! i3)


devideIntoKeyframes :: Int ->
   [(Float,Float,Float)] -> [[(Float,Float,Float)]]
devideIntoKeyframes _ [] = []
devideIntoKeyframes n verts =
   (take n verts):(devideIntoKeyframes n (drop n verts))


devideBy64 :: (Float,Float,Float) -> (Float,Float,Float)
devideBy64 (x,y,z) = (x / 64,y /64,z / 64)

-- - - - - - - - - - - - - - - - - - -
-- reads the vertices

readVertices ::
   Handle -> Integer -> MD3MeshHeader -> IO [MD3Vertex]
readVertices handle posn header = do
   hSeek handle AbsoluteSeek (posn+(fromIntegral (vertexStart header)))
   buf <- mallocBytes ((numMeshFrames header)*(numVertices header)*8)
   hGetBuf handle buf ((numMeshFrames header)*(numVertices header)*8)
   let ptrs = getPtrs buf ((numMeshFrames header)*(numVertices header)) 8
   triangles <- mapM readVertex ptrs
   free buf
   return triangles

readVertex :: Ptr a -> IO MD3Vertex
readVertex ptr = do
   [f1,f2,f3] <- peekArray 3 (castPtr ptr :: Ptr CShort)
   [n1,n2] <- peekArray 2 (plusPtr (castPtr ptr :: Ptr CUChar) 6)
   return MD3Vertex {
                   vert =(realToFrac f1,realToFrac f2,realToFrac        f3),
                   norm = (n1,n2)
   }




-- - - - - - - - - - - - - - - - - - -
-- reads the texture coordinates


readTexCoords :: Handle -> Integer -> MD3MeshHeader -> IO [MD3TexCoord]
readTexCoords handle posn header = do
   hSeek handle AbsoluteSeek (posn+(fromIntegral (uvStart header)))
   buf <- mallocBytes ((numVertices header)*8)
   hGetBuf handle buf ((numVertices header)*8)
   let ptrs = getPtrs buf (numVertices header) 8
   texcoords <- mapM readTexCoord ptrs
   free buf
   return texcoords

readTexCoord :: Ptr a -> IO MD3TexCoord
readTexCoord ptr = do [f1,f2] <- getFloats ptr 2
                      return (f1,f2)




-- - - - - - - - - - - - - - - - - - -
-- reads the models faces


readFaces :: Handle -> Integer -> MD3MeshHeader -> IO [MD3Face]
readFaces handle posn header = do
   hSeek handle AbsoluteSeek (posn+(fromIntegral (triStart header)))
   buf <- mallocBytes ((numTriangles header)*12)
   hGetBuf handle buf ((numTriangles header)*12)
   let ptrs = getPtrs buf (numTriangles header) 12
   faces <- mapM readFace ptrs
   free buf
   return faces


readFace :: Ptr a -> IO MD3Face
readFace ptr = do
   [f1,f2,f3] <- getInts ptr 3
   return (f1,f2,f3)




-- - - - - - - - - - - - - - - - - - -
-- reads the MD3 skins


readSkins ::Handle -> MD3MeshHeader -> IO [String]
readSkins handle header = do
   buf <- mallocBytes ((numSkins header)*68)
   hGetBuf handle buf ((numSkins header)*68)
   let skinPtrs = getPtrs buf (numSkins header) 68
   skins <- mapM readSkin skinPtrs
   free buf
   return skins


readSkin :: Ptr a -> IO String
readSkin buf = do
   skin <- getString buf 68
   return skin




-- - - - - - - - - - - - - - - - - - -
-- reads a meshheader


readMD3MeshHeader :: Handle -> IO MD3MeshHeader
readMD3MeshHeader handle = do
   buf <- mallocBytes 108
   hGetBuf handle buf 108
   mID <- getString buf 4
   meshName <- getString (plusPtr buf 4) 68
   [i1,i2,i3,i4,i5,i6,i7,i8,i9] <- getInts (plusPtr buf 72) 9
   free buf
   return $
         MD3MeshHeader {
            meshID         = mID,
            strName        = meshName,
            numMeshFrames = i1,
            numSkins       = i2,
            numVertices   = i3,
            numTriangles  = i4,
            triStart       = i5,
            meshHeaderSize= i6,
            uvStart        = i7,
            vertexStart   = i8,
            meshSize       = i9
        }




-- - - - - - - - - - - - - - - - - - -
-- reads the tags


readTags :: Handle -> MD3Header -> IO [MD3Tag]
readTags handle header = do
   buf <- mallocBytes (112*(numFrames header)*(numTags header))
   hGetBuf handle buf (112*(numFrames header)*(numTags header))
   let ptrs = getPtrs buf ((numFrames header)*(numTags header)) 112
   tgs <- mapM readTag ptrs
   free buf
   return tgs

readTag :: Ptr a -> IO MD3Tag
readTag buf = do
   tName <- getString buf 64
   [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12] <- getFloats (plusPtr buf 64) 12
   let quat = mat2Quat ((f4,f5,f6),(f7,f8,f9),(f10,f11,f12))
   return $ MD3Tag {
                    tagName = tName,
                    tagPos =(f1,f2,f3),
                    rotation = quat
   }




-- - - - - - - - - - - - - - - - - - -
-- reads the bones which we don't use


readBones :: Handle -> MD3Header -> IO [MD3Bone]
readBones handle header = do
   buf <- mallocBytes (56*(numFrames header))
   hGetBuf handle buf (56*(numFrames header))
   let ptrs = getPtrs buf (numFrames header) 56
   bones <- mapM readBone ptrs
   free buf
   return bones


readBone :: Ptr a-> IO MD3Bone
readBone buf = do
   [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10] <- getFloats buf 10
   string <- getString (plusPtr buf 40) 16
   return MD3Bone {
                           minPos = (f1,f2,f3),
                           maxPos = (f4,f5,f6),
                           bonePos = (f7,f8,f9),
                           bscale = f10,
                           creator = string
                }




-- - - - - - - - - - - - - - - - - - -
-- reads animations from the animation.cfg file


readAnimations ::
   FilePath -> IO (Array Int MD3Animation,Array Int MD3Animation)
readAnimations filepath = withBinaryFile filepath $ \handle -> do
           lnes <- readLines handle
           animsl <- mapM readAnimation lnes
           let anms = concat animsl
           let upperAnims = filter (matchPrefix "TORSO") anms
           let lowerAnims = filter (matchPrefix "LEGS") anms
           let bothAnims  = filter (matchPrefix "BOTH") anms
           let fixedLower =
                   map (fixLower $ (startFrame $ head lowerAnims)-
                                            (startFrame $ head upperAnims)) lowerAnims
           return (listArray
                            (0,((length (bothAnims++upperAnims))-1))
                            (bothAnims++upperAnims),
                         listArray (0,((length (bothAnims++fixedLower))-1))
                            (bothAnims++fixedLower))


readAnimation :: String -> IO [MD3Animation]
readAnimation line
    | length subStrings <= 0 = do
                return []
    | length subStrings >= 5 =
          case (elem (subStrings !! 4) animList) of
                True -> do
                   let startF = (read $ subStrings!!0):: Int
                   let numF   = (read $ subStrings!!1):: Int
                   let loopF  = (read $ subStrings!!2):: Int
                   let f      = (read $ subStrings!!3):: Int
                   let aName  = subStrings!!4
                   return [MD3Animation {
                                    animName    = aName,
                                    startFrame = startF,
                                    endFrame    = startF + numF,
                                    loopFrames = loopF,
                                    fp         = 1000 * (1 / realToFrac f)
                   }]
                _ -> return []
    | otherwise = do
                return []
    where
          replc str  = map (replace' ['/','\n','\r']) str
          subStrings = (words (replc line))


fixLower :: Int -> MD3Animation -> MD3Animation
fixLower offset anim = MD3Animation {
                                           animName   = animName anim,
                                           startFrame = (startFrame anim) - offset,
                                           endFrame   = (endFrame anim) - offset,
                                           loopFrames = loopFrames anim,
                                           fp         = fp anim
                                   }


matchPrefix :: String -> MD3Animation -> Bool
matchPrefix prefix anim =
   prefix == head (words (map (replace' ['_']) (animName anim)))


readLines :: Handle -> IO [String]
readLines handle = do
            eof <- hIsEOF handle
            case (eof) of
                 False -> do
                                 lne <- hGetLine handle
                                 lnes <- readLines handle
                                 return (lne:lnes)
                 _        -> return []

-------------------------------------------------------------------------------


withBinaryFile :: FilePath -> (Handle -> IO a) -> IO a
withBinaryFile filePath = bracket (openBinaryFile filePath ReadMode) hClose

toInts :: (Integral a)=>[a] -> [Int]
toInts a = map fromIntegral a

toFloats :: (Real a) => [a] -> [Float]
toFloats a = map realToFrac a

getInts :: Ptr a -> Int -> IO [Int]
getInts ptr n = do ints <- peekArray n (castPtr ptr:: Ptr CInt)
                   return $ toInts ints

getFloats :: Ptr a -> Int -> IO [Float]
getFloats ptr n = do floats <- peekArray n (castPtr ptr :: Ptr CFloat)
                     return $ toFloats floats

getString :: Ptr a -> Int -> IO String
getString ptr _ = do string <- peekCString (castPtr ptr :: Ptr CChar)
                     return string

getPtrs :: Ptr a -> Int -> Int -> [Ptr a]
getPtrs ptr lngth size= map ((plusPtr ptr).(size*)) [0.. (lngth-1)]


