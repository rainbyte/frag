{-# LANGUAGE BangPatterns #-}

-- Object.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

module Object (
    Object,
    ObjInput(..),
    ObjOutput(..),
    ObsObjState(..),
    Message(..),
    isRay,          -- :: ObsObjState -> Bool
    isAICube,       -- :: ObsObjState -> Bool
    isProjectile,  -- :: ObsObjState -> Bool
    isCamera,       -- :: ObsObjState -> Bool
) where

import AFRP (SF, Event)
import AFRPForceable (Forceable(), force)
import Camera
import IdentityList
import MD3 (AnimState)
import Parser (GameInput)


type Object = SF ObjInput ObjOutput

data ObjInput = ObjInput {
          oiHit           :: !(Event [(ILKey,ObsObjState)]),
          oiMessage       :: !(Event [(ILKey,Message)]),
          oiCollision     :: !Camera,
          oiCollisionPos :: !(Double,Double,Double),
          oiOnLand        :: !Bool,
          oiGameInput     :: !GameInput,
          oiVisibleObjs  :: !(Event [(ILKey,ObsObjState)])
        }

data ObjOutput = ObjOutput {
    ooObsObjState :: !ObsObjState,
    ooSendMessage :: !(Event [(ILKey,(ILKey,Message))]),
    ooKillReq      :: (Event ()),
    ooSpawnReq    :: (Event [ILKey->Object])
}

data Message = Coord !(Double,Double,Double) |
                        PlayerLockedOn |
                        PlayerLockedOn2 |
                        TargetPosition !(Double,Double,Double) |
                        TargetPosition2 !(Double,Double,Double) |
                        EnemyDown

-- most fields are strict to prevent space leaks

data ObsObjState =
    OOSCamera {
          newCam          ::  !Camera,
          oldCam          ::  !Camera,
          health          ::  !Double,
          ammo    ::  !Double,
          score   ::  !Int,
          cood    ::  ![(Double,Double,Double)]
         }
    | OOSRay {
          rayStart ::    !(Double,Double,Double),
          rayEnd         ::      !(Double,Double,Double),
          rayUC  ::      !(Double,Double,Double),
          clipped  ::    !Bool,
          firedFrom :: !ILKey
         }
    | OOSProjectile {
          projectileOldPos :: !(Double,Double,Double),
          projectileNewPos :: !(Double,Double,Double),
          firedFrom :: !ILKey
         }
    | OOSWayPoint !(Double,Double,Double)
    | OOSAICube {
          oosOldCubePos  :: !(Double,Double,Double),
          oosNewCubePos  :: !(Double,Double,Double),
          oosCubeSize     :: !(Double,Double,Double),
          oosCubeAngle :: !Double,
          oosCubePitch :: !Double,
          upperAnim     :: !AnimState,
          lowerAnim     :: !AnimState,
          health                :: !Double,
          modelName     :: !String,
          target                :: !(Double,Double,Double),
          fade          :: !Float
         }

instance Forceable ObsObjState where
    -- If non-strict fields: oosNonStrict1 obj `seq` ... `seq` obj
    force obj = obj

isRay :: ObsObjState -> Bool
isRay (OOSRay {}) = True
isRay _            = False

isCamera :: ObsObjState -> Bool
isCamera (OOSCamera {}) = True
isCamera _                      = False

isAICube :: ObsObjState -> Bool
isAICube (OOSAICube {}) = True
isAICube _                 = False

isProjectile :: ObsObjState -> Bool
isProjectile (OOSProjectile {}) = True
isProjectile _            = False

