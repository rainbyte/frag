{-# LANGUAGE BangPatterns #-}

-- This is the idenity list module that came with the space invaders source
-- i just added insertILA_, listToILA

{- $Id: IdentityList.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                                I N V A D E R S                                                  *
*                                                                                                                         *
*          Module:              IdentityList                                            *
*          Purpose:     Association list with automatic key assignment and   *
*                       identity-preserving map and filter operations.          *
*          Author:              Henrik Nilsson                                  *
*                                                                                                                         *
*                   Copyright (c) Yale University, 2003                                           *
*                                                                                                                         *
******************************************************************************
-}

module IdentityList (
    ILKey,        -- Identity-list key type
    IL,           -- Identity-list, abstract. Instance of functor.
    emptyIL,      -- :: IL a
    insertIL_,   -- :: a -> IL a -> IL a
    insertIL,     -- :: a -> IL a -> (ILKey, IL a)
    listToIL,     -- :: [a] -> IL a
    keysIL,       -- :: IL a -> [ILKey]
    elemsIL,      -- :: IL a -> [a]
    assocsIL,     -- :: IL a -> [(ILKey, a)]
    deleteIL,     -- :: ILKey -> IL a -> IL a
    mapIL,        -- :: ((ILKey, a) -> b) -> IL a -> IL b
    filterIL,     -- :: ((ILKey, a) -> Bool) -> IL a -> IL a
    mapFilterIL,  -- :: ((ILKey, a) -> Maybe b) -> IL a -> IL b
    lookupIL,     -- :: ILKey -> IL a -> Maybe a
    findIL,       -- :: ((ILKey, a) -> Bool) -> IL a -> Maybe a
    mapFindIL,   -- :: ((ILKey, a) -> Maybe b) -> IL a -> Maybe b
    findAllIL,   -- :: ((ILKey, a) -> Bool) -> IL a -> [a]
    mapFindAllIL,  -- :: ((ILKey, a) -> Maybe b) -> IL a -> [b]
    insertILA_,
    listToILA

) where

------------------------------------------------------------------------------
-- Data type definitions
------------------------------------------------------------------------------

type ILKey = Int

-- Invariants:
-- * Sorted in descending key order. (We don't worry about
--      key wrap around).
-- * Keys are NOT reused
data IL a = IL { ilNextKey :: !ILKey, ilAssocs :: ![(ILKey, a)] }


------------------------------------------------------------------------------
-- Class instances
------------------------------------------------------------------------------

instance Functor IL where
    fmap f (IL {ilNextKey = nk, ilAssocs = kas}) =
           IL {ilNextKey = nk, ilAssocs = [ (i, f a) | (i, a) <- kas ]}


------------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------------

emptyIL :: IL a
emptyIL = IL {ilNextKey = 0, ilAssocs = []}


insertIL_ :: a -> IL a -> IL a
insertIL_ a il = snd (insertIL a il)


insertIL :: a -> IL a -> (ILKey, IL a)
insertIL a (IL {ilNextKey = k, ilAssocs = kas}) = (k, il') where
    il' = IL {ilNextKey = k + 1, ilAssocs = (k, a) : kas}

-- inserts an object into an identity list and gives the object its key
insertILA_ :: (ILKey -> a) -> IL a -> IL a
insertILA_ f (IL {ilNextKey = k, ilAssocs = kas}) = il' where
    il' = IL {ilNextKey = k + 1, ilAssocs = (k, f k) : kas}

listToIL :: [a] -> IL a
listToIL as = IL {ilNextKey = length as,
                  ilAssocs = reverse (zip [0..] as)} -- Maintain invariant!

-- converts a list to an identity list and gives every object in the list its ILkey
listToILA :: [(ILKey -> a)] -> IL a
listToILA as = IL {ilNextKey = length as,
                  ilAssocs = reverse (zip [0..] (appFunc as [0..] ))} -- Maintain invariant!

appFunc :: [(ILKey -> a)] -> [ILKey] -> [a]
appFunc [] _ = []
appFunc (f:fs) (k:ks) = (f k):(appFunc fs ks)
appFunc _ _ = []

------------------------------------------------------------------------------
-- Additional selectors
------------------------------------------------------------------------------

assocsIL :: IL a -> [(ILKey, a)]
assocsIL = ilAssocs


keysIL :: IL a -> [ILKey]
keysIL = map fst . ilAssocs


elemsIL :: IL a -> [a]
elemsIL = map snd . ilAssocs


------------------------------------------------------------------------------
-- Mutators
------------------------------------------------------------------------------

deleteIL :: ILKey -> IL a -> IL a
deleteIL k (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = deleteHlp kas}
    where
        deleteHlp []                                                      = []
        deleteHlp kakas@(ka@(k', _) : ks) | k > k'        = kakas
          | k == k'   = ks
          | otherwise = ka : deleteHlp ks


------------------------------------------------------------------------------
-- Filter and map operations
------------------------------------------------------------------------------

-- These are "identity-preserving", i.e. the key associated with an element
-- in the result is the same as the key of the element from which the
-- result element was derived.

mapIL :: ((ILKey, a) -> b) -> IL a -> IL b
mapIL f (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = [(k, f ka) | ka@(k,_) <- kas]}


filterIL :: ((ILKey, a) -> Bool) -> IL a -> IL a
filterIL p (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = filter p kas}


mapFilterIL :: ((ILKey, a) -> Maybe b) -> IL a -> IL b
mapFilterIL p (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {
           ilNextKey = nk,
           ilAssocs = [(k, b) | ka@(k, _) <- kas, Just b <- [p ka]]
    }


------------------------------------------------------------------------------
-- Lookup operations
------------------------------------------------------------------------------

lookupIL :: ILKey -> IL a -> Maybe a
lookupIL k il = lookup k (ilAssocs il)


findIL :: ((ILKey, a) -> Bool) -> IL a -> Maybe a
findIL p (IL {ilAssocs = kas}) = findHlp kas
    where
        findHlp []                       = Nothing
        findHlp (ka@(_, a) : ks) = if p ka then Just a else findHlp ks


mapFindIL :: ((ILKey, a) -> Maybe b) -> IL a -> Maybe b
mapFindIL p (IL {ilAssocs = kas}) = mapFindHlp kas
    where
        mapFindHlp []             = Nothing
        mapFindHlp (ka : ks) = case p ka of
                                 Nothing      -> mapFindHlp ks
                                 jb@(Just _) -> jb


findAllIL :: ((ILKey, a) -> Bool) -> IL a -> [a]
findAllIL p (IL {ilAssocs = kas}) = [ a | ka@(_, a) <- kas, p ka ]


mapFindAllIL:: ((ILKey, a) -> Maybe b) -> IL a -> [b]
mapFindAllIL p (IL {ilAssocs = kas}) = [ b | ka <- kas, Just b <- [p ka] ]
