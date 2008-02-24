{-# LANGUAGE TypeOperators, Rank2Types #-}

{- $Id: AFRP.hs,v 1.37 2004/03/22 19:46:40 antony Exp $
******************************************************************************
*                                                       A F R P                                                   *
*                                                                                                                         *
*          Module:          AFRP                                                                                  *
*          Purpose:         The AFRP core.                                              *
*       Authors:        Henrik Nilsson and Antony Courtney              *
*                                                                                                                         *
*                   Copyright (c) Yale University, 2003                                           *
*                                                                                                                         *
******************************************************************************
-}

-- ToDo:
-- * Check embedSynch for space leaks. It might be a good idea to force
--      "dropped frames".
-- * The internal "streamToSignal" is interesting, and a version somehow
--      accepting a time stamped stream/assuming equidistant samples, possibly
--      with an interpolation function, might be even more interesting. Perhaps
--      consider a version that applies "cycle" to the supplied list? Note that
--      there is a relation to "embedSynch" since a partial application of
--      "embedSynch" to "identity" would yield something similar. Or it is
--      in some sense the inverse of "embed".
-- * It seems the use of VectorSpace-based integrals causes more ambiguity
--      problems than before. Investigate (comments in AFRPTest.hs).
-- * Maybe "now", "after", "repeatedly" should return ().
--      There could be a bunch of utilities "nowTag", "afterTag", "repeatedlyTag",
--      and "edgeTag". Decide based on API consistency. E.g. edge already
--      returns ().
-- * Reconsider the semantics of "edgeBy". Does not disallow an edge
--      condition that persists between consecutive samples. OTOH, consider
--      a signal that alternates between two discrete values (True, False, say).
--      Surely we could then see edges on every sample. It's not really for us
--      to say whether the edge detecting function does a good job or not?
-- * We should probably introduce a type synonym Frequency here.
--      It might be most natural to give some parameters in terms of frequency
--      (like for "repeatedly" and "occasionally"). On the other hand, there
--      is "after", and it would be good if "after" and "repeatedly" are
--      mutually consistent, if "repeatedly" and "occasionally" are consistent,
--      and if the user knows that "Time" is the only dimension he or she needs
--      to worry about.
-- * Here's an argument for why "now", "after", etc. should return "()".
--      The event value has to be a static entity anyway in these cases. So,
--      if we need them to something DYNAMIC, then the extra argument is useless.
--      Or if we don't care. If it is decided to change the interface in that
--      way, I guess we could also change Time to Frequency where that makes
--      sense. On the other hand, what's the point of "now" always returning
--      "()"? Would one not usually want to say what to return? If yes, then
--      There is something to be said for making "after" consistent with "now".
--      After all, we should have "now = after 0".
-- * Maybe "reactimate" should be parametrized on the monad type?
-- * Revisit the "reactimate" interfaces along with embedding.
-- * Revisit integration and differentiation. Valery suggests:
--
--        integral :: VectorSpace a s => SF a a
--        integral = (\ a _ dt v -> v ^+^ realToFrac dt *^ a) `iterFrom`
--                         zeroVector
--
--        -- non-delayed integration (using the function's value at the current
--        -- time)
--        ndIntegral :: VectorSpace a s => SF a a
--        ndIntegral = (\ _ a' dt v -> v ^+^ realToFrac dt *^ a') `iterFrom`
--                              zeroVector
--
--        derivative :: VectorSpace a s => SF a a
--        derivative = (\ a a' dt _ -> (a' ^-^ a) ^/ realToFrac dt) `iterFrom`
--                              zeroVector
--
--        iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
--        f `iterFrom` b = SF (iterAux b) where
--               iterAux b a = (SFTIVar (\ dt a' -> iterAux (f a a' dt b) a'), b)
--      See also the original e-mail discussion.

module AFRP (
-- Re-exported module, classes, and types
    module Control.Arrow,
    module AFRPVectorSpace,
    RandomGen(..),
    Random(..),

-- Reverse function composition and arrow plumbing aids
    ( # ),              -- :: (a -> b) -> (b -> c) -> (a -> c), infixl 9
    dup,                -- :: a -> (a,a)
    swap,       -- :: (a,b) -> (b,a)

-- main types
    Time, -- [s] Both for time w.r.t. some reference and intervals.
    SF,         -- Signal Function.
    Event(..), -- Events; conceptually similar to Maybe (but abstract).

-- main instances
    -- SF is an instance of Arrow and ArrowLoop. Method instances:
    -- arr      :: (a -> b) -> SF a b
    -- (>>>)    :: SF a b -> SF b c -> SF a c
    -- (<<<)    :: SF b c -> SF a b -> SF a c
    -- first    :: SF a b -> SF (a,c) (b,c)
    -- second   :: SF a b -> SF (c,a) (c,b)
    -- (***)    :: SF a b -> SF a' b' -> SF (a,a') (b,b')
    -- (&&&)    :: SF a b -> SF a b' -> SF a (b,b')
    -- returnA :: SF a a
    -- loop     :: SF (a,c) (b,c) -> SF a b

    -- Event is an instance of Functor, Eq, and Ord. Some method instances:
    -- fmap     :: (a -> b) -> Event a -> Event b
    -- (==)      :: Event a -> Event a -> Bool
    -- (<=)     :: Event a -> Event a -> Bool

-- Basic signal functions
    identity,           -- :: SF a a
    constant,           -- :: b -> SF a b
    localTime,  -- :: SF a Time
    time,                   -- :: SF a Time,    Other name for localTime.

-- Initialization
    (-->),              -- :: b -> SF a b -> SF a b,            infixr 0
    (>--),              -- :: a -> SF a b -> SF a b,            infixr 0
    (-=>),                  -- :: (b -> b) -> SF a b -> SF a b      infixr 0
    (>=-),                  -- :: (a -> a) -> SF a b -> SF a b      infixr 0
    initially,  -- :: a -> SF a a

-- Basic event sources
    never,              -- :: SF a (Event b)
    now,                -- :: b -> SF a (Event b)
    after,              -- :: Time -> b -> SF a (Event b)
    repeatedly,         -- :: Time -> b -> SF a (Event b)
    afterEach,  -- :: [(Time,b)] -> SF a (Event b)
    edge,       -- :: SF Bool (Event ())
    iEdge,              -- :: Bool -> SF Bool (Event ())
    edgeTag,            -- :: a -> SF Bool (Event a)
    edgeJust,           -- :: SF (Maybe a) (Event a)
    edgeBy,             -- :: (a -> a -> Maybe b) -> a -> SF a (Event b)

-- Stateful event suppression
    notYet,             -- :: SF (Event a) (Event a)
    once,       -- :: SF (Event a) (Event a)
    takeEvents,         -- :: Int -> SF (Event a) (Event a)
    dropEvents,         -- :: Int -> SF (Event a) (Event a)

-- Basic switchers
    switch,  dSwitch,   -- :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
    rSwitch, drSwitch,  -- :: SF a b -> SF (a,Event (SF a b)) b
    kSwitch, dkSwitch,  -- :: SF a b
                        --       -> SF (a,b) (Event c)
                        --       -> (SF a b -> c -> SF a b)
                        --       -> SF a b

-- Parallel composition and switching over collections with broadcasting
    parB,       -- :: Functor col => col (SF a b) -> SF a (col b)
    pSwitchB,dpSwitchB, -- :: Functor col =>
                        --              col (SF a b)
                        --        -> SF (a, col b) (Event c)
                        --        -> (col (SF a b) -> c -> SF a (col b))
                        --        -> SF a (col b)
    rpSwitchB,drpSwitchB,-- :: Functor col =>
                        --              col (SF a b)
                        --        -> SF (a, Event (col (SF a b)->col (SF a b)))
                        --                 (col b)

-- Parallel composition and switching over collections with general routing
    par,                -- Functor col =>
                        --        (forall sf . (a -> col sf -> col (b, sf)))
                        --        -> col (SF b c)
                        --        -> SF a (col c)
    pSwitch, dpSwitch,  -- pSwitch :: Functor col =>
                        --        (forall sf . (a -> col sf -> col (b, sf)))
                        --        -> col (SF b c)
                        --        -> SF (a, col c) (Event d)
                        --        -> (col (SF b c) -> d -> SF a (col c))
                        --        -> SF a (col c)
    rpSwitch,drpSwitch, -- Functor col =>
                        --       (forall sf . (a -> col sf -> col (b, sf)))
                        --       -> col (SF b c)
                        --       -> SF (a, Event (col (SF b c) -> col (SF b c)))
                        --          (col c)

-- Wave-form generation
    hold,       -- :: a -> SF (Event a) a
    trackAndHold,       -- :: a -> SF (Maybe a) a

-- Accumulators
    accum,              -- :: a -> SF (Event (a -> a)) (Event a)
    accumBy,            -- :: (b -> a -> b) -> b -> SF (Event a) (Event b)
    accumFilter,        -- :: (c -> a -> (c, Maybe b)) -> c
                        --       -> SF (Event a) (Event b)

-- Delays
    pre,                -- :: SF a a
    iPre,       -- :: a -> SF a a

-- Integration and differentiation
    integral,           -- :: VectorSpace a s => SF a a
    derivative,         -- :: VectorSpace a s => SF a a         -- Crude!
    imIntegral,         -- :: VectorSpace a s => a -> SF a a

-- Loops with guaranteed well-defined feedback
    loopPre,            -- :: c -> SF (a,c) (b,c) -> SF a b
    loopIntegral,       -- :: VectorSpace c s => SF (a,c) (b,c) -> SF a b

-- Pointwise functions on events
    noEvent,            -- :: Event a
    noEventFst,         -- :: (Event a, b) -> (Event c, b)
    noEventSnd,         -- :: (a, Event b) -> (a, Event c)
    event,              -- :: a -> (b -> a) -> Event b -> a
    fromEvent,  -- :: Event a -> a
    isEvent,            -- :: Event a -> Bool
    isNoEvent,  -- :: Event a -> Bool
    tag,                -- :: Event a -> b -> Event b,          infixl 8
    attach,             -- :: Event a -> b -> Event (a, b),     infixl 8
    lMerge,             -- :: Event a -> Event a -> Event a,    infixl 6
    rMerge,             -- :: Event a -> Event a -> Event a,    infixl 6
    merge,              -- :: Event a -> Event a -> Event a,    infixl 6
    mergeBy,            -- :: (a -> a -> a) -> Event a -> Event a -> Event a
    mapMerge,               -- :: (a -> c) -> (b -> c) -> (a -> b -> c)
                                    --  -> Event a -> Event b -> Event c
    mergeEvents,            -- :: [Event a] -> Event a
    catEvents,  -- :: [Event a] -> Event [a]
    joinE,              -- :: Event a -> Event b -> Event (a,b),infixl 7
    splitE,             -- :: Event (a,b) -> (Event a, Event b)
    filterE,            -- :: (a -> Bool) -> Event a -> Event a
    mapFilterE,         -- :: (a -> Maybe b) -> Event a -> Event b
    gate,       -- :: Event a -> Bool -> Event a,       infixl 8

-- Noise (random signal) sources and stochastic event sources
    noise,              -- :: noise :: (RandomGen g, Random b) =>
                        --              g -> SF a b
    noiseR,             -- :: noise :: (RandomGen g, Random b) =>
                        --              (b,b) -> g -> SF a b
    occasionally,       -- :: RandomGen g => g -> Time -> b -> SF a (Event b)

-- Reactimation
    reactimate,         -- :: IO a
                                --       -> (Bool -> IO (DTime, Maybe a))
                                --       -> (Bool -> b -> IO Bool)
                                --       -> SF a b
                                --       -> IO ()
    ReactHandle,
    reactInit,      --  IO a -- init
                                    --  -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
                                    --  -> SF a b
                                    --  -> IO (ReactHandle a b)
-- process a single input sample:
    react,                  --  ReactHandle a b
                                    --  -> (DTime,Maybe a)
                                    --  -> IO Bool

-- Embedding (tentative: will be revisited)
    DTime,              -- [s] Sampling interval, always > 0.
    embed,              -- :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
    embedSynch,         -- :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
    deltaEncode,        -- :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
    deltaEncodeBy       -- :: (a -> a -> Bool) -> DTime -> [a]
                        --       -> (a, [(DTime, Maybe a)])
) where

import Control.Monad (unless)
import System.Random (RandomGen(..), Random(), randoms, randomRs)
import Data.IORef
import Control.Arrow
import AFRPDiagnostics
import AFRPMiscellany (( # ), dup, swap)
import AFRPEvent
import AFRPVectorSpace


infixr 0 -->, >--, -=>, >=-

------------------------------------------------------------------------------
-- Basic type definitions with associated utilities
------------------------------------------------------------------------------

-- The time type is really a bit bogus, since, as time passes, the minimal
-- interval between two consecutive floating-point-represented time points
-- increases. A better approach is probably to pick a reasonable resolution
-- and represent time and time intervals by Integer (giving the number of
-- "ticks").

-- Time is used both for time intervals (duration), and time w.r.t. some
-- agreed reference point in time. Conceptually, Time = R, i.e. time can be 0
-- or even negative.
type Time = Double      -- [s]


-- DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }. Don't assume Time and DTime have the
-- same representation.

type DTime = Double -- [s]


-- Representation of signal function in initial state.
-- (Naming: "TF" stands for Transition Function.)

data SF a b = SF {sfTF :: a -> Transition a b}


-- Representation of signal function in running state.
-- It would have been nice to have a constructor SFId representing (arr id):
--
--        SFId {sfTF' :: DTime -> a -> Transition a b}
--
-- But it seems as if we need dependent types as soon as we try to exploit
-- that constructor (note that the type above is too general!), and a
-- work-around based on keeping around an extra function as a "proof" that we
-- can do the required coercions, yields code which is no more efficient
-- than using SFArr in the first place.
-- (Naming: "TIVar" stands for "time-input-variable".)

data SF' a b
    = SFConst {sfTF' :: DTime -> a -> Transition a b, sfCVal :: b}
    | SFArr   {sfTF' :: DTime -> a -> Transition a b, sfAFun :: a -> b}
    | SFTIVar {sfTF' :: DTime -> a -> Transition a b}


-- A transition is a pair of the next state (in the form of a signal
-- function) and the output at the present time step.

type Transition a b = (SF' a b, b)


-- "Smart" constructors. The corresponding "raw" constructors should not
-- be used directly for construction.

sfConst :: b -> SF' a b
sfConst b = sf
    where
        sf = SFConst {sfTF' = \_ _ -> (sf, b), sfCVal = b}


sfNever :: SF' a (Event b)
sfNever = sfConst NoEvent


sfId :: SF' a a
sfId = sf
    where
        sf = SFArr {sfTF' = \_ a -> (sf, a), sfAFun = id}


sfArr :: (a -> b) -> SF' a b
sfArr f = sf
    where
        sf = SFArr {sfTF' = \_ a -> (sf, f a), sfAFun = f}


-- Freezes a "running" signal function, i.e., turns it into a continuation in
-- the form of a plain signal function.
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}


freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (flip freeze dt) sfs


------------------------------------------------------------------------------
-- Arrow instance and implementation
------------------------------------------------------------------------------

instance Arrow SF where
    arr  = arrPrim
    (>>>)  = compPrim
    first  = firstPrim
    second = secondPrim
    (***)  = parSplitPrim
    (&&&)  = parFanOutPrim


-- Lifting.
arrPrim :: (a -> b) -> SF a b
arrPrim f = SF {sfTF = \a -> (sfArr f, f a)}


-- Composition.
-- The definition exploits the following identities:
--        sf       >>> constant c = constant c
--        constant c >>> arr f     = constant (f c)
--        arr f    >>> arr g       = arr (g . f)
-- (It would have been nice to explit e.g. identity >>> sf = sf, but it would
-- seem that we need dependent types for that.)
compPrim :: SF a b -> SF b c -> SF a c
{-# INLINE compPrim #-}
compPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
        tf0 a0 = (cpAux sf1 sf2, c0)
            where
                (sf1, b0) = tf10 a0
                (sf2, c0) = tf20 b0

        cpAux _           sf2@(SFConst {}) = sfConst (sfCVal sf2)
        cpAux sf1@(SFConst {}) sf2                      = cpAuxC1 (sfCVal sf1) sf2
        cpAux sf1@(SFArr {})   sf2                      = cpAuxA1 (sfAFun sf1) sf2
        cpAux sf1                  sf2@(SFArr {})       = cpAuxA2 sf1 (sfAFun sf2)
        cpAux sf1                  sf2                  = SFTIVar {sfTF' = tf}
            where
                   tf dt a = (cpAux sf1' sf2', c)
                    where
                           (sf1', b) = (sfTF' sf1) dt a
                           (sf2', c) = (sfTF' sf2) dt b

        cpAuxC1 _ (SFConst {sfCVal = c})        = sfConst c
        cpAuxC1 b (SFArr   {sfAFun = f2})       = sfConst (f2 b)
        cpAuxC1 b (SFTIVar {sfTF'  = tf2}) = SFTIVar {sfTF' = tf}
            where
                tf dt _ = (cpAuxC1 b sf2', c)
                    where
                        (sf2', c) = tf2 dt b

        cpAuxA1 _  (SFConst {sfCVal = c})        = sfConst c
        cpAuxA1 f1 (SFArr       {sfAFun = f2})  = sfArr (f2 . f1)
        cpAuxA1 f1 (SFTIVar {sfTF'  = tf2}) = SFTIVar {sfTF' = tf}
            where
                tf dt a = (cpAuxA1 f1 sf2', c)
                    where
                        (sf2', c) = tf2 dt (f1 a)

        cpAuxA2 (SFConst {sfCVal = b})   f2 = sfConst (f2 b)
        cpAuxA2 (SFArr   {sfAFun = f1})  f2 = sfArr (f2 . f1)
        cpAuxA2 (SFTIVar {sfTF' = tf1}) f2 = SFTIVar {sfTF' = tf}
            where
                tf dt a = (cpAuxA2 sf1' f2, f2 b)
                    where
                        (sf1', b) = tf1 dt a


-- Widening.
-- The definition exploits the following identities:
--        first (constant b) = arr (\(_, c) -> (b, c))
--        (first (arr f))        = arr (\(a, c) -> (f a, c))
-- (It would have been nice to exploit first identity = identity, but it would
-- seem that we need dependent types for that.)
firstPrim :: SF a b -> SF (a,c) (b,c)
firstPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
           tf0 ~(a0, c0) = (fpAux sf1, (b0, c0))
            where
                (sf1, b0) = tf10 a0
                fpAux (SFConst {sfCVal = b}) = sfArr (\(~(_, c)) -> (b, c))
                fpAux (SFArr    {sfAFun = f}) = sfArr (\(~(a, c)) -> (f a, c))
                fpAux sf11                          = SFTIVar {sfTF' = tf}
                 where
                   tf dt ~(a, c) = (fpAux sf1', (b, c))
                    where
                        (sf1', b) = (sfTF' sf11) dt a


-- Mirror image of first.
secondPrim :: SF a b -> SF (c,a) (c,b)
secondPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
           tf0 ~(c0, a0) = (spAux sf1, (c0, b0))
            where
                (sf1, b0) = tf10 a0
                spAux (SFConst {sfCVal = b}) = sfArr (\(~(c, _)) -> (c, b))
                spAux (SFArr    {sfAFun = f}) = sfArr (\(~(c, a)) -> (c, f a))
                spAux sf11                          = SFTIVar {sfTF' = tf}
                 where
                   tf dt ~(c, a) = (spAux sf1', (c, b))
                    where
                        (sf1', b) = (sfTF' sf11) dt a

-- Parallel composition.
-- The definition exploits the following identities (which hold for SF):
--        constant b *** constant d = constant (b, d)
--        constant b *** arr f2    = arr (\(_, c) -> (b, f2 c)
--        arr f1           *** constant d = arr (\(a, _) -> (f1 a, d)
--        arr f1           *** arr f2      = arr (\(a, b) -> (f1 a, f2 b)
parSplitPrim :: SF a b -> SF c d  -> SF (a,c) (b,d)
parSplitPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
        tf0 ~(a0, c0) = (psAux sf1 sf2, (b0, d0))
            where
                (sf1, b0) = tf10 a0
                (sf2, d0) = tf20 c0

        psAux sf1@(SFConst {}) sf2                      = psAuxC1 (sfCVal sf1) sf2
        psAux sf1                  sf2@(SFConst {}) = psAuxC2 sf1 (sfCVal sf2)
        psAux sf1@(SFArr {})   sf2                      = psAuxA1 (sfAFun sf1) sf2
        psAux sf1                  sf2@(SFArr    {}) = psAuxA2 sf1 (sfAFun sf2)
        psAux sf1         sf2           = SFTIVar {sfTF' = tf}
            where
                tf dt ~(a, c) = (psAux sf1' sf2', (b, d))
                    where
                           (sf1', b) = (sfTF' sf1) dt a
                           (sf2', d) = (sfTF' sf2) dt c

        psAuxC1 b (SFConst {sfCVal = d})        = sfConst (b, d)
        psAuxC1 b (SFArr   {sfAFun = f2})       = sfArr (\(~(_, c)) -> (b, f2 c))
        psAuxC1 b (SFTIVar {sfTF'  = tf2}) = SFTIVar {sfTF' = tf}
            where
                tf dt ~(_, c) = (psAuxC1 b sf2', (b, d))
                    where
                        (sf2', d) = tf2 dt c

        psAuxC2 (SFConst {sfCVal = b})   d = sfConst (b, d)
        psAuxC2 (SFArr   {sfAFun = f1})  d = sfArr (\(~(a, _)) -> (f1 a, d))
        psAuxC2 (SFTIVar {sfTF' = tf1}) d = SFTIVar {sfTF' = tf}
            where
                tf dt ~(a, _) = (psAuxC2 sf1' d, (b, d))
                    where
                        (sf1', b) = tf1 dt a

        psAuxA1 f1 (SFConst {sfCVal = d})        = sfArr (\(~(a,_)) -> (f1 a, d))
        psAuxA1 f1 (SFArr       {sfAFun = f2})  = sfArr (\(~(a,c)) -> (f1 a, f2 c))
        psAuxA1 f1 (SFTIVar {sfTF'  = tf2}) = SFTIVar {sfTF' = tf}
            where
                tf dt ~(a, c) = (psAuxA1 f1 sf2', (f1 a, d))
                    where
                        (sf2', d) = tf2 dt c

        psAuxA2 (SFConst {sfCVal = b})   f2 = sfArr (\(~(_,c)) -> (b, f2 c))
        psAuxA2 (SFArr   {sfAFun = f1})  f2 = sfArr (\(~(a,c)) -> (f1 a, f2 c))
        psAuxA2 (SFTIVar {sfTF' = tf1}) f2 = SFTIVar {sfTF' = tf}
            where
                tf dt ~(a, c) = (psAuxA2 sf1' f2, (b, f2 c))
                    where
                        (sf1', b) = tf1 dt a


parFanOutPrim :: SF a b -> SF a c -> SF a (b, c)
parFanOutPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
        tf0 a0 = (pfoAux sf1 sf2, (b0, c0))
            where
                (sf1, b0) = tf10 a0
                (sf2, c0) = tf20 a0

        pfoAux sf1@(SFConst {}) sf2                      = pfoAuxC1 (sfCVal sf1) sf2
        pfoAux sf1                  sf2@(SFConst {}) = pfoAuxC2 sf1 (sfCVal sf2)
        pfoAux sf1@(SFArr {})   sf2                      = pfoAuxA1 (sfAFun sf1) sf2
        pfoAux sf1                  sf2@(SFArr   {}) = pfoAuxA2 sf1 (sfAFun sf2)
        pfoAux sf1                 sf2           = SFTIVar {sfTF' = tf}
            where
                tf dt a = (pfoAux sf1' sf2', (b, c))
                    where
                           (sf1', b) = (sfTF' sf1) dt a
                           (sf2', c) = (sfTF' sf2) dt a

        pfoAuxC1 b (SFConst {sfCVal = c})        = sfConst (b, c)
        pfoAuxC1 b (SFArr       {sfAFun = f2})  = sfArr (\a -> (b, f2 a))
        pfoAuxC1 b (SFTIVar {sfTF'  = tf2}) = SFTIVar {sfTF' = tf}
            where
                tf dt a = (pfoAuxC1 b sf2', (b, c))
                    where
                        (sf2', c) = tf2 dt a

        pfoAuxC2 (SFConst {sfCVal = b})   c = sfConst (b, c)
        pfoAuxC2 (SFArr   {sfAFun = f1})  c = sfArr (\a -> (f1 a, c))
        pfoAuxC2 (SFTIVar {sfTF'  = tf1}) c = SFTIVar {sfTF' = tf}
            where
                tf dt a = (pfoAuxC2 sf1' c, (b, c))
                    where
                        (sf1', b) = tf1 dt a

        pfoAuxA1 f1 (SFConst {sfCVal = c})   = sfArr (\a -> (f1 a, c))
        pfoAuxA1 f1 (SFArr       {sfAFun = f2})  = sfArr (\a -> (f1 a ,f2 a))
        pfoAuxA1 f1 (SFTIVar {sfTF'  = tf2}) = SFTIVar {sfTF' = tf}
            where
                tf dt a = (pfoAuxA1 f1 sf2', (f1 a, c))
                    where
                        (sf2', c) = tf2 dt a

        pfoAuxA2 (SFConst {sfCVal = b})   f2 = sfArr (\a -> (b, f2 a))
        pfoAuxA2 (SFArr   {sfAFun = f1})  f2 = sfArr (\a -> (f1 a, f2 a))
        pfoAuxA2 (SFTIVar {sfTF'  = tf1}) f2 = SFTIVar {sfTF' = tf}
            where
                tf dt a = (pfoAuxA2 sf1' f2, (b, f2 a))
                    where
                        (sf1', b) = tf1 dt a


------------------------------------------------------------------------------
-- ArrowLoop instance and implementation
------------------------------------------------------------------------------

instance ArrowLoop SF where
    loop = loopPrim


loopPrim :: SF (a,c) (b,c) -> SF a b
loopPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
        tf0 a0 = (loopAux sf1, b0)
            where
                   (sf1, (b0, c0)) = tf10 (a0, c0)

                   loopAux (SFConst {sfCVal = (b, _)}) = sfConst b
                   loopAux (SFArr       {sfAFun = f1})     = sfArr (\a -> let (b,c) = f1 (a,c)
                                                                                            in b)
                   loopAux sf11 = SFTIVar {sfTF' = tf}
                        where
                         tf dt a = (loopAux sf1', b)
                          where
                           (sf1', (b, c)) = (sfTF' sf11) dt (a, c)


------------------------------------------------------------------------------
-- Basic signal functions
------------------------------------------------------------------------------

-- Identity: identity = arr id
identity :: SF a a
identity = SF {sfTF = \a -> (sfId, a)}


-- Identity: constant b = arr (const b)
constant :: b -> SF a b
constant b = SF {sfTF = \_ -> (sfConst b, b)}


-- Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral


-- Alternative name for localTime.
time :: SF a Time
time = localTime


------------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------------

-- Initialization operator (cf. Lustre/Lucid Synchrone).
(-->) :: b -> SF a b -> SF a b
b0 --> (SF {sfTF = tf10}) = SF {sfTF = \a0 -> (fst (tf10 a0), b0)}


-- Input initialization operator.
(>--) :: a -> SF a b -> SF a b
a0 >-- (SF {sfTF = tf10}) = SF {sfTF = \_ -> tf10 a0}


-- Transform initial output value.
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF {sfTF = tf10}) =
    SF {sfTF = \a0 -> let (sf1, b0) = tf10 a0 in (sf1, f b0)}


-- Transform initial input value.
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF {sfTF = tf10}) = SF {sfTF = \a0 -> tf10 (f a0)}


-- Override initial value of input signal.
initially :: a -> SF a a
initially = (--> identity)


------------------------------------------------------------------------------
-- Basic event sources
------------------------------------------------------------------------------

-- Event source which never occurs.
never :: SF a (Event b)
never = SF {sfTF = \_ -> (sfNever, NoEvent)}


-- Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: b -> SF a (Event b)
now b0 = (Event b0 --> never)


-- Event source with a single occurrence at or as soon after (local) time q
-- as possible.
after :: Time -> b -> SF a (Event b)
after q x = afterEach [(q,x)]


-- Event source with repeated occurrences with interval q.
-- Note: If the interval is too short w.r.t. the sampling intervals,
-- the result will be that events occur at every sample. However, no more
-- than one event results from any sampling interval, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.
repeatedly :: Time -> b -> SF a (Event b)
repeatedly q x | q > 0 = afterEach qxs
                        | otherwise = usrErr "AFRP" "repeatedly" "Non-positive period."
    where
           qxs = (q,x):qxs


-- Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.
-- Question: Should positive periods except for the first one be required?
-- Note that periods of length 0 will always be skipped except for the first.
-- Right now, periods of length 0 is allowed on the grounds that no attempt
-- is made to forbid simultaneous events elsewhere.
afterEach :: [(Time,b)] -> SF a (Event b)
afterEach [] = never
afterEach ((q,x):qxs)
    | q < 0      = usrErr "AFRP" "afterEach" "Negative period."
    | otherwise = SF {sfTF = tf0}
    where
        tf0 _ = if q <= 0 then
                                (scheduleNextEvent 0.0 qxs, Event x)
                         else
                    (awaitNextEvent (-q) x qxs, NoEvent)

        scheduleNextEvent _ [] = sfNever
        scheduleNextEvent t ((r,y):qs)
            | r < 0      = usrErr "AFRP" "afterEach" "Negative period."
            | t' >= 0    = scheduleNextEvent t' qs
            | otherwise = awaitNextEvent t' y qs
            where
                   t' = t - r
        awaitNextEvent t y qs = SFTIVar {sfTF' = tf}
            where
                tf dt _ | t' >= 0       = (scheduleNextEvent t' qs, Event y)
                           | otherwise = (awaitNextEvent t' y qs, NoEvent)
                    where
                           t' = t + dt

-- A rising edge detector. Useful for things like detecting key presses.
-- Note that we initialize the loop with state set to True so that there
-- will not be an occurence at t0 in the logical time frame in which
-- this is started.
edge :: SF Bool (Event ())
edge = iEdge True

iEdge :: Bool -> SF Bool (Event ())
iEdge i = edgeBy (isBoolRaisingEdge ()) i

-- Like edge, but parameterized on the tag value.
edgeTag :: a -> SF Bool (Event a)
edgeTag a = edgeBy (isBoolRaisingEdge a) True


-- Internal utility.
isBoolRaisingEdge :: a -> Bool -> Bool -> Maybe a
isBoolRaisingEdge _ False False = Nothing
isBoolRaisingEdge a False True  = Just a
isBoolRaisingEdge _ True  True  = Nothing
isBoolRaisingEdge _ True  False = Nothing


-- Detects an edge where a maybe signal is changing from nothing to something.
edgeJust :: SF (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
           isJustEdge Nothing  Nothing  = Nothing
           isJustEdge Nothing  ma@(Just _) = ma
           isJustEdge (Just _) (Just _) = Nothing
           isJustEdge (Just _) Nothing  = Nothing


-- Edge detector parameterized on the edge detection function and initial
-- state, i.e., the previous input sample. The first argument to the
-- edge detection function is the previous sample, the second the current one.

-- !!! Is this broken!?! Does not disallow an edge condition that persists
-- !!! between consecutive samples. See discussion in ToDo list above.

edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a_init = SF {sfTF = tf0}
    where
        tf0 a0 = (ebAux a0, maybeToEvent (isEdge a_init a0))

        ebAux a_prev = SFTIVar {sfTF' = tf}
            where
                tf _ a = (ebAux a, maybeToEvent (isEdge a_prev a))


------------------------------------------------------------------------------
-- Stateful event suppression
------------------------------------------------------------------------------

-- Suppression of initial (at local time 0) event.
notYet :: SF (Event a) (Event a)
notYet = initially NoEvent


-- Suppress all but first event.
once :: SF (Event a) (Event a)
once = takeEvents 1


-- Suppress all but first n events.
takeEvents :: Int -> SF (Event a) (Event a)
takeEvents 0       = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))


{-
-- More complicated using "switch" that "dSwitch".
takeEvents :: Int -> SF (Event a) (Event a)
takeEvents 0        = never
takeEvents (n + 1) = switch (never &&& identity) (takeEvents' n)
    where
           takeEvents' 0           a = now a
           takeEvents' (n + 1) a = switch (now a &&& notYet) (takeEvents' n)
-}


-- Suppress first n events.
-- Here dSwitch or switch does not really matter.
dropEvents :: Int -> SF (Event a) (Event a)
dropEvents 0        = identity
dropEvents (n + 1) = dSwitch (never &&& identity)
                                            (const (NoEvent >-- dropEvents n))


------------------------------------------------------------------------------
-- Basic switchers
------------------------------------------------------------------------------

-- Basic switch.
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            case tf10 a0 of
                (sf1, (b0, NoEvent))  -> (switchAux sf1, b0)
                (_,      (_,  Event c0)) -> sfTF (k c0) a0

        switchAux (SFConst {sfCVal = (b, NoEvent)}) = sfConst b
        switchAux (SFArr   {sfAFun = f1})                   = switchAuxA1 f1
        switchAux sf1                                               = SFTIVar {sfTF' = tf}
            where
                tf dt a =
                    case (sfTF' sf1) dt a of
                        (sf1', (b, NoEvent)) -> (switchAux sf1', b)
                        (_,       (_, Event c)) -> sfTF (k c) a

        -- Note: While switch behaves as a stateless arrow at this point, that
        -- could change after a switch. Hence, SFTIVar overall.
        switchAuxA1 f1 = sf
            where
                sf        = SFTIVar {sfTF' = tf}
                tf _ a =
                    case f1 a of
                        (b, NoEvent) -> (sf, b)
                        (_, Event c) -> sfTF (k c) a


-- Switch with delayed observation.
dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
dSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let (sf1, (b0, ec0)) = tf10 a0
                  in (case ec0 of
                           NoEvent  -> dSwitchAux sf1
                           Event c0 -> fst (sfTF (k c0) a0),b0)

        dSwitchAux (SFConst {sfCVal = (b, NoEvent)}) = sfConst b
        dSwitchAux (SFArr       {sfAFun = f1})          = dSwitchAuxA1 f1
        dSwitchAux sf1                                          = SFTIVar {sfTF' = tf}
            where
                tf dt a =
                    let (sf1', (b, ec)) = (sfTF' sf1) dt a
                                in (case ec of
                            NoEvent -> dSwitchAux sf1'
                            Event c -> fst (sfTF (k c) a),

                        b)

        -- Note: While dSwitch behaves as a stateless arrow at this point, that
        -- could change after a switch. Hence, SFTIVar overall.
        dSwitchAuxA1 f1 = sf
            where
                sf = SFTIVar {sfTF' = tf}
                tf _ a =
                    let (b, ec) = f1 a
                                in (case ec of
                            NoEvent -> sf
                            Event c -> fst (sfTF (k c) a),

                        b)


-- Recurring switch.
rSwitch :: SF a b -> SF (a, Event (SF a b)) b
rSwitch sf = switch (first sf) ((noEventSnd >=-) . rSwitch)

{-
-- Old version. New is more efficient. Which one is clearer?
rSwitch :: SF a b -> SF (a, Event (SF a b)) b
rSwitch sf = switch (first sf) rSwitch'
    where
           rSwitch' sf = switch (sf *** notYet) rSwitch'
-}


-- Recurring switch with delayed observation.
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) ((noEventSnd >=-) . drSwitch)

{-
-- Old version. New is more efficient. Which one is clearer?
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) drSwitch'
    where
           drSwitch' sf = dSwitch (sf *** notYet) drSwitch'
-}


-- "Call-with-current-continuation" switch.
kSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
kSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
           tf0 a0 =
            let (sf1, b0) = tf10 a0
                  in
                   case tfe0 (a0, b0) of
                    (sfe, NoEvent)      -> (kSwitchAux sf1 sfe, b0)
                    (_, Event c0) -> sfTF (k sf10 c0) a0

           kSwitchAux sf1 (SFConst {sfCVal = NoEvent}) = sf1
           kSwitchAux sf1 sfe                                     = SFTIVar {sfTF' = tf}
            where
                tf dt a =
                    let (sf1', b) = (sfTF' sf1) dt a
                    in
                           case (sfTF' sfe) dt (a, b) of
                            (sfe', NoEvent) -> (kSwitchAux sf1' sfe', b)
                            (_,  Event c) -> sfTF (k (freeze sf1 dt) c) a


-- kSwitch with delayed observation.
dkSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
dkSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
           tf0 a0 =
            let (sf1, b0) = tf10 a0
                  in (case tfe0 (a0, b0) of
                    (sfe, NoEvent)      -> dkSwitchAux sf1 sfe
                    (_, Event c0) -> fst (sfTF (k sf10 c0) a0),
                         b0)

           dkSwitchAux sf1 (SFConst {sfCVal = NoEvent}) = sf1
           dkSwitchAux sf1 sfe                                     = SFTIVar {sfTF' = tf}
            where
                tf dt a =
                    let (sf1', b) = (sfTF' sf1) dt a
                    in (case (sfTF' sfe) dt (a, b) of
                            (sfe', NoEvent) -> dkSwitchAux sf1' sfe'
                            (_, Event c) -> fst (sfTF (k (freeze sf1 dt) c) a),
                           b)


------------------------------------------------------------------------------
-- Parallel composition and switching over collections with broadcasting
------------------------------------------------------------------------------

broadcast :: Functor col => a -> col sf -> col (a, sf)
broadcast a sfs = fmap (\sf -> (a, sf)) sfs


-- Spatial parallel composition of a signal function collection.
parB :: Functor col => col (SF a b) -> SF a (col b)
parB = par broadcast


-- Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel).
pSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c-> SF a (col b))
    -> SF a (col b)
pSwitchB = pSwitch broadcast


dpSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c->SF a (col b))
    -> SF a (col b)
dpSwitchB = dpSwitch broadcast


rpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
rpSwitchB = rpSwitch broadcast


drpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
drpSwitchB = drpSwitch broadcast


------------------------------------------------------------------------------
-- Parallel composition and switching over collections with general routing
------------------------------------------------------------------------------

-- Spatial parallel composition of a signal function collection parameterized
-- on the routing function.
-- rf ......... Routing function: determines the input to each signal function
--              in the collection. IMPORTANT! The routing function MUST
--              preserve the structure of the signal function collection.
-- sfs0 ....... Signal function collection.
-- Returns the spatial parallel composition of the supplied signal functions.

par :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF a (col c)
par rf sfs0 = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
                sfs      = fmap fst sfcs0
                cs0      = fmap snd sfcs0
            in
                (parAux rf sfs, cs0)


-- Internal definition. Also used in parallel swithers.
parAux :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF' b c)
    -> SF' a (col c)
parAux rf sfs = SFTIVar {sfTF' = tf}
    where
        tf dt a =
            let bsfs  = rf a sfs
                sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                sfs'  = fmap fst sfcs'
                cs       = fmap snd sfcs'
            in
                   (parAux rf sfs', cs)


-- Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle
-- can be derived. The signal function collection is spatially composed in
-- parallel and run until the event signal function has an occurrence. Once
-- the switching event occurs, all signal function are "frozen" and their
-- continuations are passed to the continuation function, along with the
-- event value.
-- rf ......... Routing function: determines the input to each signal function
--              in the collection. IMPORTANT! The routing function has an
--              obligation to preserve the structure of the signal function
--              collection.
-- sfs0 ....... Signal function collection.
-- sfe0 ....... Signal function generating the switching event.
-- k .......... Continuation to be invoked once event occurs.
-- Returns the resulting signal function.

pSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
    -> SF a (col c)
pSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
                sfs      = fmap fst sfcs0
                cs0      = fmap snd sfcs0
            in
                case (sfTF sfe0) (a0, cs0) of
                    (sfe, NoEvent)      -> (pSwitchAux sfs sfe, cs0)
                    (_, Event d0) -> sfTF (k sfs0 d0) a0

        pSwitchAux sfs (SFConst {sfCVal = NoEvent}) = parAux rf sfs
        pSwitchAux sfs sfe = SFTIVar {sfTF' = tf}
            where
                tf dt a =
                    let bsfs  = rf a sfs
                        sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                        sfs'  = fmap fst sfcs'
                        cs       = fmap snd sfcs'
                    in
                        case (sfTF' sfe) dt (a, cs) of
                            (sfe', NoEvent) -> (pSwitchAux sfs' sfe', cs)
                            (_,  Event d) -> sfTF (k (freezeCol sfs dt) d) a


-- Parallel switch with delayed observation parameterized on the routing
-- function.
dpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
    -> SF a (col c)
dpSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
                cs0      = fmap snd sfcs0
            in
                (case (sfTF sfe0) (a0, cs0) of
                        (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
                        (_,      Event d0) -> fst (sfTF (k sfs0 d0) a0),
                    cs0)

        dpSwitchAux sfs (SFConst {sfCVal = NoEvent}) = parAux rf sfs
        dpSwitchAux sfs sfe = SFTIVar {sfTF' = tf}
            where
                tf dt a =
                    let bsfs  = rf a sfs
                        sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                        cs       = fmap snd sfcs'
                    in
                        (case (sfTF' sfe) dt (a, cs) of
                                (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs')
                                                            sfe'
                                (_,       Event d) -> fst (sfTF (k (freezeCol sfs dt)
                                                                d)
                                                          a),
                                        cs)


-- Recurring parallel switch parameterized on the routing function.
-- rf ......... Routing function: determines the input to each signal function
--              in the collection. IMPORTANT! The routing function has an
--              obligation to preserve the structure of the signal function
--              collection.
-- sfs ........ Initial signal function collection.
-- Returns the resulting signal function.

rpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
rpSwitch rf sfs =
    pSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- rpSwitch rf (f sfs')


{-
rpSwitch rf sfs = pSwitch (rf . fst) sfs (arr (snd . fst)) k
    where
        k sfs f = rpSwitch' (f sfs)
        rpSwitch' sfs = pSwitch (rf . fst) sfs (NoEvent --> arr (snd . fst)) k
-}

-- Recurring parallel switch with delayed observation parameterized on the
-- routing function.
drpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
drpSwitch rf sfs =
    dpSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- drpSwitch rf (f sfs')

{-
drpSwitch rf sfs = dpSwitch (rf . fst) sfs (arr (snd . fst)) k
    where
        k sfs f = drpSwitch' (f sfs)
        drpSwitch' sfs = dpSwitch (rf . fst) sfs (NoEvent-->arr (snd . fst)) k
-}

------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

-- Zero-order hold.
hold :: a -> SF (Event a) a
hold a_init = switch (constant a_init &&& identity) ((NoEvent >--) . hold)


-- Tracks input signal when available, holds last value when disappears.
trackAndHold :: a -> SF (Maybe a) a
trackAndHold a_init = arr (maybe NoEvent Event) >>> hold a_init


------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

accum :: a -> SF (Event (a -> a)) (Event a)
accum = accumBy (flip ($))

accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy f b_init = switch (never &&& identity) $ \a -> abAux (f b_init a)
    where
           abAux b = switch (now b &&& notYet) $ \a -> abAux (f b a)


{-
-- Identity: accumBy f = accumFilter (\b a -> let b' = f b a in (b',Just b'))
accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy f b_init = SF {sfTF = tf0}
    where
           tf0 NoEvent    = (abAux b_init, NoEvent)
           tf0 (Event a0) = let b' = f b_init a0
                            in (abAux b', Event b')

           abAux b = SFTIVar {sfTF' = tf}
            where
                tf _ NoEvent    = (abAux b, NoEvent)
                tf _ (Event a) = let b' = f b a
                                    in (abAux b', Event b')
-}

{-
accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter f c_init = SF {sfTF = tf0}
    where
           tf0 NoEvent    = (afAux c_init, NoEvent)
           tf0 (Event a0) = case f c_init a0 of
                                   (c', Nothing) -> (afAux c', NoEvent)
                                (c', Just b0) -> (afAux c', Event b0)

           afAux c = SFTIVar {sfTF' = tf}
            where
                tf _ NoEvent    = (afAux c, NoEvent)
                tf _ (Event a) = case f c a of
                                           (c', Nothing) -> (afAux c', NoEvent)
                                        (c', Just b)  -> (afAux c', Event b)
-}


accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter f c_init = switch (never &&& identity) $ \a -> afAux (f c_init a)
    where
           afAux (c, Nothing) = switch (never &&& notYet) $ \a -> afAux (f c a)
           afAux (c, Just b)  = switch (now b &&& notYet) $ \a -> afAux (f c a)


------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- Uninitialized delay operator.
-- !!! The seq helps in the dynamic delay line example. But is it a good
-- !!! idea in general? Are there other accumulators which should be seq'ed
-- !!! as well? E.g. accum? Switch? Anywhere else? What's the underlying
-- !!! design principle? What can the user assume?
pre :: SF a a
pre = SF {sfTF = tf0}
    where
           tf0 a0 = (preAux a0, usrErr "AFRP" "pre" "Uninitialized pre operator.")
           preAux a_prev = SFTIVar {sfTF' = tf}
            where
                tf _ a = {- a_prev `seq` -} (preAux a, a_prev)


-- Initialized delay operator.
iPre :: a -> SF a a
iPre = (--> pre)


------------------------------------------------------------------------------
-- Integraltion and differentiation
------------------------------------------------------------------------------

-- Integration using the rectangle rule.
integral :: VectorSpace a s => SF a a
integral = SF {sfTF = tf0}
    where
           igrl0        = zeroVector
           tf0 a0 = (integralAux igrl0 a0, igrl0)
           integralAux igrl a_prev = SFTIVar {sfTF' = tf}
            where
                   tf dt a = (integralAux igrl' a, igrl')
                    where
                          igrl' = igrl ^+^ realToFrac dt *^ a_prev



-- "immediate" integration (using the function's value at the current time)
imIntegral :: VectorSpace a s => a -> SF a a
imIntegral = ((\ _ a' dt v -> v ^+^ realToFrac dt *^ a') `iterFrom`)

iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
f `iterFrom` b = SF (iterAux b) where
  iterAux c d = (SFTIVar (\ dt a' -> iterAux (f d a' dt c) a'), c)

{-
integral :: Fractional a => SF a a
integral = SF {sfTF = tf0}
    where
           igrl0        = 0.0

        tf0 a0 = (integralAux igrl0 a0, igrl0)

        integralAux igrl a_prev = SFTIVar {sfTF' = tf}
            where
                   tf dt a = (integralAux igrl' a, igrl')
                    where
                          igrl' = igrl + a_prev * realToFrac dt
-}

-- This is extremely crude. Use at your own risk.
derivative :: VectorSpace a s => SF a a
derivative = SF {sfTF = tf0}
    where
        tf0 a0 = (derivativeAux a0, zeroVector)

        derivativeAux a_prev = SFTIVar {sfTF' = tf}
            where
                   tf dt a = (derivativeAux a, (a ^-^ a_prev) ^/ realToFrac dt)


------------------------------------------------------------------------------
-- Loops with guaranteed well-defined feedback
------------------------------------------------------------------------------

loopPre :: c -> SF (a,c) (b,c) -> SF a b
loopPre c_init sf = loop (second (iPre c_init) >>> sf)



loopIntegral :: VectorSpace c s => SF (a,c) (b,c) -> SF a b
loopIntegral sf = loop (second integral >>> sf)


------------------------------------------------------------------------------
-- Noise (i.e. random signal generators) and stochastic processes
------------------------------------------------------------------------------

-- Noise (random signal) with default range for type in question;
-- based on "randoms".
noise :: (RandomGen g, Random b) => g -> SF a b
noise g0 = streamToSF (randoms g0)


-- Noise (random signal) with specified range; based on "randomRs".
noiseR :: (RandomGen g, Random b) => (b,b) -> g -> SF a b
noiseR range g0 = streamToSF (randomRs range g0)


-- Internal. Not very useful for other purposes since we do not have any
-- control over the intervals between each "sample". Or? A version with
-- time-stamped samples would be similar to embedSynch (applied to identity).
-- The list argument must be a stream (infinite list) at present.

streamToSF :: [b] -> SF a b
streamToSF []      = intErr "AFRP" "streamToSF" "Empty list!"
streamToSF (b:bs) = SF {sfTF = tf0}
    where
           tf0 _ = (stsfAux bs, b)

           stsfAux []      = intErr "AFRP" "streamToSF" "Empty list!"
           stsfAux (c:cs) = SFTIVar {sfTF' = tf}
            where
                tf _ _ = (stsfAux cs, c)


-- Stochastic event source with events occurring on average once every t_avg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.
-- !!! Maybe it would better to give a frequency? But like this to make
-- !!! consitent with "repeatedly".
occasionally :: RandomGen g => g -> Time -> b -> SF a (Event b)
occasionally g t_avg x | t_avg > 0 = SF {sfTF = tf0}
                                   | otherwise = usrErr "AFRP" "occasionally"
                                                  "Non-positive average interval."
    where
        -- Generally, if events occur with an average frequency of f, the
        -- probability of at least one event occurring in an interval of t
           -- is given by (1 - exp (-f*t)). The goal in the following is to
        -- decide whether at least one event occurred in the interval of size
        -- dt preceding the current sample point. For the first point,
        -- we can think of the preceding interval as being 0, implying
        -- no probability of an event occurring.

        tf0 _ = (occAux ((randoms g) :: [Double]), NoEvent)

        occAux (r:rs) = SFTIVar {sfTF' = tf}
            where
                tf dt _ = let p = 1 - exp (-(dt/t_avg)) -- Probability for at
                          in                    -- least one event.
                                 (occAux rs, if r < p then Event x else NoEvent)


------------------------------------------------------------------------------
-- Reactimation
------------------------------------------------------------------------------

-- Reactimation of a signal function.
-- init ....... IO action for initialization. Will only be invoked once,
--              at (logical) time 0, before first call to "sense".
--              Expected to return the value of input at time 0.
-- sense ...... IO action for sensing of system input.
--      arg. #1 ....... True: action may block, waiting for an OS event.
--                      False: action must not block.
--      res. #1 ....... Time interval since previous invocation of the sensing
--                      action (or, the first time round, the init action),
--                      returned. The interval must be _strictly_ greater
--                      than 0. Thus even a non-blocking invocation must
--                      ensure that time progresses.
--      res. #2 ....... Nothing: input is unchanged w.r.t. the previously
--                      returned input sample.
--                      Just i: the input is currently i.
--                      It is OK to always return "Just", even if input is
--                      unchanged.
-- actuate .... IO action for outputting the system output.
--      arg. #1 ....... True: output may have changed from previous output
--                      sample.
--                      False: output is definitely unchanged from previous
--                      output sample.
--                      It is OK to ignore argument #1 and assume that the
--                      the output has always changed.
--      arg. #2 ....... Current output sample.
--      result ....... Termination flag. Once True, reactimate will exit
--                      the reactimation loop and return to its caller.
-- sf ......... Signal function to reactimate.

reactimate :: IO a
                 -> (Bool -> IO (DTime, Maybe a))
                 -> (Bool -> b -> IO Bool)
                    -> SF a b
                 -> IO ()
reactimate int sense actuate (SF {sfTF = tf0}) =
    do     a0 <- int
           let (sf, b0) = tf0 a0
           lp sf a0 b0
    where
           lp sf a b = do
                  done <- actuate True b
                  unless (a `seq` b `seq` done) $ do
                  (dt, ma') <- sense False
                  let
                   a' = maybe a id ma'
                   (sf', b') = (sfTF' sf) dt a'
                  lp sf' a' b'

-- An API for animating a signal function when some other library
-- needs to own the top-level control flow:

-- reactimate's state, maintained across samples:
data ReactState a b = ReactState {
    rsActuate :: (ReactHandle a b -> Bool -> b -> IO Bool),
    rsSF :: (SF' a b),
    rsA :: a,
    rsB :: b
  }

type ReactHandle a b = IORef (ReactState a b)

-- initialize top-level reaction handle
reactInit :: IO a -- init
                   -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
                   -> SF a b
                   -> IO (ReactHandle a b)
reactInit int actuate (SF {sfTF = tf0}) =
  do
   a0 <- int
        --print "blah1"
   let (sf,b0) = tf0 a0
        --print "blah2"
        -- TODO: really need to fix this interface, since right now we
        -- just ignore termination at time 0:
   r <- newIORef (ReactState {rsActuate = actuate, rsSF = sf,
                                rsA = a0, rsB = b0 })
        --print "blah3"
   actuate r True b0
        --print "blah4"
   return r

-- process a single input sample:
react :: ReactHandle a b
         -> (DTime,Maybe a)
         -> IO Bool
react rh (dt,ma') =
  do
   rs@(ReactState {rsActuate = actuate,
                           rsSF = sf,
                        rsA = a,
                        rsB = _ }) <- readIORef rh
   let
    a' = maybe a id ma'
    (sf',b') = (sfTF' sf) dt a'
   writeIORef rh (rs {rsSF = sf',rsA = a',rsB = b'})
   done <- actuate rh True b'
   return done


------------------------------------------------------------------------------
-- Embedding
------------------------------------------------------------------------------

-- New embed interface. We will probably have to revisit this. To run an
-- embedded signal function while retaining full control (e.g. start and
-- stop at will), one would probably need a continuation based interface
-- (as well as a continuation based underlying implementation).
--
-- E.g. here are interesting alternative (or maybe complementary)
-- signatures:
--
--       sample :: SF a b -> SF (Event a) (Event b)
--       sample' :: SF a b -> SF (Event (DTime, a)) (Event b)

embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed sf0 (a0, dtas) = b0 : lp a0 sf dtas
    where
        (sf, b0) = (sfTF sf0) a0

        lp _ _ [] = []
        lp a_prev fs ((dt, ma) : dts) =
            b : (a `seq` b `seq` (lp a sf' dts))
            where
                a           = maybe a_prev id ma
                (sf', b) = (sfTF' fs) dt a


-- Synchronous embedding. The embedded signal function is run on the supplied
-- input and time stream at a given (but variable) ratio >= 0 to the outer
-- time flow. When the ratio is 0, the embedded signal function is paused.

-- !!! Should "dropped frames" be forced to avoid space leaks?
-- !!! It's kind of hard to se why, but "frame dropping" was a problem
-- !!! in the old robot simulator. Try to find an example!

embedSynch :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
embedSynch sf0 (a0, dtas) = SF {sfTF = tf0}
    where
        tts             = scanl (\t (dt, _) -> t + dt) 0 dtas
        bbs@(b:_) = embed sf0 (a0, dtas)

        tf0 _ = (esAux 0 (zip tts bbs), b)

        esAux _     []  = intErr "AFRP" "embedSynch" "Empty list!"
        esAux tp_prev tbtbs = SFTIVar {sfTF' = tf}
            where
                tf dt r | r < 0 = usrErr "AFRP" "embedSynch"
                                                "Negative ratio."
                        | otherwise = let tp = tp_prev + dt * r
                                          (c, tbtbs') = advance tp tbtbs
                                         in
                                          (esAux tp tbtbs', c)

                -- Advance the time stamped stream to the perceived time tp.
                -- Under the assumption that the perceived time never goes
                -- backwards (non-negative ratio), advance maintains the
                -- invariant that the perceived time is always >= the first
                -- time stamp.
                advance _ ttbs@[(_, n)] = (n, ttbs)
                advance tp ((_, c) : ts@((t', _) : _))
                    | tp <  t' = (c, ts)
                    | t' <= tp = advance tp ts


deltaEncode :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncode _   []              = usrErr "AFRP" "deltaEncode" "Empty input list."
deltaEncode dt aas@(_:_) = deltaEncodeBy (==) dt aas


deltaEncodeBy :: (a -> a -> Bool) -> DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncodeBy _  _      []         = usrErr "AFRP" "deltaEncodeBy" "Empty input list."
deltaEncodeBy eq dt (a0:as) = (a0, zip (repeat dt) (debAux a0 as))
    where
        debAux _ []                               = []
        debAux a_prev (b:bs) | b `eq` a_prev = Nothing : debAux b bs
                                            | otherwise = Just b : debAux b bs
