{- $Id: AFRPUtilities.hs,v 1.8 2003/12/19 15:31:50 henrik Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPUtilities                                        *
*       Purpose:        Derived utility definitions.                         *
*       Authors:        Antony Courtney and Henrik Nilsson                   *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

-- ToDo:
-- * Possibly add
--       impulse :: VectorSpace a k => a -> Event a
--   But to do that, we need access to Event, which we currently do not have.
-- * The general arrow utilities should be moved to a module
--   AFRPArrowUtilities.
-- * I'm not sure structuring the AFRP "core" according to what is
--   core functionality and what's not is all that useful. There are
--   many cases where we want to implement combinators that fairly
--   easily could be implemented in terms of others as primitives simply
--   because we expect that that implementation is going to be much more
--   efficient, and that the combinators are used sufficiently often to
--   warrant doing this. E.g. "switch" should be a primitive, even though
--   it could be derived from "pSwitch".
-- * Reconsider "recur". If an event source has an immediate occurrence,
--   we'll get into a loop. For example: recur now. Maybe suppress
--   initial occurrences? Initial occurrences are rather pointless in this
--   case anyway.


module AFRPUtilities (
-- General arrow utilities
    --(^>>),            -- :: Arrow a => (b -> c) -> a c d -> a b d
    --(>>^),            -- :: Arrow a => a b c -> (c -> d) -> a b d
    --(^<<),            -- :: Arrow a => (c -> d) -> a b c -> a b d
    --(<<^),            -- :: Arrow a => a c d -> (b -> c) -> a b d

-- Liftings
    arr2,               -- :: Arrow a => (b->c->d) -> a (b,c) d
    arr3,               -- :: Arrow a => (b->c->d->e) -> a (b,c,d) e
    arr4,               -- :: Arrow a => (b->c->d->e->f) -> a (b,c,d,e) f
    arr5,               -- :: Arrow a => (b->c->d->e->f->g) -> a (b,c,d,e,f) g
    lift0,              -- :: Arrow a => c -> a b c
    lift1,              -- :: Arrow a => (c->d) -> (a b c->a b d)
    lift2,              -- :: Arrow a => (c->d->e) -> (a b c->a b d->a b e)
    lift3,              -- :: Arrow a => (c->d->e->f) -> (a b c-> ... ->a b f)
    lift4,              -- :: Arrow a => (c->d->e->f->g) -> (a b c->...->a b g)
    lift5,              -- :: Arrow a => (c->d->e->f->g->h)->(a b c->...a b h)

-- Event sources
    snap,               -- :: SF a (Event a)
    snapAfter,          -- :: Time -> SF a (Event a)
    sample,             -- :: Time -> SF a (Event a)
    recur,              -- :: SF a (Event b) -> SF a (Event b)

-- Parallel composition/switchers with "zip" routing
    parZ,               -- [SF a b] -> SF [a] [b]
    pSwitchZ,           -- [SF a b] -> SF ([a],[b]) (Event c)
                        -- -> ([SF a b] -> c -> SF [a] [b]) -> SF [a] [b]
    dpSwitchZ,          -- [SF a b] -> SF ([a],[b]) (Event c)
                        -- -> ([SF a b] -> c ->SF [a] [b]) -> SF [a] [b]
    rpSwitchZ,          -- [SF a b] -> SF ([a], Event ([SF a b]->[SF a b])) [b]
    drpSwitchZ,         -- [SF a b] -> SF ([a], Event ([SF a b]->[SF a b])) [b]

-- Guards and automata-oriented combinators
    provided,           -- :: (a -> Bool) -> SF a b -> SF a b -> SF a b

-- Wave-form generation
    dHold,              -- :: a -> SF (Event a) a
    dTrackAndHold,      -- :: a -> SF (Maybe a) a

-- Accumulators
    accumHold,          -- :: a -> SF (Event (a -> a)) a
    dAccumHold,         -- :: a -> SF (Event (a -> a)) a
    accumHoldBy,        -- :: (b -> a -> b) -> b -> SF (Event a) b
    dAccumHoldBy,       -- :: (b -> a -> b) -> b -> SF (Event a) b
    count,              -- :: Integral b => SF (Event a) (Event b)

-- Delays
    fby,                -- :: b -> SF a b -> SF a b,    infixr 0

-- Integrals
    impulseIntegral     -- :: VectorSpace a k => SF (a, Event a) a
) where

import AFRPDiagnostics
import AFRP


--infixr 1 ^<<, ^>>
--infixl 1 <<^, >>^
--infixr 0 `fby`


------------------------------------------------------------------------------
-- General arrow utilities
------------------------------------------------------------------------------

--(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
--f ^>> a = arr f >>> a

--(>>^) :: Arrow a => a b c -> (c -> d) -> a b d
--a >>^ f = a >>> arr f


--(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
--f ^<< a = arr f <<< a


--(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
--a <<^ f = a <<< arr f


------------------------------------------------------------------------------
-- Liftings
------------------------------------------------------------------------------

arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry


arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 = arr . \h (b, c, d) -> h b c d


arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 = arr . \h (b, c, d, e) -> h b c d e


arr5 :: Arrow a => (b -> c -> d -> e -> f -> g) -> a (b, c, d, e, f) g
arr5 = arr . \h (b, c, d, e, f) -> h b c d e f


lift0 :: Arrow a => c -> a b c
lift0 c = arr (const c)


lift1 :: Arrow a => (c -> d) -> (a b c -> a b d)
lift1 f = \a -> a >>> arr f


lift2 :: Arrow a => (c -> d -> e) -> (a b c -> a b d -> a b e)
lift2 f = \a1 a2 -> a1 &&& a2 >>> arr2 f


lift3 :: Arrow a => (c -> d -> e -> f) -> (a b c -> a b d -> a b e -> a b f)
lift3 f = \a1 a2 a3 -> (lift2 f) a1 a2 &&& a3 >>> arr2 ($)


lift4 :: Arrow a => (c->d->e->f->g) -> (a b c->a b d->a b e->a b f->a b g)
lift4 f = \a1 a2 a3 a4 -> (lift3 f) a1 a2 a3 &&& a4 >>> arr2 ($)


lift5 :: Arrow a =>
    (c->d->e->f->g->h) -> (a b c->a b d->a b e->a b f->a b g->a b h)
lift5 f = \a1 a2 a3 a4 a5 ->(lift4 f) a1 a2 a3 a4 &&& a5 >>> arr2 ($)


------------------------------------------------------------------------------
-- Event sources
------------------------------------------------------------------------------

-- Event source with a single occurrence at time 0. The value of the event
-- is obtained by sampling the input at that time.
snap :: SF a (Event a)
snap = switch (never &&& (identity &&& now () >>^ \(a, e) -> e `tag` a)) now


-- Event source with a single occurrence at or as soon after (local) time t_ev
-- as possible. The value of the event is obtained by sampling the input a
-- that time.
snapAfter :: Time -> SF a (Event a)
snapAfter t_ev = switch (never
                         &&& (identity
                              &&& after t_ev () >>^ \(a, e) -> e `tag` a))
                        now


-- Sample a signal at regular intervals.
sample :: Time -> SF a (Event a)
sample p_ev = identity &&& repeatedly p_ev () >>^ \(a, e) -> e `tag` a


-- Makes an event source recurring by restarting it as soon as it has an
-- occurrence.
recur :: SF a (Event b) -> SF a (Event b)
recur sfe = switch (never &&& sfe) recurAux
    where
        recurAux b = switch (now b &&& sfe) recurAux


------------------------------------------------------------------------------
-- Parallel composition/switchers with "zip" routing
------------------------------------------------------------------------------

safeZip :: String -> [a] -> [b] -> [(a,b)]
safeZip fn as bs = safeZip' as bs
    where
        safeZip' _ []     = []
        safeZip' cs (d:ds) = (head' cs, d) : safeZip' (tail' cs) ds

        head' []    = err
        head' (e:_) = e

        tail' []     = err
        tail' (_:es) = es

        err = usrErr "AFRPUtilities" fn "Input list too short."


parZ :: [SF a b] -> SF [a] [b]
parZ = par (safeZip "parZ")


pSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c -> SF [a] [b])
            -> SF [a] [b]
pSwitchZ = pSwitch (safeZip "pSwitchZ")


dpSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c ->SF [a] [b])
             -> SF [a] [b]
dpSwitchZ = dpSwitch (safeZip "dpSwitchZ")


rpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
rpSwitchZ = rpSwitch (safeZip "rpSwitchZ")


drpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
drpSwitchZ = drpSwitch (safeZip "drpSwitchZ")


------------------------------------------------------------------------------
-- Guards and automata-oriented combinators
------------------------------------------------------------------------------

-- Runs sft only when the predicate p is satisfied, otherwise runs sff.
provided :: (a -> Bool) -> SF a b -> SF a b -> SF a b
provided p sft sff =
    switch (constant undefined &&& snap) $ \a0 ->
    if p a0 then stt else stf
    where
        stt = switch (sft &&& (not . p ^>> edge)) (const stf)
        stf = switch (sff &&& (p ^>> edge)) (const stt)


------------------------------------------------------------------------------
-- Wave-form generation
------------------------------------------------------------------------------

-- Zero-order hold with delay.
-- Identity: dHold a0 = hold a0 >>> iPre a0).
dHold :: a -> SF (Event a) a
dHold a0 = dSwitch (constant a0 &&& identity) dHold'
    where
        dHold' a = dSwitch (constant a &&& notYet) dHold'


dTrackAndHold :: a -> SF (Maybe a) a
dTrackAndHold a_init = trackAndHold a_init >>> iPre a_init


------------------------------------------------------------------------------
-- Accumulators
------------------------------------------------------------------------------

accumHold :: a -> SF (Event (a -> a)) a
accumHold a_init = accum a_init >>> hold a_init


dAccumHold :: a -> SF (Event (a -> a)) a
dAccumHold a_init = accum a_init >>> dHold a_init


accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy f b_init = accumBy f b_init >>> hold b_init


dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
dAccumHoldBy f b_init = accumBy f b_init >>> dHold b_init


count :: Integral b => SF (Event a) (Event b)
count = accumBy (\n _ -> n + 1) 0


------------------------------------------------------------------------------
-- Delays
------------------------------------------------------------------------------

-- Lucid-Synchrone-like initialized delay (read "followed by").
fby :: b -> SF a b -> SF a b
b0 `fby` sf = b0 --> sf >>> pre


------------------------------------------------------------------------------
-- Integrals
------------------------------------------------------------------------------

impulseIntegral :: VectorSpace a k => SF (a, Event a) a
impulseIntegral = (integral *** accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)
