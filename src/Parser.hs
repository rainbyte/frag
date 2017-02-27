{-# LANGUAGE BangPatterns #-}

{- $Id: Parser.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                                I N V A D E R S                                                  *
*                                                                                                                         *
*          Module:          Parser                                                      *
*          Purpose:         Parsing (mainly lexical analysis) of window event     *
*                       stream.                                                    *
*          Author:              Henrik Nilsson                                  *
*                                                                                                                         *
*                   Copyright (c) Yale University, 2003                                           *
*                                                                                                                         *
******************************************************************************
-}

-- Quick 'n dirty adaptation from old robot simulator. Could probably be
-- done better in the new AFRP framework.
module Parser (
    GameInput,  -- Abstract
    parseWinInput,      -- :: SF WinInput GameInput
    command,            -- :: SF GameInput (Event Command)
    cmdString,  -- :: SF GameInput (Event String)
    ptrPos,             -- :: SF GameInput Position2
    lbp,                -- :: SF GameInput (Event ())
    lbpPos,             -- :: SF GameInput (Event Position2)
    lbDown,             -- :: SF GameInput Bool
    rbp,                -- :: SF GameInput (Event ())
    rbpPos,             -- :: SF GameInput (Event Position2)
    rbDown,             -- :: SF GameInput Bool
    dragStart,  -- :: SF GameInput (Event ())
    dragStop,           -- :: SF GameInput (Event Distance2)
    dragStartPos,       -- :: SF GameInput Position2
    dragVec,            -- :: SF GameInput Distance2
    dragging,            -- :: SF GameInput Bool
    keyStat,
    getT,
    getDt

) where

import Data.Maybe (isJust)
import qualified HGL as HGL (Event(..))
import qualified HGL as HGL (Point(..))
import FRP.Yampa
import FRP.Yampa.Utilities
import FRP.Yampa.Geometry
-- import FRP.Yampa.Miscellany (mapFst)

import PhysicalDimensions
import Command

type WinInput = Event HGL.Event


------------------------------------------------------------------------------
-- Exported entities
------------------------------------------------------------------------------

data GameInput = GameInput {
    giCmdStr :: !String,
    giCmd    :: !(Event Command),
    giPDS    :: !PDState,
    giKS           :: !(Event (Char, Bool)),
    giDt           :: !Time,
    giT    :: !Time
}


parseWinInput :: SF (WinInput,WinInput) GameInput
parseWinInput = (wiToCmd <<< (arr fst))&&&
                         (wiToPDS <<<(arr snd)) &&&
                         (wiToKs <<< (arr fst)) &&& (gDt)
                         >>^ \((cmdStr, cmd), (pds, (ks,(dt,t)))) ->
                           GameInput {giCmdStr = cmdStr,
                                            giCmd    = cmd,
                                            giPDS    = pds,
                                            giKS           = ks,
                                            giDt           = dt,
                                            giT    = t}


-- All event sources below are defined such that they will NOT occur at local
-- time 0 (immediately after a switch). Sometimes explicitly using a "notYet".
-- Sometimes using through careful use of "edge" and relatives. Is this the
-- right approach?

-- A valid command has been read.
command :: SF GameInput (Event Command)
command = giCmd ^>> notYet

gDt :: SF a (Double,Double)
gDt
  = (arr (\_ -> 1) >>>
          (imIntegral 0 >>> arr (\ t -> (t, t))) >>>
         (first ((iPre 0) <<< identity) >>> arr (\ (lt, t) -> (t - lt + 0.005,t))))

-- Continuous parser feed back.
cmdString :: SF GameInput String
cmdString = arr giCmdStr

getT :: SF GameInput Time
getT = (iPre 0) <<< (arr giT)

getDt :: SF GameInput Time
getDt = (iPre 0) <<< (arr giDt)

ptrPos :: SF GameInput Position2
ptrPos = arr (pdsPos . giPDS)


lbp :: SF GameInput (Event ())
lbp = lbpPos >>^ (`tag` ())

keyStat :: SF GameInput (Event (Char, Bool))
keyStat = giKS ^>> notYet

lbpPos :: SF GameInput (Event Position2)
lbpPos = (giPDS >>> pdsLeft) ^>> edgeJust


lbDown :: SF GameInput Bool
lbDown = arr (giPDS >>> pdsLeft >>> isJust)


rbp :: SF GameInput (Event ())
rbp = rbpPos >>^ (`tag` ())


rbpPos :: SF GameInput (Event Position2)
rbpPos = (giPDS >>> pdsRight) ^>> edgeJust


rbDown :: SF GameInput Bool
rbDown = arr (giPDS >>> pdsRight >>> isJust)


dragStart :: SF GameInput (Event ())
dragStart = (giPDS >>> pdsDrag) ^>> edgeBy detectStart (Just undefined)
    where
           detectStart Nothing  (Just _) = Just ()
           detectStart _            _      = Nothing


dragStop :: SF GameInput (Event Distance2)
dragStop = ((giPDS >>> pdsDrag) ^>> edgeBy detectStop Nothing) &&& dragVec
                 >>^ \(e, dv) -> e `tag` dv
    where
           detectStop (Just _) Nothing = Just ()
           detectStop _    _     = Nothing


-- (Last) drag start position.
dragStartPos :: SF GameInput Position2
dragStartPos = arr (giPDS >>> pdsDragStartPos)


-- (Last) drag vector.
dragVec :: SF GameInput Distance2
dragVec = arr (giPDS >>> pdsDragVec)


dragging :: SF GameInput Bool
dragging = arr (giPDS >>> pdsDrag >>> isJust)




------------------------------------------------------------------------------
-- Lexical analysis of character input
------------------------------------------------------------------------------

-- Currently overkill, but being able to enter multi-character commands
-- could possibly be useful.

wiToKs :: SF WinInput (Event (Char,Bool))
wiToKs = arr (mapFilterE getKs)
    where
           getKs (HGL.Char {HGL.char=c, HGL.isDown = ks}) = Just (c,ks)
           getKs _                        = Nothing


wiToCmd :: SF WinInput (String, Event Command)
wiToCmd = arr (mapFilterE selChar)
                >>> (accumBy scanChar (undefined,scanCmds) >>^ fmap fst >>^ splitE)
                >>> hold "" *** arr (mapFilterE id)
    where
           scanChar (_, S cont) c = cont c
           selChar (HGL.Char {HGL.char=c, HGL.isDown = True}) = Just c
           selChar _                        = Nothing


-- This ought to be redone. Kont should probably be called Tranition or
-- somethinig.

-- We define a continuation to be the command recognized thus far (a String
-- and maybe a complete Command), and a scanner to be applied to the rest
-- of the input. (I.e., there's output at every step.)

type Kont = ((String, Maybe Command), Scanner)
type Cont a = a -> Kont

-- Since a scanner is applied to one character at a time (typically, on
-- Char events), we recursively define a scanner to be a character
-- continuation.

newtype Scanner = S (Cont Char)

-- Scan commands

scanCmds :: Scanner
scanCmds = scanCmd cmds
    where
           cmds =
            [ ("q", emitCmd scanCmds CmdQuit), -- Discard inp.?
                 ("p", emitCmd scanCmds CmdNewGame),
                 ("f", emitCmd scanCmds CmdFreeze),
                 ("r", emitCmd scanCmds CmdResume)
            ]


-- Scan one command.
-- Looks for a valid command. Outputs prefix as long as the current
-- prefix is valid. Starts over on first invalid character. Invokes success
-- continuation on success.
-- cmds ....... List of pairs of valid command and corresponding success
--              continuation.

scanCmd :: [(String, Cont String)] -> Scanner
scanCmd cmds = scanSubCmd "" cmds


-- Scan one subcommand/keyword argument.
-- Looks for a valid command. Outputs prefix as long as the current
-- prefix is valid. Starts over on first invalid character. Invokes success
-- continuation on success.
-- pfx0 ....... Initial prefix.
-- cmds ....... List of pairs of valid command and corresponding success
--              continuation.

scanSubCmd :: String -> [(String, Cont String)] -> Scanner
scanSubCmd pfx0 cmds = S (scHlp pfx0 cmds)
    where
           -- pfx ........      Command prefix.
           -- sfxconts ...      Command suffixes paired with success continuations.
           -- c .......... Input character.
           scHlp pfx sfxconts c =
            case c of
                '\r' ->
                    case [ cont | ("", cont) <- sfxconts ] of
                          []       -> emitPfx (S (scHlp pfx sfxconts)) pfx
                          (cont : _) -> cont pfx
                '.'     ->
                    case sfxconts of
                          []             -> emitPfx (S (scHlp pfx0 cmds)) pfx0
                          [(sfx, cont)] -> cont (pfx ++ sfx)
                          _                      ->
                            let
                                 (sfxs, conts) = unzip sfxconts
                                 cpfx           = foldr1 lcp sfxs
                                 sfxs'          = map (drop (length cpfx)) sfxs
                                 pfx'     = pfx ++ cpfx
                                 sfxconts'      = zip sfxs' conts
                            in
                                 emitPfx (S (scHlp pfx' sfxconts')) pfx'
                _       ->
                    let
                           pfx' = pfx ++ [c]
                           sfxconts' = [ (tail sfx, cont) | (sfx, cont) <- sfxconts, not (null sfx) && head sfx == c]
                    in
                           case sfxconts' of
                            []            -> emitPfx (S (scHlp pfx0 cmds))
                                                    pfx0
                                                    -- ("Invalid: " ++ [c])
                            [("", cont)] -> cont pfx'
                            _             -> emitPfx (S (scHlp pfx' sfxconts'))
                                                    pfx'

-- Emit command (and command string), then continue scanning.
emitCmd :: Scanner -> Command -> String -> Kont
emitCmd scanner cmd cmdStr = ((cmdStr, Just cmd), scanner)

-- Emit current prefix, then scan next character.
emitPfx :: Scanner -> String -> Kont
emitPfx scanner pfx = ((pfx, Nothing), scanner)


------------------------------------------------------------------------------
-- Pointing device processing
------------------------------------------------------------------------------

-- State of the pointing device.
-- The points for pdsLeft, pdsRight, and pdsDrag reflect where the button
-- was initially pressed.


data PDState = PDState {
    pdsPos              :: !Position2,    -- Current position.
    pdsDragStartPos :: !Position2,        -- (Last) drag start position.
    pdsDragVec  :: !Distance2,    -- (Latest) drag vector.
    pdsLeft             :: !(Maybe Position2),      -- Left button currently down.
    pdsRight            :: !(Maybe Position2),      -- Right button currently down.
    pdsDrag             :: !(Maybe Position2)       -- Currently dragging.
}


-- Initial state.
initPDS :: PDState
initPDS = PDState {
                 pdsPos           = origin,
                 pdsDragStartPos = origin,
                 pdsDragVec       = zeroVector,
                 pdsLeft                  = Nothing,
                 pdsRight         = Nothing,
                 pdsDrag                  = Nothing
          }


wiToPDS :: SF WinInput PDState
wiToPDS = (accumHoldBy nextPDS initPDS)


-- Compute next pointing device state.
nextPDS :: PDState -> HGL.Event -> PDState
--nextPDS pds (HGL.Key {}) = pds                          -- Currently we ignore keys.
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = True, HGL.isDown = True}) =
    -- Left button pressed.
    pds {pdsPos = p', pdsDragVec = dv, pdsLeft = Just p'}
    where
           p' = gPointToPosition2 p
           dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) (pdsDrag pds)
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = True, HGL.isDown = False}) =
    -- Left button released.
    pds {pdsPos = p', pdsDragVec = dv, pdsLeft = Nothing, pdsDrag = md}
    where
           p' = gPointToPosition2 p
           md = maybe Nothing (const (pdsDrag pds)) (pdsRight pds)
           dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) md
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = False, HGL.isDown = True}) =
    -- Right button pressed.
    pds {pdsPos = p', pdsDragVec = dv, pdsRight = Just p'}
    where
           p' = gPointToPosition2 p
           dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) (pdsDrag pds)
nextPDS pds (HGL.Button {HGL.pt = p, HGL.isLeft = False, HGL.isDown = False}) =
    -- Right button released.
    pds {pdsPos = p', pdsDragVec = dv, pdsRight = Nothing, pdsDrag = md}
    where
           p' = gPointToPosition2 p
           md = maybe Nothing (const (pdsDrag pds)) (pdsLeft pds)
           dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) md
nextPDS pds (HGL.MouseMove {HGL.pt = p}) =
    -- Mouse move.
    pds {pdsPos = p', pdsDragStartPos = dsp, pdsDragVec = dv, pdsDrag = md}
    where
           p' = gPointToPosition2 p
           md = case pdsLeft pds of
                    mlp@(Just _) -> mlp
                    Nothing       -> pdsRight pds
           dsp = maybe (pdsDragStartPos pds) id md
           dv = maybe (pdsDragVec pds) (\dspos -> p' .-. dspos) md
nextPDS pds _ = pds                     -- Ignore unknown events.

gPointToPosition2 :: HGL.Point -> Position2
gPointToPosition2 (HGL.Point (x, y)) = (Point2 (fromIntegral x) (fromIntegral y))
------------------------------------------------------------------------------
-- General utilities
------------------------------------------------------------------------------

-- Longest common prefix.
lcp :: Eq a => [a] -> [a] -> [a]
lcp []   _                              = []
lcp _    []                             = []
lcp (x:xs) (y:ys) | x == y      = x : lcp xs ys
                           | otherwise = []
