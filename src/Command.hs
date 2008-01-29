{- $Id: Command.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*						 I N V A D E R S						  *
*															  *
*	   Module:		Command							*
*	   Purpose:	The Invader command type.				*
*	   Author:		Henrik Nilsson 					*
*															  *
*		    Copyright (c) Yale University, 2003 					  *
*															  *
******************************************************************************
-}

module Command (
    Command(..)
) where


data Command =
	 CmdQuit				-- Quit Invaders.
    | CmdNewGame			-- Play game.
    | CmdFreeze				-- Freeze game.
    | CmdResume				-- Resume game.
    -- | CmdUp 			   -- Move Up.
    -- | CmdDown			   -- Move Down.
    -- | CmdLeft			   -- Move Left.
    -- | CmdRight			   -- Move Right.
