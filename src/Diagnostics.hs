{- $Id: Diagnostics.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:         Diagnostics					     *
*       Purpose:        Standardized error-reporting for Invaders	     *
*	Authors:	Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module Diagnostics where

usrErr :: String -> String -> String -> a
usrErr mn fn msg = error (mn ++ "." ++ fn ++ ": " ++ msg)

intErr :: String -> String -> String -> a
intErr mn fn msg = error ("[internal error] " ++ mn ++ "." ++ fn ++ ": "
                          ++ msg)
