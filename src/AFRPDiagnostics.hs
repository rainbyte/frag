{- $Id: AFRPDiagnostics.hs,v 1.3 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPDiagnostics                                      *
*       Purpose:        Standardized error-reporting for AFRP                *
*       Authors:        Henrik Nilsson and Antony Courtney                   *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPDiagnostics where

usrErr :: String -> String -> String -> a
usrErr mn fn msg = error (mn ++ "." ++ fn ++ ": " ++ msg)

intErr :: String -> String -> String -> a
intErr mn fn msg = error ("[internal error] " ++ mn ++ "." ++ fn ++ ": "
                          ++ msg)
