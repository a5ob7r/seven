module Main where

import Options.Applicative
import Seven.Nl
import System.Exit

main :: IO ()
main = customExecParser helpLongEquals nlOpts >>= nl >>= exitWith
