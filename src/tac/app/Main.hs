module Main where

import Options.Applicative
import Seven.Tac
import System.Exit

main :: IO ()
main = customExecParser (prefs helpLongEquals) tacOpts >>= tac >>= exitWith
