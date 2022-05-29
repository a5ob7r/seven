module Main where

import Options.Applicative
import Seven.Tac

main :: IO ()
main = customExecParser (prefs helpLongEquals) tacOpts >>= tac
