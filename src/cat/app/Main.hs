module Main where

import Options.Applicative
import Seven.Cat
import System.Exit

main :: IO ()
main = execParser catOpts >>= cat >>= exitWith
