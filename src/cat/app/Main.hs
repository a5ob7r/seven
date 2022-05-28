module Main where

import Options.Applicative
import Seven.Cat

main :: IO ()
main = execParser catOpts >>= cat
