module Main where

import Options.Applicative
import Seven.Nl

main :: IO ()
main = customExecParser helpLongEquals nlOpts
