{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Seven.Cat

newtype Options = Cat CatOptions

optionParser :: Parser Options
optionParser =
  subparser
    (command "cat" $ Cat <$> catOpts)

sevenOpts :: ParserInfo Options
sevenOpts = info (optionParser <**> helper) mempty

main :: IO ()
main =
  execParser sevenOpts >>= \case
    Cat opt -> cat opt
