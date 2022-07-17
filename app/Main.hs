{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Seven.Cat
import Seven.Nl
import Seven.Tac
import System.Exit

data Options
  = Cat CatOptions
  | Nl NlOptions
  | Tac TacOptions

optionParser :: Parser Options
optionParser =
  subparser $
    foldr1
      (<>)
      [ command "cat" $ Cat <$> catOpts,
        command "nl" $ Nl <$> nlOpts,
        command "tac" $ Tac <$> tacOpts
      ]

sevenOpts :: ParserInfo Options
sevenOpts = info (optionParser <**> helper) mempty

main :: IO ()
main =
  customExecParser (prefs helpLongEquals) sevenOpts >>= \case
    Cat opt -> cat opt >>= exitWith
    Nl opt -> nl opt >>= exitWith
    Tac opt -> tac opt >>= exitWith
