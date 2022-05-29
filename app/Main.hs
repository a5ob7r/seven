{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Seven.Cat
import Seven.Tac

data Options
  = Cat CatOptions
  | Tac TacOptions

optionParser :: Parser Options
optionParser =
  subparser $
    foldr1
      (<>)
      [ command "cat" $ Cat <$> catOpts,
        command "tac" $ Tac <$> tacOpts
      ]

sevenOpts :: ParserInfo Options
sevenOpts = info (optionParser <**> helper) mempty

main :: IO ()
main =
  customExecParser (prefs helpLongEquals) sevenOpts >>= \case
    Cat opt -> cat opt
    Tac opt -> tac opt
