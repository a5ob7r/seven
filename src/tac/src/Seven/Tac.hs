{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Seven.Tac (TacOptions, tacOpts, tac) where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as C
import Data.Functor
import Options.Applicative
import System.Exit
import System.IO
import Text.Regex.TDFA hiding (before)

-- | Convert a value of a type to a value of another one.
class From a b where
  from :: a -> b

-- | A string separator.
data Separator
  = Newline
  | Plain String
  | Regex String

instance From TacOptions Separator where
  from TacOptions {..} =
    case (separator, regex) of
      (Just s, True) -> Regex s
      (Just s, _) -> Plain s
      _ -> Newline

-- | A readonly configuration of cat.
data TacConfig = TacConfig
  { before :: Bool,
    separator :: Separator
  }

instance From TacOptions TacConfig where
  from options = TacConfig {..}
    where
      before = options.before
      separator = from options

-- | Command-line arguments of tac.
newtype Arguments = Arguments [FilePath]

instance From TacOptions Arguments where
  from TacOptions {arguments = []} = Arguments ["-"]
  from TacOptions {arguments} = Arguments arguments

-- | Error logs of tac.
newtype Errors = Errors [SomeException]

-- | A global state of cat.
data TacState = TacState
  { arguments :: Arguments,
    errors :: Errors
  }

instance From TacOptions TacState where
  from options = TacState {..}
    where
      arguments = from options
      errors = Errors []

-- | A tac monad.
newtype Tac a = Tac (ReaderT TacConfig (StateT TacState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader TacConfig, MonadState TacState)

-- | Run a "Tac" monad.
runTac :: Tac a -> TacConfig -> TacState -> IO (a, TacState)
runTac (Tac a) c = runStateT $ runReaderT a c

-- | Command-line options and arguments of tac.
data TacOptions = TacOptions
  { before :: Bool,
    regex :: Bool,
    separator :: Maybe String,
    arguments :: [String]
  }

-- | An option parser of tac.
optionParser :: Parser TacOptions
optionParser =
  TacOptions
    <$> switch (long "before" <> short 'b' <> help "Prepend, but not append, the separator string to each output.")
    <*> switch (long "regex" <> short 'r' <> help "Interpret the separator string as a regular expression instead of plain string.")
    <*> option (str <&> Just) (long "separator" <> short 's' <> value Nothing <> metavar "STRING" <> help "The separator string, a newline character by default.")
    <*> (many . strArgument) (metavar "FILE..." <> action "file")

-- | An option parser info of tac.
tacOpts :: ParserInfo TacOptions
tacOpts = info (optionParser <**> helper) mempty

-- | Run a tac command with options.
tac :: TacOptions -> IO ()
tac options = do
  let c = from options
      s = from options

  runTac go c s >>= \case
    (_, TacState {errors = Errors []}) -> pure ()
    _ -> exitWith $ ExitFailure 1
  where
    handleError :: SomeException -> Tac ()
    handleError e = do
      TacState {arguments, errors = Errors errs} <- get
      put $ TacState {arguments, errors = Errors $ e : errs}

      liftIO $ hPrint stderr e

    go =
      get >>= \case
        TacState {arguments = Arguments []} -> pure ()
        TacState {arguments = Arguments (x : xs), errors} -> do
          put $ TacState {arguments = Arguments xs, errors}

          handle handleError $ bracket (getHandle x) (liftIO . hClose) handler

          go

handler :: Handle -> Tac ()
handler h = do
  TacConfig {..} <- ask

  liftIO $ C.hGetContents h >>= liftIO . C.putStr . C.concat . reverse . (if before then tailApply attacher else attacher) . tokenize separator
  where
    tailApply :: ([a] -> [a]) -> [a] -> [a]
    tailApply _ [] = []
    tailApply _ [x] = [x]
    tailApply f (x : xs) = x : f xs

    attacher :: [C.ByteString] -> [C.ByteString]
    attacher [] = []
    attacher [x] = [x]
    attacher (x1 : x2 : xs) = x1 <> x2 : attacher xs

-- | Split a string into a string list separated by a separator. The string
-- list contain also the separator, and the separator is interspersed into the
-- separated strings.
--
-- >>> tokenize Newline "a\nbc\n\n"
-- ["a","\n","bc","\n","","\n",""]
--
-- >>> tokenize (Plain "a") "a\nbac\nda\n"
-- ["","a","\nb","a","c\nd","a","\n"]
--
-- >>> tokenize (Regex "a.") "a\nbac\ndae\n"
-- ["a\nb","ac","\nd","ae","\n"]
tokenize :: Separator -> C.ByteString -> [C.ByteString]
tokenize Newline s = tokenize (Plain "\n") s
tokenize (Plain sep) s = go (C.pack sep) s
  where
    go x y = case C.breakSubstring x y of
      (h, "") -> [h]
      (h, t) -> h : x : go x (C.drop (C.length x) t)
tokenize sep@(Regex regex) s = case s =~ regex of
  MR {mrMatch = "", ..} -> [mrBefore]
  MR {..} -> mrBefore : mrMatch : tokenize sep mrAfter

-- | Get a file handle with read mode.
getHandle :: (MonadCatch m, MonadIO m) => FilePath -> m Handle
getHandle "-" = pure stdin
getHandle path = liftIO $ openFile path ReadMode
