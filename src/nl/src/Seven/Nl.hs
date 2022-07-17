{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Seven.Nl (NlOptions, nl, nlOpts) where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as C
import Options.Applicative hiding (style)
import System.Exit
import System.IO
import System.IO.Error
import Text.Regex.TDFA

-- | Convert a value of a type into another one of a type.
class From a b where
  from :: a -> b

-- | Errors of nl.
newtype Errors = Errors [SomeException]

-- | Command-line arguments.
newtype Arguments = Arguments [FilePath]

instance From NlOptions Arguments where
  from NlOptions {arguments = []} = Arguments ["-"]
  from NlOptions {arguments} = Arguments arguments

-- | A line number.
newtype LineNo = LineNo {unLineNo :: Int}
  deriving (Num)

-- | Sections of logical pages.
data Section
  = -- | Headers are indicated by a line is consist of triple-joined delimiters.
    Header
  | -- | Bodies are indicated by a line is consist of double-joined delimiters.
    Body
  | -- | Footers are indicated by a line is consist of single delimiter.
    Footer

-- | Line number style.
data Style = All | NonEmpty | None | Regex C.ByteString

-- | An alignment and a format of line numbers.
data Format = LeftNoZero | RightNoZero | RightZero

-- | Configurations for line number increment or reset.
data LineNumberConf = LineNumberConf
  { start :: LineNo,
    increment :: LineNo,
    renumber :: Bool,
    delimiter :: C.ByteString,
    header :: Style,
    body :: Style,
    footer :: Style
  }

instance From NlOptions LineNumberConf where
  from NlOptions {..} =
    LineNumberConf
      { start = LineNo startingLineNumber,
        increment = LineNo lineIncrement,
        renumber = not noRenumber,
        delimiter,
        header = headerNumbering,
        body = bodyNumbering,
        footer = footerNumbering
      }
    where
      delimiter = C.pack $ case length sectionDelimiter of
        1 -> sectionDelimiter <> ":"
        _ -> sectionDelimiter

-- | Configurations for line rendering.
data LineRenderConf = LineRenderConf
  { format :: Format,
    separater :: C.ByteString,
    width :: Word
  }

instance From NlOptions LineRenderConf where
  from NlOptions {..} =
    LineRenderConf
      { format = numberFormat,
        separater = C.pack numberSeparator,
        width = numberWidth
      }

-- | Readonly configurations of nl.
data NlConfig = NlConfig
  { lnConf :: LineNumberConf,
    lrConf :: LineRenderConf,
    -- | Max number of consecutive empty lines which are counted as the same line number.
    groupEmptyLines :: Word
  }

instance From NlOptions NlConfig where
  from opt =
    NlConfig
      { lnConf = from opt,
        lrConf = from opt,
        groupEmptyLines = opt.joinBlankLines
      }

-- | Global states of nl.
data NlState = NlState
  { lineno :: LineNo,
    -- | The current section of logical pages.
    section :: Section,
    -- | The last consecutive empty line number.
    lastEmptyLineCount :: Word,
    arguments :: Arguments,
    errors :: Errors
  }

instance From NlOptions NlState where
  from opt@NlOptions {..} =
    NlState
      { lineno = LineNo startingLineNumber,
        section = Body,
        lastEmptyLineCount = 0,
        arguments = from opt,
        errors = Errors []
      }

-- | Command-line options and arguments.
data NlOptions = NlOptions
  { bodyNumbering :: Style,
    sectionDelimiter :: String,
    footerNumbering :: Style,
    headerNumbering :: Style,
    lineIncrement :: Int,
    joinBlankLines :: Word,
    numberFormat :: Format,
    noRenumber :: Bool,
    numberSeparator :: String,
    startingLineNumber :: Int,
    numberWidth :: Word,
    arguments :: [String]
  }

optionParser :: Parser NlOptions
optionParser =
  NlOptions
    <$> option styleParser (long "body-numbering" <> short 'b' <> value NonEmpty <> metavar "STYLE" <> help "Use STYLE to number body lines." <> styleCompleter)
    <*> option str (long "section-delimiter" <> short 'd' <> value "\\:" <> metavar "CC" <> help "Use CC to delimit logical pages.")
    <*> option styleParser (long "footer-numbering" <> short 'f' <> value None <> metavar "STYLE" <> help "Use STYLE to number footer lines." <> styleCompleter)
    <*> option styleParser (long "header-numbering" <> short 'h' <> value None <> metavar "STYLE" <> help "Use STYLE to number header lines." <> styleCompleter)
    <*> option auto (long "line-increment" <> short 'i' <> value 1 <> metavar "NUMBER" <> help "Increment of line number at each line.")
    <*> option natParser (long "join-blank-lines" <> short 'l' <> value 1 <> metavar "NUMBER" <> help "Treat NUMBER consecutive empty lines as a single one.")
    <*> option formatParser (long "number-format" <> short 'n' <> value RightNoZero <> metavar "FORMAT" <> help "Specify a line number format and alignment." <> formatCompleter)
    <*> switch (long "no-renumber" <> short 'p' <> help "No resets line numbers even if a new section starts.")
    <*> strOption (long "number-separator" <> short 's' <> value "\t" <> metavar "STRING" <> help "Insert STRING between a line number and a string.")
    <*> option auto (long "starting-line-number" <> short 'v' <> value 1 <> metavar "NUMBER" <> help "Speficy NUMBER as an initial line number.")
    <*> option natParser (long "number-width" <> short 'w' <> value 6 <> metavar "NUMBER" <> help "Specify column numbers of line numbers.")
    <*> (many . strArgument) (metavar "FILE..." <> action "file")
  where
    natParser =
      (auto :: ReadM Int) >>= \case
        n | n > 0 -> pure $ fromIntegral n
        _ -> readerError "The value must be a natural number."

    styleParser =
      str >>= \case
        "a" -> pure All
        "t" -> pure NonEmpty
        "n" -> pure None
        'p' : regex -> pure . Regex $ C.pack regex
        _ -> readerAbort UnknownError

    styleCompleter = completeWith ["a", "t", "n", "p"]

    formatParser =
      (str :: ReadM String) >>= \case
        "ln" -> pure LeftNoZero
        "rn" -> pure RightNoZero
        "rz" -> pure RightZero
        _ -> readerAbort UnknownError

    formatCompleter = completeWith ["ln", "rn", "rz"]

nlOpts :: ParserInfo NlOptions
nlOpts = info (optionParser <**> helper) mempty

-- | A monad for nl.
newtype Nl a = Nl (ReaderT NlConfig (StateT NlState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader NlConfig, MonadState NlState)

runNl :: NlConfig -> NlState -> Nl () -> IO ((), NlState)
runNl c s (Nl m) = runStateT (runReaderT m c) s

nl :: NlOptions -> IO ExitCode
nl opt = do
  runNl (from opt) (from opt) go >>= \case
    ((), NlState {errors = Errors []}) -> return ExitSuccess
    _ -> return $ ExitFailure 1
  where
    -- Render each line.
    render :: LineRenderConf -> Maybe LineNo -> C.ByteString -> C.ByteString
    render LineRenderConf {..} Nothing "" = C.replicate (fromIntegral width + 1) ' '
    render LineRenderConf {..} Nothing s = C.replicate (fromIntegral width + 1) ' ' <> s
    render LineRenderConf {format = LeftNoZero, ..} (Just lineno) s = padLWith ' ' (fromIntegral width) (C.pack $ show lineno.unLineNo) <> separater <> s
    render LineRenderConf {format = RightNoZero, ..} (Just lineno) s = padRWith ' ' (fromIntegral width) (C.pack $ show lineno.unLineNo) <> separater <> s
    render LineRenderConf {format = RightZero, ..} (Just lineno) s = padRWith '0' (fromIntegral width) (C.pack $ show lineno.unLineNo) <> separater <> s

    errorHandler :: SomeException -> Nl ()
    errorHandler e = do
      s@NlState {errors = Errors errs} <- get
      put $ s {errors = Errors $ e : errs}
      liftIO $ hPrint stderr e

    handler :: Handle -> Nl ()
    handler h = do
      tryIO (liftIO $ C.hGetLine h) >>= \case
        Left e | isEOFError e -> pure ()
        Left e -> throw e
        Right s -> do
          c <- ask
          st <- get

          let style = case st.section of
                Header -> c.lnConf.header
                Body -> c.lnConf.body
                Footer -> c.lnConf.footer

          liftIO . C.hPutStrLn stdout =<< case (st, c) of
            (NlState {..}, NlConfig {lnConf = LineNumberConf {increment}, ..}) | s == mempty ->
              case style of
                NonEmpty -> pure $ render lrConf Nothing s
                None -> pure $ render lrConf Nothing s
                Regex regex | regex /= mempty -> pure $ render lrConf Nothing s
                _
                  | lastEmptyLineCount + 1 >= groupEmptyLines -> do
                      put $ NlState {lineno = lineno + increment, lastEmptyLineCount = 0, ..}

                      pure $ render lrConf (Just lineno) s
                  | otherwise -> do
                      put $ NlState {lastEmptyLineCount = lastEmptyLineCount + 1, ..}

                      pure $ render lrConf Nothing s
            (NlState {..}, NlConfig {lnConf = LineNumberConf {start, renumber, delimiter}})
              | s == delimiter <> delimiter <> delimiter -> do
                  put $ NlState {lineno = lineno', section = Header, lastEmptyLineCount = 0, ..}

                  pure mempty
              | s == delimiter <> delimiter -> do
                  put $ NlState {lineno = lineno', section = Body, lastEmptyLineCount = 0, ..}

                  pure mempty
              | s == delimiter -> do
                  put $ NlState {lineno = lineno', section = Footer, lastEmptyLineCount = 0, ..}

                  pure mempty
              where
                lineno' =
                  if renumber
                    then start
                    else lineno
            (NlState {..}, NlConfig {lnConf = LineNumberConf {increment}, ..}) ->
              case style of
                None -> pure $ render lrConf Nothing s
                Regex regex | not $ regex =~ s -> pure $ render lrConf Nothing s
                _ -> do
                  put $ NlState {lineno = lineno + increment, ..}

                  pure $ render lrConf (Just lineno) s

          handler h

    go :: Nl ()
    go = do
      get >>= \case
        NlState {arguments = Arguments []} -> pure ()
        NlState {arguments = Arguments (x : xs), ..} -> do
          put $ NlState {lineno, section, lastEmptyLineCount, arguments = Arguments xs, errors}
          handle errorHandler $ bracket (getHandle x) (liftIO . hClose) handler
          go

-- | Get a readonly handle of a file.
getHandle :: (MonadCatch m, MonadIO m) => FilePath -> m Handle
getHandle "-" = pure stdin
getHandle path = liftIO $ openFile path ReadMode

-- | Pad a string with a character until the padded string's length is a
-- length. This assumes that all of characters has the same width, so don't
-- consider multi-byte characters.
--
-- >>> padRWith '.' 5 "abc"
-- ..abc
padRWith :: Char -> Int -> C.ByteString -> C.ByteString
padRWith c n s = case n - C.length s of
  l | l > 0 -> C.replicate l c <> s
  _ -> s

padLWith :: Char -> Int -> C.ByteString -> C.ByteString
padLWith c n s = case n - C.length s of
  l | l > 0 -> s <> C.replicate l c
  _ -> s
