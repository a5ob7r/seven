{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Seven.Cat (CatOptions, cat, catOpts) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Options.Applicative
import System.Exit
import System.IO
import System.IO.Error

-- | Convert a value of a type into a value of another type.
class From a b where
  from :: a -> b

-- | A line number.
newtype LineNo = LineNo {unLineNo :: Word}
  deriving (Enum)

-- | The line, which is outputed just before.
newtype LastLine = LastLine (Maybe C.ByteString)

-- | Command-line arguments.
newtype Arguments = Arguments {unArgument :: [FilePath]}

instance From CatOptions Arguments where
  from CatOptions {args = []} = Arguments ["-"]
  from CatOptions {args} = Arguments args

-- | A line number style, is used at the head of each line.
data LineNumberStyle = None | NonEmpty | All

instance From CatOptions LineNumberStyle where
  from opt = case (opt.numberNonBlank, opt.number) of
    (True, _) -> NonEmpty
    (_, True) -> All
    _ -> None

-- | A configuration to display invisible characters.
data ListChars = ListChars
  { ends :: Bool,
    tabs :: Bool,
    nonPrinting :: Bool
  }

instance From CatOptions ListChars where
  from opt = ListChars {..}
    where
      ends = opt.showAll || opt.e || opt.showEnds
      tabs = opt.showAll || opt.t || opt.showTabs
      nonPrinting = opt.showAll || opt.e || opt.t || opt.showNonPrinting

-- | Error logs.
newtype Errors = Errors [SomeException]

-- | A readonly cat configuration.
data CatConfig = CatConfig
  { lineNumberStyle :: LineNumberStyle,
    listChars :: ListChars,
    squeezeBlank :: Bool
  }

instance From CatOptions CatConfig where
  from opt = CatConfig {..}
    where
      lineNumberStyle = from opt
      listChars = from opt
      squeezeBlank = opt.squeezeBlank

-- | A global cat state.
newtype CatState = CatState (Arguments, LineNo, LastLine, Errors)

-- | A cat monad.
newtype Cat a = Cat (ReaderT CatConfig (StateT CatState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader CatConfig, MonadState CatState)

-- | Command-line options and arguments of cat.
data CatOptions = CatOptions
  { showAll :: Bool,
    numberNonBlank :: Bool,
    e :: Bool,
    showEnds :: Bool,
    number :: Bool,
    squeezeBlank :: Bool,
    t :: Bool,
    showTabs :: Bool,
    _u :: Bool,
    showNonPrinting :: Bool,
    args :: [String]
  }

-- | An option parser of cat.
optionParser :: Parser CatOptions
optionParser =
  CatOptions
    <$> switch (long "show-all" <> short 'A' <> help "An alias of -vET.")
    <*> switch (long "number-nonblank" <> short 'b' <> help "Number only non-empty output lines, overrides -n.")
    <*> switch (short 'e' <> help "An alias of -vE.")
    <*> switch (long "show-ends" <> short 'E' <> help "Show a dolalr mark($) at the end of each line.")
    <*> switch (long "number" <> short 'n' <> help "Number all output lines.")
    <*> switch (long "squeeze-blank" <> short 's' <> help "Suppress repeated empty output lines into one.")
    <*> switch (short 't' <> help "An alias of -vT.")
    <*> switch (long "show-tabs" <> short 'T' <> help "Show every TAB characters as ^I.")
    <*> switch (short 'u' <> help "(ignored)")
    <*> switch (long "show-nonprinting" <> short 'v' <> help "Show non-printable characters, except LFD and TAB, using ^ and M- format.")
    <*> (many . strArgument) (metavar "FILE..." <> action "file")

-- | An option parser info of cat.
catOpts :: ParserInfo CatOptions
catOpts = info (optionParser <**> helper) mempty

-- | Get a file handle with read mode.
getHandle :: (MonadCatch m, MonadIO m) => FilePath -> m Handle
getHandle "-" = pure stdin
getHandle path = liftIO $ openFile path ReadMode

-- | Run a "Cat" monad.
runCat :: Cat a -> CatConfig -> CatState -> IO (a, CatState)
runCat (Cat a) c = runStateT $ runReaderT a c

-- | Run a cat command.
cat :: CatOptions -> IO ()
cat options = do
  let c = from options
      s = CatState (from options, LineNo 0, LastLine Nothing, Errors [])

  runCat go c s >>= \case
    (_, CatState (_, _, _, Errors [])) -> pure ()
    _ -> exitWith $ ExitFailure 1
  where
    handleError :: SomeException -> Cat ()
    handleError e = do
      CatState (arguments, lineno, lastline, Errors errors) <- get
      put $ CatState (arguments, lineno, lastline, Errors $ e : errors)

      liftIO $ hPrint stderr e

    go :: Cat ()
    go = do
      CatState (args, lineno, lastline, errors) <- get
      case args.unArgument of
        [] -> pure ()
        x : xs -> do
          put $ CatState (Arguments xs, lineno, lastline, errors)

          handle handleError $ bracket (getHandle x) (liftIO . hClose) handler

          go

-- | An operation of cat on each file handle.
--
-- TODO: Buffering outputs.
handler :: Handle -> Cat ()
handler h =
  try (liftIO $ C.hGetLine h) >>= \case
    Left e | isEOFError e -> pure ()
    Left e -> throw e
    Right line -> do
      config <- ask
      CatState (_, lineno, lastline, _) <- get
      state $ \(CatState (a, n, _, e)) -> ((), CatState (a, n, LastLine $ Just line, e))

      case (config.squeezeBlank, lastline) of
        (True, LastLine (Just "")) | C.null line -> pure ()
        _ ->
          liftIO . C.putStrLn =<< case config.lineNumberStyle of
            None -> pure $ render config line
            NonEmpty | C.null line -> pure $ render config line
            _ -> do
              let lineno' = succ lineno
                  prefix = C.pack $ pad 6 (show $ unLineNo lineno')
              state $ \(CatState (a, _, l, e)) -> ((), CatState (a, lineno', l, e))
              pure $ prefix <> "\t" <> render config line

      handler h

-- | Render a line.
render :: CatConfig -> C.ByteString -> C.ByteString
render c = renderEnds' . renderNonPrintings' . renderTabs'
  where
    renderEnds' =
      if c.listChars.ends
        then renderEnds
        else id
    renderTabs' =
      if c.listChars.tabs
        then renderTabs
        else id
    renderNonPrintings' =
      if c.listChars.nonPrinting
        then renderNonPrintings
        else id

-- | Append a dollar at the end of a line.
--
-- >>> renderEnds "foobar"
-- foobar$
renderEnds :: C.ByteString -> C.ByteString
renderEnds line = line <> "$"

-- | Substitute all TAB character with @^I@.
--
-- >>> renderTabs "foo\tbar"
-- foo^Ibar
renderTabs :: C.ByteString -> C.ByteString
renderTabs = C.concatMap $ \case
  '\t' -> "^I"
  c -> C.singleton c

-- | Substitute all nonprintable characters with @^@ and @M-@ notations. This
-- function assumes all characters consist of single byte.
--
-- >>> renderNonPrintings "fooあいうえおbar"
-- fooM-cM-^AM-^BM-cM-^AM-^DM-cM-^AM-^FM-cM-^AM-^HM-cM-^AM-^Jbar
--
-- TODO: Determine the output by calculation, not by this deterministic way.
renderNonPrintings :: C.ByteString -> C.ByteString
renderNonPrintings = B.concatMap $ \case
  0 -> "^@"
  1 -> "^A"
  2 -> "^B"
  3 -> "^C"
  4 -> "^D"
  5 -> "^E"
  6 -> "^F"
  7 -> "^G"
  8 -> "^H"
  11 -> "^K"
  12 -> "^L"
  13 -> "^M"
  14 -> "^N"
  15 -> "^O"
  16 -> "^P"
  17 -> "^Q"
  18 -> "^R"
  19 -> "^S"
  20 -> "^T"
  21 -> "^U"
  22 -> "^V"
  23 -> "^W"
  24 -> "^X"
  25 -> "^Y"
  26 -> "^Z"
  27 -> "^["
  28 -> "^\\"
  29 -> "^]"
  30 -> "^^"
  31 -> "^_"
  127 -> "^?"
  128 -> "M-^@"
  129 -> "M-^A"
  130 -> "M-^B"
  131 -> "M-^C"
  132 -> "M-^D"
  133 -> "M-^E"
  134 -> "M-^F"
  135 -> "M-^G"
  136 -> "M-^H"
  137 -> "M-^I"
  138 -> "M-^J"
  139 -> "M-^K"
  140 -> "M-^L"
  141 -> "M-^M"
  142 -> "M-^N"
  143 -> "M-^O"
  144 -> "M-^P"
  145 -> "M-^Q"
  146 -> "M-^R"
  147 -> "M-^S"
  148 -> "M-^T"
  149 -> "M-^U"
  150 -> "M-^V"
  151 -> "M-^W"
  152 -> "M-^X"
  153 -> "M-^Y"
  154 -> "M-^Z"
  155 -> "M-^["
  156 -> "M-^\\"
  157 -> "M-^]"
  158 -> "M-^^"
  159 -> "M-^_"
  160 -> "M- "
  161 -> "M-!"
  162 -> "M-\""
  163 -> "M-#"
  164 -> "M-$"
  165 -> "M-%"
  166 -> "M-&"
  167 -> "M-'"
  168 -> "M-("
  169 -> "M-)"
  170 -> "M-*"
  171 -> "M-+"
  172 -> "M-,"
  173 -> "M--"
  174 -> "M-."
  175 -> "M-/"
  176 -> "M-0"
  177 -> "M-1"
  178 -> "M-2"
  179 -> "M-3"
  180 -> "M-4"
  181 -> "M-5"
  182 -> "M-6"
  183 -> "M-7"
  184 -> "M-8"
  185 -> "M-9"
  186 -> "M-:"
  187 -> "M-;"
  188 -> "M-<"
  189 -> "M-="
  190 -> "M->"
  191 -> "M-?"
  192 -> "M-@"
  193 -> "M-A"
  194 -> "M-B"
  195 -> "M-C"
  196 -> "M-D"
  197 -> "M-E"
  198 -> "M-F"
  199 -> "M-G"
  200 -> "M-H"
  201 -> "M-I"
  202 -> "M-J"
  203 -> "M-K"
  204 -> "M-L"
  205 -> "M-M"
  206 -> "M-N"
  207 -> "M-O"
  208 -> "M-P"
  209 -> "M-Q"
  210 -> "M-R"
  211 -> "M-S"
  212 -> "M-T"
  213 -> "M-U"
  214 -> "M-V"
  215 -> "M-W"
  216 -> "M-X"
  217 -> "M-Y"
  218 -> "M-Z"
  219 -> "M-["
  220 -> "M-\\"
  221 -> "M-]"
  222 -> "M-^"
  223 -> "M-_"
  224 -> "M-`"
  225 -> "M-a"
  226 -> "M-b"
  227 -> "M-c"
  228 -> "M-d"
  229 -> "M-e"
  230 -> "M-f"
  231 -> "M-g"
  232 -> "M-h"
  233 -> "M-i"
  234 -> "M-j"
  235 -> "M-k"
  236 -> "M-l"
  237 -> "M-m"
  238 -> "M-n"
  239 -> "M-o"
  240 -> "M-p"
  241 -> "M-q"
  242 -> "M-r"
  243 -> "M-s"
  244 -> "M-t"
  245 -> "M-u"
  246 -> "M-v"
  247 -> "M-w"
  248 -> "M-x"
  249 -> "M-y"
  250 -> "M-z"
  251 -> "M-{"
  252 -> "M-|"
  253 -> "M-}"
  254 -> "M-~"
  255 -> "M-^?"
  n -> B.singleton n

-- | Pad the left side of a string with spaces.
pad :: Int -> String -> String
pad = padWith ' '

-- | Pad the left side of a string with single character. This function assumes
-- all characters have the same width.
--
-- >>> padWith '0' 3 "1"
-- 001
padWith :: Char -> Int -> String -> String
padWith c n s = case n - length s of
  l | l > 0 -> replicate l c <> s
  _ -> s
