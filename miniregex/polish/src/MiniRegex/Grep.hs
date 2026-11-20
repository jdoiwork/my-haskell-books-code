-- ANCHOR: all
-- ANCHOR: header
-- | minigrep: a small grep on MiniRegex. The parts here are pure: the
--   options, and what to print for one file. Reading files and the
--   exit status are in app/Grep.hs.
-- minigrep：MiniRegex で作る小さな grep。ここにあるのは純粋な部品：
-- オプションと、1つのファイルで何を書くか。ファイルを読むのと終了
-- ステータスは app/Grep.hs にある
module MiniRegex.Grep
  ( Settings (..)
  , ColorWhen (..)
  , settingsInfo
  , View (..)
  , viewOf
  , Report (..)
  , grepText
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative

import MiniRegex
-- ANCHOR_END: imports

-- ANCHOR: settings
-- 🎨 When to color: --color=auto (only on a terminal), always, never.
-- 色を付けるとき：--color=auto（端末のときだけ）・always・never
data ColorWhen = ColorAuto | ColorAlways | ColorNever
  deriving (Show, Eq)

-- 📋 What was typed on the command line.
-- コマンドラインに打ったもの
data Settings = Settings
  { caseless     :: Bool        -- -i
  , invert       :: Bool        -- -v
  , countOnly    :: Bool        -- -c
  , onlyMatching :: Bool        -- -o
  , lineNumbers  :: Bool        -- -n
  , color        :: ColorWhen   -- --color
  , pattern      :: Text
  , files        :: [FilePath]  -- none, or "-": standard input
  }
  deriving (Show, Eq)
-- ANCHOR_END: settings

-- ANCHOR: parser
-- 🧰 The parser of the command line, one part to a line.
-- コマンドラインのパーサー。1部品1行
settingsParser :: Parser Settings
settingsParser =
  Settings
    <$> switch caselessSettings
    <*> switch invertSettings
    <*> switch countSettings
    <*> switch onlySettings
    <*> switch numberSettings
    <*> option colorReader colorSettings
    <*> strArgument patternSettings
    <*> many (strArgument fileSettings)
  where
    caselessSettings = mconcat
      [ short 'i', long "ignore-case"
      , help "Ignore the case of letters" ]
    invertSettings = mconcat
      [ short 'v', long "invert-match"
      , help "Show the lines that do not match" ]
    countSettings = mconcat
      [ short 'c', long "count"
      , help "Show only how many lines were chosen" ]
    onlySettings = mconcat
      [ short 'o', long "only-matching"
      , help "Show only the matches, one to a line" ]
    numberSettings = mconcat
      [ short 'n', long "line-number"
      , help "Show the number of each line" ]
    colorSettings = mconcat
      [ long "color", metavar "WHEN", value ColorAuto
      , help "Color the matches: auto, always or never" ]
    patternSettings = mconcat
      [ metavar "PATTERN", help "A PCRE2-like pattern" ]
    fileSettings = mconcat
      [ metavar "FILE...", help "Files to read (none, or -: stdin)" ]

-- 🎨 auto, always or never.
-- auto・always・never のどれか
colorReader :: ReadM ColorWhen
colorReader = eitherReader $ \case
  "auto"   -> Right ColorAuto
  "always" -> Right ColorAlways
  "never"  -> Right ColorNever
  other    -> Left ("--color must be auto, always or never, not " <> other)

-- ℹ️ The parser, with --help.
-- パーサーと --help
settingsInfo :: ParserInfo Settings
settingsInfo = info (settingsParser <**> helper) infoSettings
  where
    infoSettings = mconcat
      [ fullDesc
      , progDesc "Print the lines that match PATTERN (MiniRegex)"
      , header "minigrep - a small grep on MiniRegex"
      -- a wrong option is trouble, too: 2, the same as grep
      -- まちがったオプションも困ったこと：grep と同じく 2
      , failureCode 2 ]
-- ANCHOR_END: parser

-- ANCHOR: view
-- 👀 How to print, once the files are known.
-- ファイルがわかってから決まる、書き方
data View = View
  { viewInvert  :: Bool
  , viewCount   :: Bool
  , viewOnly    :: Bool
  , viewNumbers :: Bool
  , viewColor   :: Bool        -- color, after auto is settled
  , viewName    :: Maybe Text  -- the file name, with two files or more
  }

-- 👀 From the settings: is there color, and is a name shown?
-- 設定から：色を付けるか、名前を見せるか
viewOf :: Settings -> Bool -> Maybe Text -> View
viewOf s colored name = View
  { viewInvert  = invert s
  , viewCount   = countOnly s
  , viewOnly    = onlyMatching s
  , viewNumbers = lineNumbers s
  , viewColor   = colored
  , viewName    = name
  }
-- ANCHOR_END: view

-- ANCHOR: report
-- 📄 What one file gives: the lines to print, how many lines were
--    chosen, and the lines where the engine gave up.
-- 1つのファイルの結果：書く行、選んだ行の数、エンジンがあきらめた行
data Report = Report
  { printedLines :: [Text]
  , chosenCount  :: Int
  , gaveUpLines  :: [Int]
  }
  deriving (Show, Eq)
-- ANCHOR_END: report

-- ANCHOR: grep
-- 🔍 Run the regex over each line of the text.
-- 文字列の1行ずつに、正規表現を走らせる
grepText :: View -> Regex -> Text -> Report
grepText view re text
  | viewCount view = Report [prefix Nothing <> T.pack (show n)] n stuck
  | otherwise      = Report (concat printed) n stuck
  where
    results = [ (i, line, findAllWithin defaultLimit re line)
              | (i, line) <- zip [1 :: Int ..] (T.lines text) ]
    stuck   = [ i | (i, _, Left _) <- results ]
    -- the lines that are chosen: matching, or not matching with -v
    -- 選ぶ行：合う行。-v なら、合わない行
    picked  = [ (i, line, ms)
              | (i, line, Right ms) <- results
              , null ms == viewInvert view ]
    n       = length picked
    printed = map printLine picked
    printLine (i, line, ms)
      | viewOnly view && viewInvert view = []
      | viewOnly view =
          [ prefix (Just i) <> paint (matchText m)
          | m <- ms, matchStart m /= matchEnd m ]
      | otherwise = [prefix (Just i) <> highlight ms line]
    -- ANCHOR_END: grep
    -- ANCHOR: paint
    -- file:12: before the line
    -- 行の前の file:12:
    prefix i = mconcat
      [ maybe "" (\f -> colored "35" f <> separator) (viewName view)
      , case i of
          Just k | viewNumbers view ->
            colored "32" (T.pack (show k)) <> separator
          _ -> "" ]
    separator = colored "36" ":"
    highlight ms line
      | viewColor view = rebuild ms line
      | otherwise      = line
    paint = colored "01;31"
    colored code t
      | viewColor view = "\ESC[" <> code <> "m" <> t <> "\ESC[m"
      | otherwise      = t
    -- the line, with each match painted
    -- 一致を1つずつ塗った行
    rebuild ms line = T.concat (go 0 ms)
      where
        go at [] = [T.drop at line]
        go at (m : rest) =
          slice at (matchStart m) : paint (matchText m)
            : go (matchEnd m) rest
        slice from to = T.take (to - from) (T.drop from line)
    -- ANCHOR_END: paint
-- ANCHOR_END: all
