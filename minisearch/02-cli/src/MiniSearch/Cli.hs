-- ANCHOR: head
-- ANCHOR: all
-- | The command line: what the user asked for, as types.
-- コマンドライン：ユーザーが頼んだことを、型にする。
module MiniSearch.Cli
  ( Command (..)
  , IndexOptions (..)
  , SearchOptions (..)
  , commandInfo
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
-- ANCHOR_END: head

-- ANCHOR: types
-- 🧭 The two things the command can do.
-- コマンドができる、2つのこと
data Command
  = IndexCmd IndexOptions    -- minisearch index DIR
  | SearchCmd SearchOptions  -- minisearch search WORDS...
  deriving (Eq, Show)

data IndexOptions = IndexOptions
  { docsDir  :: FilePath
  , indexOut :: FilePath
  }
  deriving (Eq, Show)

data SearchOptions = SearchOptions
  { indexIn :: FilePath
  , limit   :: Int
  , query   :: Text
  }
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: parsers
-- 🏗️ minisearch index DIR [--out FILE]
indexOptions :: Parser IndexOptions
indexOptions =
  IndexOptions
    <$> strArgument dirSettings
    <*> strOption outSettings
  where
    dirSettings = mconcat
      [ metavar "DIR"
      , help "The folder of .txt documents"
      ]
    outSettings = mconcat
      [ long "out"
      , short 'o'
      , metavar "FILE"
      , value "index.json"
      , showDefault
      , help "Where to save the index"
      ]

-- 🔍 minisearch search WORDS... [--index FILE] [--limit N]
searchOptions :: Parser SearchOptions
searchOptions =
  SearchOptions
    <$> strOption indexSettings
    <*> option positiveInt limitSettings
    <*> (joinWords <$> some (strArgument wordSettings))
  where
    indexSettings = mconcat
      [ long "index"
      , short 'i'
      , metavar "FILE"
      , value "index.json"
      , showDefault
      , help "The index to search"
      ]
    limitSettings = mconcat
      [ long "limit"
      , short 'n'
      , metavar "N"
      , value 5
      , showDefault
      , help "How many results to show"
      ]
    wordSettings = mconcat
      [ metavar "WORDS..."
      , help "The words to search for (all of them must match)"
      ]
    joinWords ws = T.unwords (map T.pack ws)

-- 🔢 A number that is 1 or more.
-- 1 以上の数
positiveInt :: ReadM Int
positiveInt = eitherReader $ \s ->
  case reads s of
    [(n, "")] | n >= 1 -> Right n
    _                  -> Left ("not a positive number: " <> s)
-- ANCHOR_END: parsers

-- ANCHOR: command
-- 🧩 The whole command: two subcommands.
-- コマンド全体：2つのサブコマンド
commandParser :: Parser Command
commandParser = hsubparser (indexCommand <> searchCommand)
  where
    indexCommand  = command "index"
      (info (IndexCmd <$> indexOptions)
            (progDesc "Build an index from a folder of documents"))
    searchCommand = command "search"
      (info (SearchCmd <$> searchOptions)
            (progDesc "Search the index"))

commandInfo :: ParserInfo Command
commandInfo = info (commandParser <**> helper) infoSettings
  where
    infoSettings = mconcat
      [ fullDesc
      , progDesc "A mini full-text search engine"
      ]
-- ANCHOR_END: command
-- ANCHOR_END: all
