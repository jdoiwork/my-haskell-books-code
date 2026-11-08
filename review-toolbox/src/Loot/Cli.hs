-- ANCHOR: all
-- 🖥️ The command line of lootbook.
-- lootbook のコマンドライン
-- ANCHOR: imports
module Loot.Cli
  ( Command (..)
  , Options (..)
  , optionsInfo
  ) where

import Options.Applicative
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🖥️ What the user asked for.
-- 利用者が頼んだこと
data Command
  = Total
  | Top Int
  deriving (Show, Eq)

data Options = Options
  { bookFile :: FilePath
  , request  :: Command
  }
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: parser
-- 🖥️ lootbook [--file FILE] (total | top N)
optionsInfo :: ParserInfo Options
optionsInfo =
  info (optionsParser <**> helper) (progDesc "Look into the loot book")

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption fileSettings
    <*> hsubparser commands
  where
    fileSettings = mconcat
      [ long "file"
      , metavar "FILE"
      , value "loot.json"
      , showDefault
      , help "The loot book (JSON)"
      ]
    commands = mconcat
      [ command "total"
          (info (pure Total) (progDesc "The value of the whole book"))
      , command "top"
          (info (Top <$> argument auto (metavar "N"))
                (progDesc "The N most valuable items"))
      ]
-- ANCHOR_END: parser
-- ANCHOR_END: all
