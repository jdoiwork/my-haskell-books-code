-- ANCHOR: all
-- ANCHOR: module
-- 📋 monlist: the first, smaller command. It only lists the monsters,
--    so it has options but no subcommands.
-- monlist：最初の、小さなコマンド。一覧を出すだけなので、
-- オプションはあるが、サブコマンドはない
module Mondex.Monlist
  ( MonlistInput (..)
  , monlistInfo
  ) where

-- The command-line parser library.
-- コマンドライン引数を解析するライブラリ
import Options.Applicative

import Mondex.Command (MonlistRequest, fileOption, monlistRequestParser)
-- ANCHOR_END: module

-- ANCHOR: monlistInput
-- ANCHOR: monlistInputType
-- ⌨️ Everything the user typed to monlist, after parsing.
-- 利用者が monlist に打ったもの全部（解析したあと）
data MonlistInput = MonlistInput
  { bestiaryFile :: FilePath
  , request      :: MonlistRequest
  }
  deriving (Show, Eq)
-- ANCHOR_END: monlistInputType

-- ANCHOR: monlistInputParser
-- ⌨️ The file option and the list options, given to MonlistInput in order.
-- ファイルのオプションと一覧のオプションを、順番に MonlistInput に渡す
monlistInputParser :: Parser MonlistInput
monlistInputParser =
  MonlistInput
    <$> fileOption "monsters.json"
    <*> monlistRequestParser
-- ANCHOR_END: monlistInputParser
-- ANCHOR_END: monlistInput

-- ANCHOR: monlistInfo
-- 📖 The parser, with --help, --version and the description.
-- パーサーに、--help と --version と説明を付けたもの
monlistInfo :: ParserInfo MonlistInput
monlistInfo = info parser description
  where
    parser = monlistInputParser <**> helper <**> simpleVersioner "0.1.0"
    description = mconcat
      [ fullDesc
      , header "monlist - list the monsters in your bestiary"
      , progDesc "Show the monsters in a JSON bestiary file"
      ]
-- ANCHOR_END: monlistInfo
-- ANCHOR_END: all
