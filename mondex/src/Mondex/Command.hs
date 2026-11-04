-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- ANCHOR: module
-- ⌨️ What the user can type: the options and commands of mondex.
-- 利用者が打てるもの：mondex のオプションとコマンド
module Mondex.Command
  ( MondexInput (..)
  , MondexAction (..)
  , MonlistRequest (..)
  , MonsterOrder (..)
  , elementOption
  , sortReader
  , fileOption
  , monlistRequestParser
  , mondexInfo
  ) where

-- The command-line parser library.
-- コマンドライン引数を解析するライブラリ
import Options.Applicative

-- Help text in several lines.
-- 何行にもなるヘルプの文
import Options.Applicative.Help.Pretty (Doc, vsep)

-- Lists and text.
-- リストとテキスト
import Data.List (intercalate)
import Data.Text (Text)

import Mondex.Monster (Element, Monster (..), allElements, parseElement)
-- ANCHOR_END: module

-- ANCHOR: listTypes
-- 📋 How to list the monsters.
-- 一覧の出し方
data MonlistRequest = MonlistRequest
  { onlyElement :: Maybe Element
  , order       :: MonsterOrder
  , asJson      :: Bool
  }
  deriving (Show, Eq)

-- 🔢 What to sort by.
-- 何で並べるか
data MonsterOrder = ByName | ByHp
  deriving (Show, Eq)
-- ANCHOR_END: listTypes

-- ANCHOR: commandTypes
-- ⌨️ Everything the user typed to mondex, after parsing.
-- 利用者が mondex に打ったもの全部（解析したあと）
data MondexInput = MondexInput
  { dexFile   :: FilePath
  , dexAction :: MondexAction
  }
  deriving (Show, Eq)

-- 🧭 The commands: list, add, or remove.
-- コマンド：一覧、追加、削除
data MondexAction
  = List MonlistRequest
  | Add Monster
  | Remove Text
  deriving (Show, Eq)
-- ANCHOR_END: commandTypes

-- ANCHOR: readers
-- 🔥 Read an element. A wrong word gets a helpful message.
-- 属性を読む。まちがった言葉には、親切なメッセージを返す
elementReader :: ReadM Element
elementReader = eitherReader $ \word ->
  case parseElement word of
    Just e  -> Right e
    -- 「知らない属性：Thunder（Fire、Water、Grass から選んでね）」
    Nothing -> Left ("unknown element: " ++ word
                     ++ " (choose from " ++ elementNames ++ ")")

-- 📋 Every element, for messages and the help: "Fire, Water, Grass"
-- メッセージとヘルプに使う、すべての属性："Fire, Water, Grass"
elementNames :: String
elementNames = intercalate ", " (map show allElements)

-- 🔢 Read what to sort by: "name" or "hp".
-- 何で並べるかを読む："name" か "hp"
sortReader :: ReadM MonsterOrder
sortReader = eitherReader $ \word ->
  case word of
    "name" -> Right ByName
    "hp"   -> Right ByHp
    -- 「並べ方は name か hp だよ」
    _      -> Left ("sort by name or hp, not " ++ word)
-- ANCHOR_END: readers

-- ANCHOR: elementOption
-- 🔥 The --element option. The help text is given from outside,
--    so that list and add can share this part.
--    The help also shows every element.
-- --element オプション。説明の文を外から受け取るので、
-- list と add で、この部品を使い回せる
-- ヘルプには、すべての属性も表示する
elementOption :: String -> Parser Element
elementOption helpText = option elementReader settings
  where
    settings = mconcat
      [ long "element"
      , short 'e'
      , metavar "ELEMENT"
      , completeWith (map show allElements)
      , help (helpText ++ ": " ++ elementNames)
      ]
-- ANCHOR_END: elementOption

-- ANCHOR: monlistRequestParser
-- 📋 [--element E] [--sort name|hp] [--json]
--    The three parts, given to MonlistRequest in order.
-- 3つの部品を、順番に MonlistRequest に渡す
monlistRequestParser :: Parser MonlistRequest
monlistRequestParser =
  MonlistRequest
    <$> optional (elementOption "Show only this element")
    <*> option sortReader sortSettings
    <*> switch jsonSettings
  where
    sortSettings = mconcat
      [ long "sort"
      , metavar "KEY"
      , value ByName
      , help "Sort by KEY: name or hp (default: name)"
      ]
    jsonSettings = mconcat
      [ long "json"
      , help "Print the monsters as JSON"
      ]
-- ANCHOR_END: monlistRequestParser

-- ANCHOR: fileOption
-- 📁 [--file FILE]. The default file is given from outside.
-- [--file FILE]。既定のファイルは、外から受け取る
fileOption :: FilePath -> Parser FilePath
fileOption defaultFile = strOption settings
  where
    settings = mconcat
      [ long "file"
      , short 'f'
      , metavar "FILE"
      , value defaultFile
      , showDefault
      , help "The bestiary file"
      ]
-- ANCHOR_END: fileOption

-- ANCHOR: addParser
-- ➕ mondex add NAME --hp HP --element E [--drop ITEM]...
addParser :: Parser MondexAction
addParser = Add <$> monsterParser

-- 👾 The four parts, given to Monster in order.
-- 4つの部品を、順番に Monster に渡す
monsterParser :: Parser Monster
monsterParser =
  Monster
    <$> strArgument nameSettings
    <*> option auto hpSettings
    <*> elementOption "Its element"
    <*> many (strOption dropSettings)
  where
    nameSettings = mconcat
      [ metavar "NAME"
      , help "The name of the monster"
      ]
    hpSettings = mconcat
      [ long "hp"
      , metavar "HP"
      , help "Its hit points"
      ]
    dropSettings = mconcat
      [ long "drop"
      , metavar "ITEM"
      , helpDoc (Just dropHelp)
      ]
-- ANCHOR_END: addParser

-- ANCHOR: dropHelp
-- 🎒 The help of --drop, in two lines.
-- --drop の説明。2行に分ける
dropHelp :: Doc
dropHelp = vsep
  -- 「落とす道具」
  [ "An item it drops."
  -- 「何回でも書ける：--drop wing --drop fang」
  , "Give this many times: --drop wing --drop fang"
  ]
-- ANCHOR_END: dropHelp

-- ANCHOR: removeParser
-- ➖ mondex remove NAME
removeParser :: Parser MondexAction
removeParser = Remove <$> strArgument nameSettings
  where
    nameSettings = mconcat
      [ metavar "NAME"
      , help "The name of the monster to remove"
      ]
-- ANCHOR_END: removeParser

-- ANCHOR: mondexParser
-- ⌨️ The whole command line: a file option, then one command.
-- コマンドライン全体：ファイルのオプションと、コマンドを1つ
mondexParser :: FilePath -> Parser MondexInput
mondexParser defaultFile =
  MondexInput
    <$> fileOption defaultFile
    <*> hsubparser commands
  where
    commands = mconcat
      [ command "list"
          (info (List <$> monlistRequestParser) (progDesc "List the monsters"))
      , command "add"
          (info addParser (progDesc "Add a monster"))
      , command "remove"
          (info removeParser (progDesc "Remove a monster"))
      ]
-- ANCHOR_END: mondexParser

-- ANCHOR: mondexInfo
-- 📖 The parser, with --help, --version and the description.
-- パーサーに、--help と --version と説明を付けたもの
mondexInfo :: FilePath -> ParserInfo MondexInput
mondexInfo defaultFile = info parser description
  where
    parser =
      mondexParser defaultFile <**> helper <**> simpleVersioner "0.1.0"
    description = mconcat
      [ fullDesc
      , header "mondex - your monster index"
      , progDesc "Keep a bestiary of monsters in a JSON file"
      , footerDoc (Just mondexExamples)
      ]
-- ANCHOR_END: mondexInfo

-- ANCHOR: mondexExamples
-- 📚 Examples, shown at the end of the help.
-- ヘルプの最後に表示する、使い方の例
mondexExamples :: Doc
mondexExamples = vsep
  [ "Examples:"
  , "  mondex list --element water --sort hp"
  , "  mondex add Bat --hp 25 --element fire --drop wing"
  , "  mondex remove Bat"
  , "  mondex --file other.json list"
  ]
-- ANCHOR_END: mondexExamples
-- ANCHOR_END: all
