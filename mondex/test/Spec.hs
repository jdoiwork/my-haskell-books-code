-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- ANCHOR: module
-- 🧪 Tests for mondex: the command line and the pure core.
-- mondex のテスト：コマンドラインと、純粋な核
module Main (main) where

import Test.Hspec

-- Run the parser on a list of words, without running the program.
-- プログラムを動かさずに、単語のリストでパーサーを試す
import Options.Applicative
  ( ParserInfo, ParserResult (..), defaultPrefs, execParserPure
  , getParseResult, info, renderFailure )

-- Golden tests: compare the output with a saved file.
-- ゴールデンテスト：出力を、保存したファイルと比べる
import Test.Hspec.Golden (Golden, defaultGolden)
import Data.Text qualified as T

import Mondex.Command
import Mondex.DoStyle (monlistRequestParserDo)
import Mondex.Monlist (MonlistInput (..), monlistInfo)
import Mondex.Core    (Outcome (..), formatMonster, listMonsters, runAction)
import Mondex.Monster (Element (..), Monster (..))
import DiceAnswersSpec (diceSpec)
-- ANCHOR_END: module

-- ANCHOR: parse
-- ⌨️ Parse the words with a parser, as if the user typed them.
-- 利用者が打ったつもりで、パーサーで単語を解析する
parseWith :: ParserInfo a -> [String] -> Maybe a
parseWith pinfo args = getParseResult (execParserPure defaultPrefs pinfo args)

-- ⌨️ Parse the words that follow "mondex".
-- "mondex" の後に打った単語を解析する
parse :: [String] -> Maybe MondexInput
parse args = parseWith (mondexInfo "monsters.json") args

-- 📁 A MondexInput with the default file.
-- 既定のファイルを使うオプション
inDefaultFile :: MondexAction -> MondexInput
inDefaultFile cmd = MondexInput "monsters.json" cmd
-- ANCHOR_END: parse

-- ANCHOR: slime
-- 👾 A monster for the tests.
-- テスト用のモンスター
slime :: Monster
slime = Monster "Slime" 30 Water ["jelly"]
-- ANCHOR_END: slime

-- ANCHOR: main
main :: IO ()
main = hspec $ do
  monlistSpec
  goldenSpec
  mondexSpec
  coreSpec
  -- answers for the training / トレーニングの解答
  diceSpec
-- ANCHOR_END: main

-- ANCHOR: monlistSpec
-- 🧪 monlist: the options.
-- monlist：オプション
monlistSpec :: Spec
monlistSpec =
  describe "monlist" $ do
    -- 「何も付けなければ、名前の順に全部を一覧にする」
    it "lists all, sorted by name, by default" $
      parseWith monlistInfo [] `shouldBe`
        Just (MonlistInput "monsters.json" (MonlistRequest Nothing ByName False))
    -- 「オプションは、どの順番で書いてもよい」
    it "takes the options in any order" $
      parseWith monlistInfo ["--sort", "hp", "-e", "water"]
        `shouldBe` parseWith monlistInfo ["-e", "water", "--sort", "hp"]
    -- 「知らない属性は断る」
    it "rejects an unknown element" $
      parseWith monlistInfo ["--element", "thunder"] `shouldBe` Nothing
    -- 「do で書いた版も、同じに読む」
    it "reads the same with the do version" $
      let args = ["-e", "water", "--sort", "hp", "--json"]
          viaDo = parseWith (info monlistRequestParserDo mempty) args
          viaAp = parseWith (info monlistRequestParser mempty) args
      in  viaDo `shouldBe` viaAp
-- ANCHOR_END: monlistSpec

-- ANCHOR: goldenSpec
-- 🖼️ monlist: the help and the table, compared with saved files.
-- monlist：ヘルプと一覧の表を、保存したファイルと比べる
goldenSpec :: Spec
goldenSpec =
  describe "monlist output" $ do
    -- 「ヘルプは、保存したものと同じ」
    it "shows the same help" $
      helpText ["--help"]
    -- 「一覧の表は、保存したものと同じ」
    it "shows the same table" $
      tableText (MonlistRequest Nothing ByName False)

-- 📖 What monlist prints for these words when it stops early (--help).
-- monlist がこの単語で途中で止まるとき（--help）に表示するもの
helpText :: [String] -> Golden String
helpText args =
  case execParserPure defaultPrefs monlistInfo args of
    Failure failure ->
      let (message, _exitCode) = renderFailure failure "monlist"
      in  defaultGolden "monlist-help" message
    _ -> error "expected the help"

-- 📋 The table that monlist prints for a small bestiary.
-- 小さな図鑑で、monlist が表示する表
tableText :: MonlistRequest -> Golden String
tableText opts =
  let outcome = listMonsters opts sampleBestiary
  in  defaultGolden "monlist-table" (T.unpack (T.unlines (output outcome)))

-- 👾 A small bestiary for the tests.
-- テスト用の小さな図鑑
sampleBestiary :: [Monster]
sampleBestiary =
  [ Monster "Slime"      30 Water ["jelly"]
  , Monster "Goblin"     50 Grass ["club", "copper coin"]
  , Monster "Salamander" 80 Fire  ["fire scale"]
  , Monster "Kelpie"     65 Water []
  ]
-- ANCHOR_END: goldenSpec

-- ANCHOR: mondexSpec
-- 🧪 mondex: the subcommands.
-- mondex：サブコマンド
mondexSpec :: Spec
mondexSpec =
  describe "mondex" $ do
    -- 「list のオプションは monlist と同じ」
    it "reads the list options like monlist" $
      parse ["list", "--element", "water"] `shouldBe`
        Just (inDefaultFile (List (MonlistRequest (Just Water) ByName False)))
    -- 「--drop を何回でも受け取る」
    it "takes --drop many times" $
      let args = [ "add", "Bat", "--hp", "15", "-e", "grass"
                 , "--drop", "wing", "--drop", "fang" ]
          bat  = Monster "Bat" 15 Grass ["wing", "fang"]
      in  parse args `shouldBe` Just (inDefaultFile (Add bat))
    -- 「--file は、サブコマンドの前にも後ろにも書ける」
    it "takes --file before or after the subcommand" $ do
      parse ["-f", "other.json", "remove", "Slime"]
        `shouldBe` Just (MondexInput "other.json" (Remove "Slime"))
      parse ["remove", "Slime", "-f", "other.json"]
        `shouldBe` Just (MondexInput "other.json" (Remove "Slime"))
    -- 「コマンドがなければ断る」
    it "rejects a missing command" $
      parse [] `shouldBe` Nothing
-- ANCHOR_END: mondexSpec

-- ANCHOR: coreSpec
-- 🧪 The pure core: what each command does to the bestiary.
-- 純粋な核：コマンドごとに、図鑑をどう変えるか
coreSpec :: Spec
coreSpec =
  describe "the pure core" $ do
    -- 「同じ名前のモンスターは、2匹は入れない」
    it "does not add the same monster twice" $
      runAction (Add slime) [slime]
        `shouldBe` Left "Slime is already in the bestiary"
    -- 「いないモンスターは、消せない」
    it "cannot remove a monster that is not there" $
      runAction (Remove "Dragon") [slime]
        `shouldBe` Left "Dragon is not in the bestiary"
    -- 「一覧を出しても、図鑑は変わらない」
    it "does not change the bestiary when listing" $
      let listAll = List (MonlistRequest Nothing ByName False)
      in  runAction listAll [slime]
            `shouldBe` Right (Outcome [formatMonster slime] Nothing)
-- ANCHOR_END: coreSpec
-- ANCHOR_END: all
