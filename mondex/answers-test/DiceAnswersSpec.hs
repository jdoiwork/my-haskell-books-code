-- 「CLI ツールを作る」の章のトレーニングの解答（テスト）
module DiceAnswersSpec (diceSpec) where

import Test.Hspec
import Options.Applicative
  ( ParserInfo, defaultPrefs, execParserPure, getParseResult
  , noBacktrack, prefs )

import DiceAnswers

-- ⌨️ Parse the words with a parser.
-- パーサーで単語を解析する
parseWith :: ParserInfo a -> [String] -> Maybe a
parseWith pinfo args = getParseResult (execParserPure defaultPrefs pinfo args)

diceSpec :: Spec
diceSpec = do
  optionSpec
  subcommandSpec

-- 🎲 The training of the chapter "CLI ツールを作る".
-- 「CLI ツールを作る」の章のトレーニング
optionSpec :: Spec
optionSpec = describe "the dice: options (training)" $ do
  -- 「何も付けなければ、6面を1回」
  it "rolls a 6-sided die once by default" $
    parseWith rollInfo [] `shouldBe` Just (Roll 6 1)
  -- 「面の数と回数を読む」
  it "reads the sides and the times" $
    parseWith rollInfo ["--sides", "20", "--times", "3"]
      `shouldBe` Just (Roll 20 3)
-- ANCHOR: sidesTest
  -- 「1面のサイコロは断る」
  it "rejects a 1-sided die" $
    parseWith safeRollInfo ["--sides", "1"] `shouldBe` Nothing
  -- 「2面のサイコロは受け取る」
  it "takes a 2-sided die" $
    parseWith safeRollInfo ["--sides", "2"] `shouldBe` Just (Roll 2 1)
-- ANCHOR_END: sidesTest

-- 🎲 The training of the chapter "サブコマンドのある CLI ツール".
-- 「サブコマンドのある CLI ツール」の章のトレーニング
subcommandSpec :: Spec
subcommandSpec = describe "the dice: subcommands (training)" $ do
  -- 「roll と flip の2つのコマンドがある」
  it "has roll and flip" $ do
    parseWith diceInfo ["roll", "--times", "2"]
      `shouldBe` Just (RollDice (Roll 6 2))
    parseWith diceInfo ["flip"] `shouldBe` Just FlipCoin
-- ANCHOR: globalTest
  -- 「--verbose は、サブコマンドの前に書ける」
  it "takes --verbose before the subcommand" $
    parseWith diceOptionsInfo ["--verbose", "flip"]
      `shouldBe` Just (DiceOptions True FlipCoin)
  -- 「後ろに書いても、全体のオプションとして読む」
  it "also takes --verbose after the subcommand" $
    parseWith diceOptionsInfo ["flip", "--verbose"]
      `shouldBe` Just (DiceOptions True FlipCoin)
  -- 「noBacktrack にすると、後ろの --verbose は受け付けない」
  it "rejects --verbose after the subcommand with noBacktrack" $
    let strict = prefs noBacktrack
        result = execParserPure strict diceOptionsInfo ["flip", "--verbose"]
    in  getParseResult result `shouldBe` Nothing
-- ANCHOR_END: globalTest
