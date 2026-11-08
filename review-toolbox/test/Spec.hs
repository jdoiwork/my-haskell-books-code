-- ANCHOR: all
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- 🧪 Tests for the loot book: examples, properties, and the command line.
-- 戦利品の目録のテスト：例、性質、コマンドライン
module Main (main) where

import Data.Maybe          (fromJust)
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Loot.Book
import Loot.Cli
import Loot.Gold

-- ANCHOR: arbitrary
-- 🎲 Random items: gold through mkGold, counts not negative.
-- でたらめな品物：ゴールドは mkGold を通し、数はマイナスにしない
instance Arbitrary Item where
  arbitrary = do
    NonNegative g <- arbitrary
    NonNegative c <- arbitrary
    n             <- elements ["potion", "herb", "elixir"]
    pure (Item n (fromJust (mkGold g)) c)
-- ANCHOR_END: arbitrary

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  describe "parseBook" $ do
    -- 「マイナスの数は、名前つきで断る」
    it "turns away a negative count" $
      parseBook "[{\"name\": \"ring\", \"price\": 9, \"count\": -1}]"
        `shouldBe` Left (NegativeCount "ring")
    -- 「マイナスのゴールドは、JSON の門で断る」
    it "turns away negative gold" $
      parseBook "[{\"name\": \"ring\", \"price\": -9, \"count\": 1}]"
        `shouldSatisfy` either isBadJson (const False)

  describe "totalValue" $ do
    -- 「例：3つの品物の値打ちを足す」
    it "adds up the value of a small book" $
      goldAmount (totalValue sampleBook) `shouldBe` 140
    -- 「2つの目録を合わせた値打ちは、値打ちの和」
    prop "of two books is the sum of their values" $
      \xs ys -> totalValue (xs ++ ys) == totalValue xs <> totalValue ys

  describe "topItems" $ do
    -- 「例：値打ちの高い順に 2 つ」
    it "picks the most valuable items first" $
      map name (topItems 2 sampleBook) `shouldBe` ["elixir", "potion"]
    -- 「n 種類より多くは返さない」
    prop "returns at most n items" $
      \(NonNegative n) xs -> length (topItems n xs) <= n

  describe "the command line" $ do
    -- 「top 2 を読める」
    it "reads top 2" $
      parse ["top", "2"] `shouldBe` Just (Options "loot.json" (Top 2))
    -- 「--file は、サブコマンドの前に書ける」
    it "reads --file before the subcommand" $
      parse ["--file", "x.json", "total"]
        `shouldBe` Just (Options "x.json" Total)
  -- ANCHOR_END: spec
  where
    isBadJson (BadJson _) = True
    isBadJson _           = False
    -- ANCHOR: sample
    -- potion: 10 G x 3 = 30, elixir: 100 G x 1 = 100, herb: 2 G x 5 = 10
    sampleBook =
      [ Item "potion" (gold 10)  3
      , Item "elixir" (gold 100) 1
      , Item "herb"   (gold 2)   5
      ]
    gold = fromJust . mkGold
    -- ANCHOR_END: sample
    parse args = getParseResult (execParserPure defaultPrefs optionsInfo args)
-- ANCHOR_END: all
