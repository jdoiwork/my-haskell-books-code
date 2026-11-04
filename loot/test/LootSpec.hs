-- ANCHOR: all
-- ANCHOR: module
-- 🧪 Tests for splitting the loot.
-- 戦利品を分ける関数のテスト
module LootSpec where

-- hspec for examples, QuickCheck for properties.
-- 例のテストは hspec、性質のテストは QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Loot (splitLoot, splitLootDraft)
-- ANCHOR_END: module

-- ANCHOR: examples
-- ✅ Examples: for this input, this output.
-- 例：この入力なら、この出力
exampleSpec :: Spec
exampleSpec =
  describe "splitLootDraft" $ do
    -- 「100 ゴールドを4人で分ける」
    it "splits 100 gold among 4 members" $
      splitLootDraft 100 4 `shouldBe` [25, 25, 25, 25]
    -- 「90 ゴールドを3人で分ける」
    it "splits 90 gold among 3 members" $
      splitLootDraft 90 3 `shouldBe` [30, 30, 30]
-- ANCHOR_END: examples

-- ANCHOR: remainder
-- ✅ Where does the remainder go? (The draft fails this one.)
-- 余りは、どこへ？（下書きは、このテストに失敗する）
remainderSpec :: Spec
remainderSpec =
  -- 「10 ゴールドを3人で分ける」
  it "splits 10 gold among 3 members" $
    splitLootDraft 10 3 `shouldBe` [4, 3, 3]
-- ANCHOR_END: remainder

-- ANCHOR: propRaw
-- 🎲 A property: whatever the gold and members, the total is kept.
-- 性質：ゴールドとメンバーが何であっても、合計は変わらない
prop_totalRaw :: Int -> Int -> Bool
prop_totalRaw gold members =
  sum (splitLootDraft gold members) == gold
-- ANCHOR_END: propRaw

-- ANCHOR: propDraft
-- 🎲 The same property, only for gold of 0 or more
--    and members of 1 or more.
-- 同じ性質。ただし、ゴールドは 0 以上、メンバーは 1 人以上だけ
prop_totalDraft :: NonNegative Int -> Positive Int -> Bool
prop_totalDraft (NonNegative gold) (Positive members) =
  sum (splitLootDraft gold members) == gold
-- ANCHOR_END: propDraft

-- ANCHOR: spec
-- 🧪 The tests for the fixed splitLoot: examples and properties.
-- 直した splitLoot のテスト：例と性質
spec :: Spec
spec =
  describe "splitLoot" $ do
    -- 「ゴールドがちょうど割り切れるとき」
    context "when the gold divides evenly" $
      -- 「全員に同じ分け前を配る」
      it "gives everyone the same share" $
        splitLoot 100 4 `shouldBe` [25, 25, 25, 25]
    -- 「余りがあるとき」
    context "when there is a remainder" $
      -- 「余りは前のメンバーから配る」
      it "gives it to the first members" $
        splitLoot 10 3 `shouldBe` [4, 3, 3]
    -- 「ゴールドがないとき」
    context "when there is no gold" $
      -- 「だれも何ももらえない」
      it "gives everyone nothing" $
        splitLoot 0 3 `shouldBe` [0, 0, 0]
    -- 「メンバーが1人だけのとき」
    context "with only one member" $
      -- 「ゴールドを全部その人に配る」
      it "gives all the gold to that member" $
        splitLoot 7 1 `shouldBe` [7]
    fairSplit splitLoot
-- ANCHOR_END: spec

-- ANCHOR: fairSplit
-- 🤝 What every way of splitting the loot must do.
--    A test that takes the function to test, so we can reuse it.
-- 戦利品の分け方なら、どれも満たすべきこと
-- テストする関数を受け取るテストなので、使い回せる
-- ANCHOR: fairSplitType
fairSplit :: (Int -> Int -> [Int]) -> Spec
-- ANCHOR_END: fairSplitType
fairSplit split =
  -- 「ゴールドがいくらでも、メンバーが何人でも」
  context "for any gold and any number of members" $ do
    -- 「合計は変わらない」
    prop "keeps the total" $
      \(NonNegative gold) (Positive members) ->
        sum (split gold members) `shouldBe` gold
    -- 「全員が分け前をもらえる」
    prop "gives a share to every member" $
      \(NonNegative gold) (Positive members) ->
        length (split gold members) `shouldBe` members
    -- 「公平である：分け前の差は1以下」
    prop "is fair: shares differ by 1 at most" $
      \(NonNegative gold) (Positive members) ->
        let shares = split gold members
        in  maximum shares - minimum shares `shouldSatisfy` (<= 1)
-- ANCHOR_END: fairSplit
-- ANCHOR_END: all
