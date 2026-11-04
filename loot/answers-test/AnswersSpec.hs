-- 「テスト」の章のトレーニングの解答（テスト）
module AnswersSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.Map.Strict as Map

import Bag           (addItem, emptyBag, removeItem)
import Party         (Member (..), rankByLevel)
import PartySpec     ()
import Streak        (bestStreak)
import StreakAnswers (bestStreakNaive)

spec :: Spec
spec = do
-- ANCHOR: step1
  describe "removeItem (examples)" $ do
    -- 「足りないときは Nothing になる」
    it "is Nothing when there are not enough" $
      removeItem "potion" 3 (addItem "potion" 2 emptyBag)
        `shouldBe` Nothing
    -- 「残りは袋に残る」
    it "leaves the rest in the bag" $
      removeItem "potion" 1 (addItem "potion" 3 emptyBag)
        `shouldBe` Just (Map.fromList [("potion", 2)])
-- ANCHOR_END: step1
-- ANCHOR: step2
  describe "rankByLevel (more properties)" $ do
    -- 「メンバーはひとりも減らない」
    prop "keeps every member" $
      \members ->
        length (rankByLevel members) `shouldBe` length members
    -- 「いちばんレベルの高いメンバーが先頭に来る」
    prop "puts the highest level first" $
      \members ->
        not (null members) ==>
          case rankByLevel members of
            first : _ -> level first `shouldBe` maximum (map level members)
            []        -> expectationFailure "the ranking is empty"
-- ANCHOR_END: step2
-- ANCHOR: step3
  describe "bestStreak" $
    -- 「素朴なやり方と同じ答えになる」
    prop "agrees with the naive way" $
      \(Positive k) damages ->
        bestStreak k damages `shouldBe` bestStreakNaive k damages
-- ANCHOR_END: step3
