module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Review.Quests

-- 🎲 A few quests on a short day, so they overlap often.
-- 短い1日に、少しの依頼（よく重なるように）
genQuests :: Gen [Quest]
genQuests = do
  k <- chooseInt (0, 9)
  vectorOf k genQuest
  where
    genQuest = do
      s <- chooseInt (0, 12)
      len <- chooseInt (1, 5)
      r <- chooseInt (1, 20)
      pure (Quest "q" s (s + len) r)

-- ✂️ Shrink by dropping quests.
-- 依頼を減らして縮める
shrinkQuests :: [Quest] -> [[Quest]]
shrinkQuests = shrinkList (const [])

main :: IO ()
main = hspec $ do
  describe "the board" $ do
    -- 例の掲示板
    it "spec, slow DP and DP agree on the example" $ do
      bestRewardSpec board `shouldBe` 75
      bestRewardSlow board `shouldBe` 75
      bestReward board `shouldBe` 75
    -- 貪欲法は、例の掲示板で足りない
    it "greedy falls short" $
      greedyReward board `shouldBe` 60
  -- ANCHOR: props
  describe "properties" $ do
    -- 貪欲法は、報酬では負けることがある（失敗するのが正解）
    it "greedy can lose (expected to fail)" $ expectFailure $
      withMaxSuccess 2000 $ forAllShrink genQuests shrinkQuests $ \qs ->
        greedyReward qs === bestRewardSpec qs
    -- 遅い DP は、仕様と同じ
    it "bestRewardSlow == bestRewardSpec" $
      forAllShrink genQuests shrinkQuests $ \qs ->
        bestRewardSlow qs === bestRewardSpec qs
    -- 二分探索の DP も、仕様と同じ
    it "bestReward == bestRewardSpec" $
      forAllShrink genQuests shrinkQuests $ \qs ->
        bestReward qs === bestRewardSpec qs
  -- ANCHOR_END: props
