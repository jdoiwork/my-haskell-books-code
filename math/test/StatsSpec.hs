module StatsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Math.Stats
import StatsAnswers

-- ANCHOR: helpers
-- 🎲 A non-empty list of numbers.
-- 空でない数のリスト
genData :: Gen [Double]
genData = listOf1 (choose (-1000, 1000))

-- 📏 Close enough for decimals?
-- 小数として、十分に近い？
near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-6
-- ANCHOR_END: helpers

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 中央値は、いちばん小さい値と大きい値のあいだ
  it "the median is between the smallest and the largest" $
    forAll genData $ \xs ->
      minimum xs <= median xs && median xs <= maximum xs
  -- 全部に同じ数を足しても、ばらつきは変わらない
  it "shifting the data does not change the spread" $
    forAll genData $ \xs -> forAll (choose (-100, 100)) $ \c ->
      near (stdDev (map (+ c) xs)) (stdDev xs)
  -- パーセンタイルは、p が大きいほど大きい（か同じ）
  it "a higher percentile is never smaller" $
    forAll genData $ \xs -> forAll (choose (1, 99)) $ \p ->
      percentile p xs <= percentile (p + 1) xs
  -- まっすぐ一緒に増えるなら +1、逆なら -1
  it "a straight line has correlation 1 or -1" $
    forAll (vectorOf 20 (choose (-100, 100))) $ \xs ->
      stdDev xs > 0 ==>
        near (correlation xs (map (\x -> 2 * x + 1) xs)) 1
          && near (correlation xs (map negate xs)) (-1)
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "the trimmed mean ignores the dragon slayer" $
      trimmedMean 1 memberGold `shouldBe` 70.22222222222223
    it "outliers finds the dragon slayer" $
      outliers 2 memberGold `shouldBe` [100000]
