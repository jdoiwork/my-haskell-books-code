-- ANCHOR: all
-- ANCHOR: head
-- 🧪 Stress tests: random small cases, fast answer against slow answer.
-- ストレステスト：でたらめな小さい問題で、速い解と遅い解を比べる
module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Algo
import CompeteAnswers

main :: IO ()
main = hspec $ do
  stressSpec
  -- answers for the training / トレーニングの解答
  answersSpec
-- ANCHOR_END: head

-- ANCHOR: spec
stressSpec :: Spec
stressSpec = do
  -- 「累積和で引いた区間の合計は、ふつうに足した合計と同じ」
  prop "rangeSum agrees with adding up" $
    \(NonEmpty damages) (Positive a) (Positive b) ->
      let n = length damages
          l = (a `mod` n) + 1
          r = l + (b `mod` (n - l + 1))
      in  rangeSum (prefixSums damages) l r === rangeSumNaive damages l r

  -- 「二分探索の答えは、1から順に試した答えと同じ」
  prop "weakestAttack agrees with trying in order" $
    \(NonEmpty hps') (Positive extra) ->
      let hps = map (\h -> h `mod` 100 + 1) hps'
          k   = length hps + extra `mod` 50
      in  weakestAttack hps k === weakestAttackNaive hps k
-- ANCHOR_END: spec

-- ANCHOR: answers
answersSpec :: Spec
answersSpec =
  -- ANCHOR: ways
  -- 「動的計画法の答えは、すべて試した答えと同じ」
  prop "waysToPay agrees with trying every choice" $
    -- a few kinds of coins: the slow answer blows up with many
    -- 硬貨の種類は少しだけ（多いと、遅い解が爆発する）
    forAll (resize 5 (listOf (choose (1, 6)))) $ \coins ->
      forAll (choose (0, 15)) $ \amount ->
        waysToPay coins amount === waysToPayNaive coins amount
  -- ANCHOR_END: ways
-- ANCHOR_END: answers
-- ANCHOR_END: all
