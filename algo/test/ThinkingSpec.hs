module ThinkingSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Algo.Thinking
import AlgoAnswers

spec :: Spec
spec = describe "Algo.Thinking" $ do
  -- 例のダンジョン
  it "finds the best run in the example" $
    map ($ dungeon) [bestRun0, bestRun1, bestRun2, bestRun3, bestRun]
      `shouldBe` [11, 11, 11, 11, 11]
  -- ANCHOR: props
  -- 変形の1歩ごとに、仕様と同じ答え
  it "step 1 agrees with the spec" $ property $ \xs ->
    bestRun1 xs === bestRun0 xs
  it "step 2 agrees with the spec" $ property $ \xs ->
    bestRun2 xs === bestRun0 xs
  it "step 3 agrees with the spec" $ property $ \xs ->
    bestRun3 xs === bestRun0 xs
  it "the one-pass version agrees with the spec" $ property $ \xs ->
    bestRun xs === bestRun0 xs
  -- ANCHOR_END: props
  -- トレーニングの解答
  it "bestRunNonEmpty agrees with its own spec" $ property $
    \(NonEmpty xs) ->
      bestRunNonEmpty xs ===
        maximum [ sum run | run <- segments xs, not (null run) ]
  it "bestRunWhere finds the example's run" $
    bestRunWhere dungeon `shouldBe` (11, 2, 4)
