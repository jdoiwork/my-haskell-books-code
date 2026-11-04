module CalculusSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import CalculusAnswers
import Math.Calculus

near :: Double -> Double -> Double -> Bool
near tolerance a b = abs (a - b) < tolerance

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 3次式の傾きは、公式 3x^2 + 2 と同じ
  it "the slope of x^3 + 2x is 3x^2 + 2" $
    forAll (choose (-10, 10)) $ \x ->
      near 1e-4 (derivative (\t -> t * t * t + 2 * t) x) (3 * x * x + 2)
  -- 坂を登ると、売上がいちばん多い値段 25 に着く
  it "climbing finds the best price, 25" $
    near 1e-6 (climb revenue 0.1 10 !! 50) 25 `shouldBe` True
  -- 小さな学習率なら、くぼ地の底 (3, -1) に着く
  it "a small step reaches the bottom of the valley" $ do
    let (x, y) = descend 0.1 (0, 0) !! 100
    (near 1e-6 x 3, near 1e-6 y (-1)) `shouldBe` (True, True)
  -- 大きすぎる学習率では、高さがどんどん増える
  it "too big a step climbs out of the valley" $ do
    let heights = map height (descend 0.6 (0, 0))
    heights !! 9 > heights !! 0 `shouldBe` True
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "Newton's method finds the square root of 2" $
      near 1e-12 (newton (\x -> x * x - 2) 1 !! 6) (sqrt 2) `shouldBe` True
    it "the best angle is 45 degrees" $
      near 1e-6 bestAngle 45 `shouldBe` True
