module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Answers
import Autograd
import AutogradDemo
import Dual
import MiniSklearn.Datasets
import qualified MiniSklearn.Linear as Lin
import qualified MiniNumpy as Np

-- 📏 The slope by a tiny step on each side (the reference).
-- 両側に小さく動かして測る傾き（お手本）
numeric :: (Double -> Double) -> Double -> Double
numeric g x = (g (x + h) - g (x - h)) / (2 * h)
  where h = 1e-6

near :: Double -> Double -> Double -> Bool
near tolerance a b = abs (a - b) < tolerance

main :: IO ()
main = hspec $ do
  -- 「自動微分」
  describe "autograd" $ do
    -- ANCHOR: examples
    -- 「f x y = x * y + sin x の傾き」
    it "finds the gradient of f" $ do
      let grads = backward (f (param "x" 2) (param "y" 3))
      (grad "x" grads, grad "y" grads) `shouldBe` (3 + cos 2, 2)

    -- 「同じ値を2回使うと、傾きは2つの道から足される」
    it "adds the gradients from two paths" $
      grad "x" (backward (let x = param "x" 3 in x * x))
        `shouldBe` 6
    -- ANCHOR_END: examples

    -- ANCHOR: props
    -- 「自動微分の傾きは、小さく動かして測った傾きと同じ」
    it "agrees with the numeric slope" $
      property $ forAll (choose (-3, 3)) $ \x ->
        forAll (choose (-3, 3)) $ \y ->
        let auto = grad "x" (backward (f (param "x" x) (constant y)))
        in near 1e-5 auto (numeric (\t -> f t y) x)

    -- 「二重数（フォワードモード）とも同じ」
    it "agrees with dual numbers" $
      property $ forAll (choose (-3, 3)) $ \x ->
        forAll (choose (-3, 3)) $ \y ->
        let auto = grad "x" (backward (f (param "x" x) (constant y)))
        in near 1e-12 auto (derivative (\t -> f t (Dual y 0)) x)
    -- ANCHOR_END: props

    -- 「autograd で学ぶ回帰は、前の章と同じ答え」
    it "learns the same line as the hand-written gradients" $ do
      let (x, y) = potionPrices
          m      = Lin.fit Lin.defaultOptions x y
      fitWithAutograd 0.01 2000 x y
        `shouldBe` (Np.item (Lin.coef m), Lin.intercept m)

  -- 「トレーニング」
  describe "answers" $ do
    it "the gradient of sigmoid is s * (1 - s)" $
      property $ forAll (choose (-4, 4)) $ \z ->
        let s = 1 / (1 + exp (negate z))
        in near 1e-12 (grad "z" (backward (sigmoidV (param "z" z))))
                      (s * (1 - s))
    it "gradDual runs once per input" $ do
      let g2 ds = case ds of
            [a, b] -> a * b + sin a
            _      -> 0
      gradDual g2 [2, 3]
        `shouldSatisfy` (\gs -> and (zipWith (near 1e-12) gs [3 + cos 2, 2]))
    it "squareChain n x = x ^ (2 ^ n)" $
      grad "x" (backward (squareChain 3 (param "x" 1.1)))
        `shouldSatisfy` near 1e-9 (8 * 1.1 ^ (7 :: Int))
