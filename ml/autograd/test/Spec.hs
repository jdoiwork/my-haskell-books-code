-- ANCHOR: head
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
-- ANCHOR_END: head

-- ANCHOR: unary
-- 🧪 One-input functions, with a range where each is defined.
--    For each, autograd (Value), dual numbers and the numeric slope
--    must agree.
-- 入力が1つの関数と、それぞれが定まる範囲。autograd・二重数・
-- 数値微分の3つが同じになるはず
newtype Unary = Unary (forall a. Floating a => a -> a)

unary :: [(String, (Double, Double), Unary)]
unary =
  [ ("exp",    (-3, 3),      Unary exp)
  , ("log",    (0.1, 5),     Unary log)
  , ("sqrt",   (0.1, 5),     Unary sqrt)
  , ("sin",    (-3, 3),      Unary sin)
  , ("cos",    (-3, 3),      Unary cos)
  , ("tanh",   (-3, 3),      Unary tanh)
  , ("asin",   (-0.9, 0.9),  Unary asin)
  , ("acos",   (-0.9, 0.9),  Unary acos)
  , ("atan",   (-3, 3),      Unary atan)
  , ("sinh",   (-3, 3),      Unary sinh)
  , ("cosh",   (-3, 3),      Unary cosh)
  , ("asinh",  (-3, 3),      Unary asinh)
  , ("acosh",  (1.1, 5),     Unary acosh)
  , ("atanh",  (-0.9, 0.9),  Unary atanh)
  , ("abs",    (-3, 3),      Unary abs)
  , ("1 - x",  (-3, 3),      Unary (1 -))
  , ("1 / x",  (0.5, 3),     Unary recip)
  , ("x ** 2", (-3, 3),      Unary (** 2))
  , ("2 ** x", (-3, 3),      Unary (2 **))
  ]
-- ANCHOR_END: unary

-- ANCHOR: main
main :: IO ()
main = hspec $ do
  -- 「自動微分」
  describe "autograd" $ do
-- ANCHOR_END: main
    -- ANCHOR: examples
    -- 「f x y = x * y + sin x の傾き」
    it "finds the gradient of f" $ do
      let grads = backward (f (param "x" 2) (param "y" 3))
      (grad "x" grads, grad "y" grads) `shouldBe` (3 + cos 2, 2)

    -- 「同じ値を2回使うと、傾きは2つの道から足される」
    it "adds the gradients from two paths" $
      grad "x" (backward (let x = param "x" 3 in x * x))
        `shouldBe` 6

    -- 「引き算の右側にも、符号を変えて傾きが届く」
    it "sends a negative gradient to the right of -" $
      grad "y" (backward (param "x" 5 - param "y" 2)) `shouldBe` (-1)

    -- 「relu は、正の側だけ傾きを通す」
    it "lets relu pass the gradient only above 0" $
      map (\v -> grad "x" (backward (relu (param "x" v)))) [-2, 3]
        `shouldBe` [0, 1]
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

    -- 「部品の関数も1つずつ、3つのやり方が同じ」
    it "agrees for every one-input function" $
      conjoin
        [ counterexample name $
            forAll (choose range) $ \x ->
              let auto = grad "x" (backward (g (param "x" x)))
              in near 1e-5 auto (numeric g x)
                   && near 1e-9 auto (derivative g x)
        | (name, range, Unary g) <- unary ]
    -- ANCHOR_END: props

    -- 「autograd で学ぶ回帰は、前の章と同じ答え（0.01・2000 歩では、
    --   最後の桁まで）」
    it "learns the same line as the hand-written gradients" $ do
      let (x, y) = potionPrices
          m      = Lin.fit Lin.defaultOptions x y
      fitWithAutograd 0.01 2000 x y
        `shouldBe` (Np.item (Lin.coef m), Lin.intercept m)

    -- 「(-2) ** 2 も、Double と同じく計算できる」
    it "takes powers of negative numbers" $ do
      let v = param "x" (-2) ** 2
      (value v, grad "x" (backward v)) `shouldBe` (4, -4)
      derivative (** 2) (-2) `shouldBe` (-4)

  -- 「トレーニング」
  -- ANCHOR: answers
  describe "answers" $ do
    -- 「sigmoid の傾きは s * (1 - s)（15 桁くらいまで）」
    it "the gradient of sigmoid is s * (1 - s)" $
      property $ forAll (choose (-4, 4)) $ \z ->
        let s = 1 / (1 + exp (negate z))
        in near 1e-12 (grad "z" (backward (sigmoidV (param "z" z))))
                      (s * (1 - s))

    -- 「二重数で、入力ごとの傾きを全部」
    it "gradDual finds every input's slope" $ do
      let g2 ds = case ds of
            [a, b] -> a * b + sin a
            _      -> 0
      gradDual g2 [2, 3]
        `shouldSatisfy` (\gs -> length gs == 2
          && and (zipWith (near 1e-12) gs [3 + cos 2, 2]))

    -- 「n 回2乗すると、x ^ (2 ^ n)」
    it "squareChain n x = x ^ (2 ^ n)" $
      grad "x" (backward (squareChain 3 (param "x" 1.1)))
        `shouldSatisfy` near 1e-9 (8 * 1.1 ^ (7 :: Int))
  -- ANCHOR_END: answers
