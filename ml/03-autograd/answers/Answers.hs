-- ANCHOR: all
-- | Answers to the training of the autograd chapter.
-- 自動微分の章のトレーニングの解答。
module Answers
  ( sigmoidV
  , gradDual
  , squareChain
  ) where

import Autograd
import Dual

-- ANCHOR: sigmoid
-- 〰️ sigmoid, written with exp: no new gradient rule is needed.
-- exp で書いた sigmoid：新しい傾きの決まりは要らない
sigmoidV :: Value -> Value
sigmoidV z = 1 / (1 + exp (negate z))
-- ANCHOR_END: sigmoid

-- ANCHOR: gradDual
-- 👯 The gradient with dual numbers: one run per input.
-- 二重数で求める勾配：入力1つにつき、1回ずつ走らせる
gradDual :: ([Dual] -> Dual) -> [Double] -> [Double]
gradDual g xs =
  [ let Dual _ d = g [ Dual x (if j == i then 1 else 0)
                     | (j, x) <- zip [0 :: Int ..] xs ]
    in d
  | i <- [0 .. length xs - 1] ]
-- ANCHOR_END: gradDual

-- ANCHOR: chain
-- 🔁 Square n times: x², x⁴, x⁸, ... Every step uses the value twice.
-- n 回2乗する。どの段も、値を2回使う
squareChain :: Num a => Int -> a -> a
squareChain 0 x = x
squareChain n x = let y = squareChain (n - 1) x in y * y
-- ANCHOR_END: chain
-- ANCHOR_END: all
