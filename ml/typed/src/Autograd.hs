-- ANCHOR: all
-- | A tiny autograd (reverse mode): build a value, then call backward
--   to get the gradient of every parameter, like PyTorch.
-- 小さな autograd（リバースモード）：値を作り、backward を呼ぶと、
-- PyTorch のように、すべてのパラメーターの傾きがわかる。
module Autograd
  ( Value
  , Grads
  , value
  , param
  , constant
  , backward
  , grad
  , relu
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- ANCHOR: value
-- 🔖 Gradients: for each parameter's name, d(output) / d(parameter).
-- 傾き：パラメーターの名前ごとの、d（出力）/ d（パラメーター）
type Grads = Map String Double

-- 💡 A value that remembers how to send a gradient back.
--    backprop g: "if d(output)/d(me) is g, here are the gradients
--    of the parameters I was made from".
-- 傾きを送り返す方法を覚えている値。
-- backprop g：「d（出力）/ d（自分）が g なら、自分の材料になった
-- パラメーターの傾きはこれ」
data Value = Value
  { value    :: Double
  , backprop :: Double -> Grads
  }

instance Show Value where
  show v = "Value " <> show (value v)

-- 🌱 A parameter: a leaf with a name. Its own gradient is g.
-- パラメーター：名前の付いた葉。自分の傾きは g そのもの
param :: String -> Double -> Value
param name x = Value x (Map.singleton name)

-- 🪨 A constant: it sends nothing back.
-- 定数：何も送り返さない
constant :: Double -> Value
constant x = Value x (const Map.empty)
-- ANCHOR_END: value

-- ANCHOR: backward
-- ⏪ Start with d(output)/d(output) = 1, and send it all the way back.
-- d（出力）/ d（出力）= 1 から始めて、いちばん後ろまで送り返す
backward :: Value -> Grads
backward v = backprop v 1

-- 🔍 The gradient of one parameter (0 if it was not used).
-- パラメーター1つの傾き（使われていなければ 0）
grad :: String -> Grads -> Double
grad = Map.findWithDefault 0
-- ANCHOR_END: backward

-- ANCHOR: num
-- ➕ Every operation says how to pass the gradient to its inputs
--    (the chain rule). Gradients from two paths are added up.
-- それぞれの計算は、入力に傾きをどう渡すかを決める（連鎖律）。
-- 2つの道から来た傾きは、足し合わせる
both :: Grads -> Grads -> Grads
both = Map.unionWith (+)

instance Num Value where
  a + b = Value (value a + value b)
                (\g -> backprop a g `both` backprop b g)
  a - b = Value (value a - value b)
                (\g -> backprop a g `both` backprop b (negate g))
  -- d(ab)/da = b,  d(ab)/db = a
  a * b = Value (value a * value b)
                (\g -> backprop a (g * value b)
                         `both` backprop b (g * value a))
  negate a    = Value (negate (value a)) (\g -> backprop a (negate g))
  abs a       = Value (abs (value a))
                      (\g -> backprop a (g * signum (value a)))
  signum a    = constant (signum (value a))
  fromInteger = constant . fromInteger

instance Fractional Value where
  -- d(1/a)/da = -1/a²
  recip a = Value (recip (value a))
                  (\g -> backprop a (negate g / (value a * value a)))
  fromRational = constant . fromRational
-- ANCHOR_END: num

-- ANCHOR: floating
-- 📐 One-input functions: the value, and its derivative.
-- 入力が1つの関数：値と、その導関数
lift1 :: (Double -> Double) -> (Double -> Double) -> Value -> Value
lift1 f f' a = Value (f (value a)) (\g -> backprop a (g * f' (value a)))

instance Floating Value where
  pi    = constant pi
  exp   = lift1 exp exp
  log   = lift1 log recip
  sqrt  = lift1 sqrt (\x -> 0.5 / sqrt x)
  sin   = lift1 sin cos
  cos   = lift1 cos (negate . sin)
  tanh  = lift1 tanh (\x -> 1 - tanh x ^ (2 :: Int))
  asin  = lift1 asin (\x -> 1 / sqrt (1 - x * x))
  acos  = lift1 acos (\x -> -1 / sqrt (1 - x * x))
  atan  = lift1 atan (\x -> 1 / (1 + x * x))
  sinh  = lift1 sinh cosh
  cosh  = lift1 cosh sinh
  asinh = lift1 asinh (\x -> 1 / sqrt (x * x + 1))
  acosh = lift1 acosh (\x -> 1 / sqrt (x * x - 1))
  atanh = lift1 atanh (\x -> 1 / (1 - x * x))

-- 📈 relu x = max 0 x: passes the gradient only where x > 0.
-- x > 0 のところだけ、傾きを通す
relu :: Value -> Value
relu = lift1 (max 0) (\x -> if x > 0 then 1 else 0)
-- ANCHOR_END: floating
-- ANCHOR_END: all
