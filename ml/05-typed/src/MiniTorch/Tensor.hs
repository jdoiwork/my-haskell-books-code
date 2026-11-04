-- ANCHOR: all
-- | Autograd for whole arrays (tensors), like torch.Tensor.
-- 配列（テンソル）まるごとの自動微分。torch.Tensor のように。
module MiniTorch.Tensor
  ( Tensor
  , Grads
  , tvalue
  , param
  , constant
  , backward
  , matmul
  , addBias
  , relu
  , crossEntropy
  , oneHot
  , softmax
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)

-- ANCHOR: tensor
-- 🔖 The gradient of each parameter: an array of the same shape.
-- パラメーターごとの傾き：パラメーターと同じ形の配列
type Grads = Map String NDArray

-- 💡 Like Value in the last chapter, but the value is an array,
--    and so is the gradient sent back.
-- 前の章の Value と同じ。ただし、値も、送り返す傾きも、配列
data Tensor = Tensor
  { tvalue   :: NDArray
  , backprop :: NDArray -> Grads
  }

param :: String -> NDArray -> Tensor
param name x = Tensor x (Map.singleton name)

constant :: NDArray -> Tensor
constant x = Tensor x (const Map.empty)

-- ⏪ The loss is one number, so start with the gradient 1.
-- 損失は1つの数なので、傾き 1 から始める
backward :: Tensor -> Grads
backward t = backprop t (Np.scalar 1)

both :: Grads -> Grads -> Grads
both = Map.unionWith (+)
-- ANCHOR_END: tensor

-- ANCHOR: ops
-- ✖️ X @ W.  d/dX = g @ Wᵀ,  d/dW = Xᵀ @ g
-- 行列の積。X の傾きは g @ Wᵀ、W の傾きは Xᵀ @ g
matmul :: Tensor -> Tensor -> Tensor
matmul x w =
  Tensor (Np.dot (tvalue x) (tvalue w)) $ \g ->
    backprop x (Np.dot g (Np.transpose (tvalue w)))
      `both` backprop w (Np.dot (Np.transpose (tvalue x)) g)

-- ➕ Z + b, with b broadcast to every row. b's gradient adds up the rows.
-- Z + b（b は、どの行にもブロードキャスト）。b の傾きは、行を足したもの
addBias :: Tensor -> Tensor -> Tensor
addBias z b =
  Tensor (tvalue z + tvalue b) $ \g ->
    backprop z g `both` backprop b (Np.sumAxis 0 g)

-- 📈 relu: let the gradient through only where the value was > 0.
-- 値が 0 より大きかったところだけ、傾きを通す
relu :: Tensor -> Tensor
relu z =
  let mask = Np.map (\v -> if v > 0 then 1 else 0) (tvalue z)
  in Tensor (tvalue z * mask) (\g -> backprop z (g * mask))
-- ANCHOR_END: ops

-- ANCHOR: loss
-- 🌡️ Softmax of each row (numbers -> probabilities that add up to 1).
-- 行ごとの softmax（数を、足すと 1 になる確率にする）
softmax :: NDArray -> NDArray
softmax x =
  let n      = Np.nrows x
      maxes  = Np.reshape [n, 1]
                 (Np.array [ Np.maxAll (Np.row i x) | i <- [0 .. n - 1] ])
      e      = exp (x - maxes)
  in e / Np.reshape [n, 1] (Np.sumAxis 1 e)

-- 🎯 [3, 0] with 4 classes -> [[0,0,0,1], [1,0,0,0]]
oneHot :: Int -> [Int] -> NDArray
oneHot k labels =
  Np.array2 [ [ if j == c then 1 else 0 | j <- [0 .. k - 1] ] | c <- labels ]

-- 📉 Cross-entropy loss: how surprised we are by the right answers.
--    The gradient has a famously simple form: (softmax - onehot) / n.
-- 交差エントロピー損失：正解に、どれだけ驚いたか。
-- 傾きは、有名なほど単純な形：(softmax - onehot) / n
crossEntropy :: Tensor -> [Int] -> Tensor
crossEntropy logits labels =
  let p      = softmax (tvalue logits)
      k      = Np.shape p !! 1
      target = oneHot k labels
      n      = fromIntegral (length labels)
      loss   = negate (Np.sum (target * log p)) / Np.scalar n
  in Tensor loss $ \g ->
       backprop logits (g * (p - target) / Np.scalar n)
-- ANCHOR_END: loss
-- ANCHOR_END: all
