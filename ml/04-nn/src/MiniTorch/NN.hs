-- ANCHOR: all
-- | Layers, a model, and an optimizer, in the style of torch.nn.
-- torch.nn 風の、層とモデルと最適化。
module MiniTorch.NN
  ( Params
  , initParams
  , linear
  , mlp
  , sgdStep
  , accuracy
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.Random (uniforms)
import MiniTorch.Tensor

-- ANCHOR: params
-- 🎒 All the learnable numbers, by name: "fc1.weight", "fc1.bias", ...
-- 学ぶ数のすべてを、名前で
type Params = Map String NDArray

-- 🌱 Small random weights in [-a, a], a = 1 / sqrt(inputs);
--    biases start at 0 (like nn.Linear, but with our own random numbers).
-- 重みは [-a, a] の小さな乱数、a = 1 / √入力の数。バイアスは 0 から
initParams :: Word64 -> [(String, Int, Int)] -> Params
initParams seed layers = Map.fromList (concat (zipWith make [0 ..] layers))
  where
    make i (name, inputs, outputs) =
      let a  = 1 / sqrt (fromIntegral inputs)
          us = take (inputs * outputs) (uniforms (seed + i))
          w  = Np.reshape [inputs, outputs]
                 (Np.array [ (2 * u - 1) * a | u <- us ])
      in [ (name <> ".weight", w), (name <> ".bias", Np.zeros [outputs]) ]
-- ANCHOR_END: params

-- ANCHOR: model
-- 🧱 nn.Linear: x @ weight + bias.
linear :: String -> Params -> Tensor -> Tensor
linear name ps x =
  let weightName = name <> ".weight"
      biasName   = name <> ".bias"
      weight     = param weightName (ps Map.! weightName)
      bias       = param biasName   (ps Map.! biasName)
  in  addBias (matmul x weight) bias

-- 🧠 A two-layer network: Linear -> ReLU -> Linear.
--    (torch: nn.Sequential(nn.Linear(35, 32), nn.ReLU(), nn.Linear(32, 10)))
-- 2層のネットワーク
mlp :: Params -> NDArray -> Tensor
mlp ps x =
  let hidden = relu (linear "fc1" ps (constant x))
  in  linear "fc2" ps hidden
-- ANCHOR_END: model

-- ANCHOR: sgd
-- 🚶 One step of SGD: every parameter moves against its gradient.
-- SGD の1歩：どのパラメーターも、傾きの逆へ動く
sgdStep :: Double -> Params -> Grads -> Params
sgdStep lr ps grads =
  Map.mapWithKey (\name p -> case Map.lookup name grads of
                               Just g  -> p - Np.scalar lr * g
                               Nothing -> p) ps

-- 🏅 How many rows does the model get right?
-- いくつの行を当てたか
accuracy :: Params -> NDArray -> [Int] -> Double
accuracy ps x labels =
  let guesses = Np.argmaxAxis1 (tvalue (mlp ps x))
      right   = length (filter id (zipWith (==) guesses labels))
  in fromIntegral right / fromIntegral (length labels)
-- ANCHOR_END: sgd
-- ANCHOR_END: all
