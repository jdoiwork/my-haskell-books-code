-- ANCHOR: head
-- ANCHOR: all
-- | Optimizers in the style of torch.optim: SGD, momentum, and Adam.
-- torch.optim 風の最適化：SGD、Momentum、Adam
module Review.Optim
  ( Optimizer (..)
  , sgd
  , momentum
  , AdamConfig (..)
  , defaultAdam
  , adam
  , trainWith
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.NN (Params, mlp, sgdStep)
import MiniTorch.Tensor (Grads, backward, crossEntropy, tvalue)
-- ANCHOR_END: head

-- ANCHOR: optimizer
-- 🧭 An optimizer: give it the parameters and their gradients, and it
--    gives back the moved parameters and the next optimizer (which
--    remembers whatever it needs, like a velocity).
-- 最適化：パラメーターと傾きを渡すと、動かしたパラメーターと、
-- 次の最適化（速度など、覚えておくものを持っている）を返す
newtype Optimizer = Optimizer
  { step :: Params -> Grads -> (Params, Optimizer) }

-- 🚶 Plain SGD remembers nothing. (torch.optim.SGD(lr=...))
-- ふつうの SGD は、何も覚えない
sgd :: Double -> Optimizer
sgd lr = Optimizer (\ps grads -> (sgdStep lr ps grads, sgd lr))
-- ANCHOR_END: optimizer

-- ANCHOR: momentum
-- 🛷 Momentum: keep a velocity for each parameter, like a sled.
--    v = mu * v + g (the first time, v = g);  p = p - lr * v.
--    A parameter with no gradient this time does not move, and keeps
--    its velocity for later (like PyTorch).
--    (torch.optim.SGD(lr=..., momentum=...))
-- そりのように、パラメーターごとに速度を持つ。今回傾きのないパラメーターは
-- 動かさず、速度はあとのために取っておく（PyTorch と同じ）
momentum :: Double -> Double -> Optimizer
momentum lr mu = go Map.empty
  where
    go :: Map String NDArray -> Optimizer
    go velocity = Optimizer $ \ps grads ->
      let updated = Map.mapWithKey speedUp grads
          speedUp name g = case Map.lookup name velocity of
            Nothing -> g
            Just v  -> Np.scalar mu * v + g
          move name p = case Map.lookup name updated of
            Nothing -> p
            Just v  -> p - Np.scalar lr * v
      in  (Map.mapWithKey move ps, go (Map.union updated velocity))
-- ANCHOR_END: momentum

-- ANCHOR: adam
-- ANCHOR: adamconfig
-- ⚙️ Adam's settings, with PyTorch's defaults.
-- Adam の設定。PyTorch の既定値
data AdamConfig = AdamConfig
  { learningRate :: Double
  , beta1        :: Double
  , beta2        :: Double
  , epsilon      :: Double
  }

defaultAdam :: AdamConfig
defaultAdam = AdamConfig
  { learningRate = 0.001
  , beta1        = 0.9
  , beta2        = 0.999
  , epsilon      = 1e-8
  }
-- ANCHOR_END: adamconfig

-- 🦉 Adam: an average of the gradients (m) and of their squares (v).
--    Each parameter gets its own step size: big where the gradient is
--    steady, small where it jumps around.
-- 傾きの平均 m と、傾きの2乗の平均 v。パラメーターごとに歩幅が変わる
--    Each parameter counts its own steps (t), and a parameter with no
--    gradient this time keeps its averages (like PyTorch).
-- 歩数 t もパラメーターごと。今回傾きのないパラメーターは、
-- 平均を取っておく（PyTorch と同じ）
adam :: AdamConfig -> Optimizer
adam cfg = go Map.empty Map.empty Map.empty
  where
    go :: Map String Int -> Map String NDArray -> Map String NDArray
       -> Optimizer
    go ts ms vs = Optimizer $ \ps grads ->
      let b1  = beta1 cfg
          b2  = beta2 cfg
          ts' = Map.mapWithKey (\name _ -> Map.findWithDefault 0 name ts + 1)
                               grads
          ms' = Map.mapWithKey (average b1 ms) grads
          vs' = Map.mapWithKey (average b2 vs) (Map.map (\g -> g * g) grads)
          move name p = case ( Map.lookup name ts', Map.lookup name ms'
                             , Map.lookup name vs' ) of
            (Just t, Just m, Just v) ->
              -- Undo the pull toward 0 of the first steps (bias correction).
              -- 最初のうち 0 に引っぱられる分を戻す（バイアス補正）
              let mHat = m / Np.scalar (1 - b1 ^ t)
                  vHat = v / Np.scalar (1 - b2 ^ t)
              in  p - Np.scalar (learningRate cfg) * mHat
                        / (sqrt vHat + Np.scalar (epsilon cfg))
            _ -> p
      in  ( Map.mapWithKey move ps
          , go (Map.union ts' ts) (Map.union ms' ms) (Map.union vs' vs) )

-- 📊 old * beta + new * (1 - beta); the first time, old is 0.
-- 古い平均 × beta + 新しい値 × (1 - beta)。最初は、古い平均を 0 とする
average :: Double -> Map String NDArray -> String -> NDArray -> NDArray
average beta old name new = case Map.lookup name old of
  Nothing -> Np.scalar (1 - beta) * new
  Just o  -> Np.scalar beta * o + Np.scalar (1 - beta) * new
-- ANCHOR_END: adam

-- ANCHOR: train
-- 🔁 Train for n steps with any optimizer. The losses are n + 1: before
--    each step, and after the last one (the loss of the parameters it
--    gives back).
-- どの最適化でも n 歩学ぶ。損失は n + 1 個：各歩の前と、最後の歩のあと
-- （返すパラメーターの損失）
trainWith :: Optimizer -> Int -> Params -> NDArray -> [Int]
          -> ([Double], Params)
trainWith _   0 ps x ys =
  ([Np.item (tvalue (crossEntropy (mlp ps x) ys))], ps)
trainWith opt n ps x ys =
  let loss          = crossEntropy (mlp ps x) ys
      (ps', opt')   = step opt ps (backward loss)
      (rest, final) = trainWith opt' (n - 1) ps' x ys
  in  (Np.item (tvalue loss) : rest, final)
-- ANCHOR_END: train
-- ANCHOR_END: all
