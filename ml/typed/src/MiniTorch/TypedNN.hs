-- ANCHOR: head
-- ANCHOR: all
-- | A network whose layer sizes are in its type: MLP 35 32 10.
--   Inside, it uses the untyped MiniTorch from the last chapter.
-- 層の大きさが型に書いてあるネットワーク：MLP 35 32 10。
-- 中では、前の章の型のない MiniTorch を使う
module MiniTorch.TypedNN
  ( Linear (..)
  , MLP (..)
  , initMLP
  , linear
  , mlp
  , predict
  , toParams
  , trainStep
  ) where

import Data.Proxy (Proxy (..))
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.TypeLits (KnownNat, Nat)

import qualified MiniNumpy as Np
import qualified MiniTorch.NN as NN
import MiniTorch.NN (Params)
import MiniTorch.Tensor (backward, crossEntropy, tvalue)
import MiniTorch.Typed
-- ANCHOR_END: head

-- ANCHOR: layers
-- 🧱 nn.Linear(i, o), with i and o in the type.
-- 入力 i と出力 o を型に書いた nn.Linear
data Linear (i :: Nat) (o :: Nat) = Linear
  { weight :: Matrix i o
  , bias   :: Vector o
  }

-- 🧠 Linear i h -> ReLU -> Linear h o.
--    The h in the middle must be the same number in both layers.
-- 真ん中の h は、2つの層で同じ数でないといけない
data MLP i h o = MLP
  { fc1 :: Linear i h
  , fc2 :: Linear h o
  }
-- ANCHOR_END: layers

-- ANCHOR: forward
-- ➡️ x @ weight + bias: (n × i) becomes (n × o).
-- (n × i) が (n × o) になる
linear :: Linear i o -> Matrix n i -> Matrix n o
linear layer x = addRow (matmul x (weight layer)) (bias layer)

-- ➡️ The whole network: (n × i) becomes (n × o).
-- ネットワーク全体：(n × i) が (n × o) になる
mlp :: MLP i h o -> Matrix n i -> Matrix n o
mlp net x =
  let hidden = mapMatrix (max 0) (linear (fc1 net) x)
  in  linear (fc2 net) hidden

-- 🏷️ The best guess for each row.
-- 行ごとの、いちばんの答え
predict :: MLP i h o -> Matrix n i -> [Int]
predict net x = Np.argmaxAxis1 (toNDArray (mlp net x))
-- ANCHOR_END: forward

-- ANCHOR: init
-- 🌱 The sizes come from the type: initMLP @35 @32 @10 7.
--    Same random numbers as initParams in the last chapter.
-- 大きさは型から読む。乱数は前の章の initParams と同じ
initMLP :: forall i h o. (KnownNat i, KnownNat h, KnownNat o)
        => Word64 -> MLP i h o
initMLP seed =
  let sizeI = natInt (Proxy @i)
      sizeH = natInt (Proxy @h)
      sizeO = natInt (Proxy @o)
      ps = NN.initParams seed [("fc1", sizeI, sizeH), ("fc2", sizeH, sizeO)]
  in  fromParams ps
-- ANCHOR_END: init

-- ANCHOR: bridge
-- 🌉 Typed -> untyped: give the layers their names.
-- 型あり → 型なし：層に名前を付ける
toParams :: MLP i h o -> Params
toParams net = Map.fromList
  [ ("fc1.weight", toNDArray       (weight (fc1 net)))
  , ("fc1.bias",   vectorToNDArray (bias   (fc1 net)))
  , ("fc2.weight", toNDArray       (weight (fc2 net)))
  , ("fc2.bias",   vectorToNDArray (bias   (fc2 net)))
  ]

-- 🌉 Untyped -> typed. Only this module calls it, with Params that came
--    from an MLP i h o (or from initParams with the sizes of the type),
--    so the shapes are right. That is why unsafe is fine here.
-- 型なし → 型あり。MLP i h o から来た（か、型の大きさで作った）Params
-- だけを渡すので、形は正しい。だから、ここでは unsafe でよい
fromParams :: Params -> MLP i h o
fromParams ps = MLP
  { fc1 = Linear (unsafeMatrix (ps Map.! "fc1.weight"))
                 (unsafeVector (ps Map.! "fc1.bias"))
  , fc2 = Linear (unsafeMatrix (ps Map.! "fc2.weight"))
                 (unsafeVector (ps Map.! "fc2.bias"))
  }

-- 🚶 One step of SGD on a batch of n rows. The answer has the same type
--    as the network we started with: the step cannot change a shape.
-- n 行の束で SGD を1歩。答えは、元のネットワークと同じ型。
-- 1歩で形が変わることはない
trainStep :: Double -> MLP i h o -> Matrix n i -> [Int] -> (Double, MLP i h o)
trainStep lr net x labels =
  let ps    = toParams net
      loss  = crossEntropy (NN.mlp ps (toNDArray x)) labels
      grads = backward loss
  in  (Np.item (tvalue loss), fromParams (NN.sgdStep lr ps grads))
-- ANCHOR_END: bridge
-- ANCHOR_END: all
