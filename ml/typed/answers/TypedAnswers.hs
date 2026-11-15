-- ANCHOR: all
-- | Answers to the training of "Keeping dimensions in types".
-- 「型で次元を守る」のトレーニングの解答
module TypedAnswers
  ( MLP3 (..)
  , mlp3
  , splitRows
  ) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, type (+))

import qualified MiniNumpy as Np
import MiniTorch.Typed
import MiniTorch.TypedNN

-- ANCHOR: mlp3
-- 🧠 Step 2: three layers. Each middle size appears in two layers.
-- ステップ2：3層。真ん中の大きさは、どれも2つの層に出てくる
data MLP3 i h1 h2 o = MLP3
  { layer1 :: Linear i  h1
  , layer2 :: Linear h1 h2
  , layer3 :: Linear h2 o
  }

mlp3 :: MLP3 i h1 h2 o -> Matrix n i -> Matrix n o
mlp3 net x =
  let hidden1 = mapMatrix (max 0) (linear (layer1 net) x)
      hidden2 = mapMatrix (max 0) (linear (layer2 net) hidden1)
  in  linear (layer3 net) hidden2
-- ANCHOR_END: mlp3

-- ANCHOR: split
-- ✂️ Step 3: the opposite of vstack. The first a rows, and the other b.
--    a + b rows go in, so taking a rows always leaves exactly b:
--    the shapes are right, and unsafeMatrix is fine here.
-- ステップ3：vstack の逆。最初の a 行と、残りの b 行。
-- a + b 行が入ってくるので、a 行を取れば、残りはちょうど b 行。
-- 形は正しいので、ここでは unsafeMatrix でよい
splitRows :: forall a b c. (KnownNat a, KnownNat b, KnownNat c)
          => Matrix (a + b) c -> (Matrix a c, Matrix b c)
splitRows m =
  let rowsA  = natInt (Proxy @a)
      rowsB  = natInt (Proxy @b)
      cols   = natInt (Proxy @c)
      values = Np.toList (toNDArray m)
      (top, bottom) = splitAt (rowsA * cols) values
  in  ( unsafeMatrix (Np.reshape [rowsA, cols] (Np.array top))
      , unsafeMatrix (Np.reshape [rowsB, cols] (Np.array bottom)) )
-- ANCHOR_END: split
-- ANCHOR_END: all
