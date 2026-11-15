-- ANCHOR: all
-- | Answers to the training of the NumPy chapter.
-- NumPy の章のトレーニングの解答。
-- ANCHOR: head
module Answers
  ( eye
  , clip
  , softmaxRows
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
-- ANCHOR_END: head

-- ANCHOR: eye
-- 🪪 Np.eye 3: 1 on the diagonal, 0 elsewhere.
-- 対角線に 1、ほかは 0
eye :: Int -> NDArray
eye n = Np.array2 [ [ if i == j then 1 else 0 | j <- [1 .. n] ]
                   | i <- [1 .. n] ]
-- ANCHOR_END: eye

-- ANCHOR: clip
-- ✂️ Np.clip lo hi a: numbers below lo become lo, above hi become hi.
--    If lo > hi, every number becomes hi (the same as np.clip).
-- lo より小さい数は lo に、hi より大きい数は hi にする。
-- lo が hi より大きければ、全部 hi になる（np.clip と同じ）
clip :: Double -> Double -> NDArray -> NDArray
clip lo hi = Np.map (min hi . max lo)
-- ANCHOR_END: clip

-- ANCHOR: softmax
-- 🌡️ Softmax of each row: exp(x - max) / sum(exp(x - max)).
--    Every row becomes numbers from 0 to 1 that add up to 1.
-- 行ごとの softmax：どの行も、足すと 1 になる、0 から 1 の数になる
softmaxRows :: NDArray -> NDArray
softmaxRows x =
  let rows    = Np.nrows x
      -- The max of each row, as a column: shape [rows, 1].
      -- 行ごとの最大値を、列にする（形 [rows, 1]）
      maxes   = Np.reshape [rows, 1]
                  (Np.array [ Np.maxAll (Np.row i x) | i <- [0 .. rows - 1] ])
      e       = exp (x - maxes)
      totals  = Np.reshape [rows, 1] (Np.sumAxis 1 e)
  in e / totals
-- ANCHOR_END: softmax
-- ANCHOR_END: all
