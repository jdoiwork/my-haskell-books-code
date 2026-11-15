-- ANCHOR: all
-- | Answers to the training of the scikit-learn chapter.
-- scikit-learn の章のトレーニングの解答。
module Answers
  ( fitClosed
  , standardize
  , splitEvery
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniSklearn.Linear (LinearRegression (..))

-- ANCHOR: closed
-- 🧮 One feature only: the best line, by a formula (no walking).
--    slope = cov(x, y) / var(x),  intercept = mean y - slope * mean x
-- 特徴が1つのときの、いちばんよい直線を、式で一気に求める（歩かない）
fitClosed :: NDArray -> NDArray -> LinearRegression
fitClosed x y =
  let xs    = Np.reshape [Np.size x] x
      mx    = Np.mean xs
      my    = Np.mean y
      cov   = Np.item (Np.sum ((xs - mx) * (y - my)))
      var   = Np.item (Np.sum ((xs - mx) * (xs - mx)))
      slope = cov / var
  in LinearRegression (Np.array [slope]) (Np.item my - slope * Np.item mx)
-- ANCHOR_END: closed

-- ANCHOR: standardize
-- 📏 Make each column have mean 0 and spread 1 (StandardScaler).
-- 列ごとに、平均 0・ばらつき 1 にする（StandardScaler）
standardize :: NDArray -> NDArray
standardize x =
  let n      = fromIntegral (Np.nrows x)
      means  = Np.sumAxis 0 x / Np.scalar n
      spread = sqrt (Np.sumAxis 0 ((x - means) * (x - means)) / Np.scalar n)
  in (x - means) / spread
-- ANCHOR_END: standardize

-- ANCHOR: split
-- ✂️ Every k-th row goes to the test set, the rest to training.
--    Returns (train X, train y, test X, test y).
-- k 行ごとに1行をテスト用に、残りを学習用にする
splitEvery
  :: Int -> NDArray -> NDArray -> (NDArray, NDArray, NDArray, NDArray)
splitEvery k x y =
  let n       = Np.nrows x
      isTest i = i `mod` k == 0
      pick p  = [ i | i <- [0 .. n - 1], p i ]
      rowsAt is = Np.array2 [ Np.toList (Np.row i x) | i <- is ]
      ysAt is   = Np.array [ Np.toList y !! i | i <- is ]
      trainIx = pick (not . isTest)
      testIx  = pick isTest
  in (rowsAt trainIx, ysAt trainIx, rowsAt testIx, ysAt testIx)
-- ANCHOR_END: split
-- ANCHOR_END: all
