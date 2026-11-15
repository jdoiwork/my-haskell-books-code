-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the scikit-learn chapter.
-- scikit-learn の章のトレーニングの解答。
module Answers
  ( fitClosed
  , Scaler (..)
  , fitScaler
  , transform
  , standardize
  , splitEvery
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniSklearn.Linear (LinearRegression (..))
-- ANCHOR_END: head

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
-- 📏 The mean and the spread of each column, learned from the training
--    data (like StandardScaler's mean_ and scale_).
-- 列ごとの平均とばらつき。学習用のデータから学ぶ
-- （StandardScaler の mean_ と scale_）
data Scaler = Scaler
  { means   :: NDArray
  , spreads :: NDArray
  }
  deriving (Show)

-- 🏋️ Learn the scaler from X. A column with no spread (every number
--    the same) is divided by 1, as StandardScaler does.
-- X から学ぶ。ばらつきのない列（全部同じ数）は、1 で割る
-- （StandardScaler と同じ）
fitScaler :: NDArray -> Scaler
fitScaler x =
  let n      = fromIntegral (Np.nrows x)
      ms     = Np.sumAxis 0 x / Np.scalar n
      spread = sqrt (Np.sumAxis 0 ((x - ms) * (x - ms)) / Np.scalar n)
  in Scaler ms (Np.map (\s -> if s == 0 then 1 else s) spread)

-- 🔁 Use the learned means and spreads on any X (training or new data).
-- 学んだ平均とばらつきで、どの X でも変換する（学習用でも、新しいデータでも）
transform :: Scaler -> NDArray -> NDArray
transform sc x = (x - means sc) / spreads sc

-- 📏 Make each column have mean 0 and spread 1.
-- 列ごとに、平均 0・ばらつき 1 にする
standardize :: NDArray -> NDArray
standardize x = transform (fitScaler x) x
-- ANCHOR_END: standardize

-- ANCHOR: split
-- ✂️ Every k-th row goes to the test set, the rest to training.
--    Returns (train X, train y, test X, test y).
-- k 行ごとに1行をテスト用に、残りを学習用にする
splitEvery
  :: Int -> NDArray -> NDArray -> (NDArray, NDArray, NDArray, NDArray)
splitEvery k x y
  | k < 2     = error "splitEvery: k must be 2 or more"
  | otherwise =
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
