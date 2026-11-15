-- ANCHOR: head
-- ANCHOR: all
-- | Logistic regression: sort things into two groups (0 or 1).
--   import qualified MiniSklearn.Logistic as Logi
-- ロジスティック回帰：ものを2つの組（0 か 1）に分ける。
module MiniSklearn.Logistic
  ( LogisticRegression (..)
  , fit
  , predictProba
  , predict
  , score
  , sigmoid
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniSklearn.Linear (Options (..))
-- ANCHOR_END: head

-- ANCHOR: model
-- 🎚️ Like linear regression, but the line goes through sigmoid,
--    which squeezes any number into 0 .. 1 (a probability).
-- 線形回帰と同じだが、直線を sigmoid に通して、0〜1（確率）にする
data LogisticRegression = LogisticRegression
  { coef      :: NDArray
  , intercept :: Double
  }
  deriving (Show)

-- 〰️ sigmoid z = 1 / (1 + e^(-z)):  -∞ -> 0,  0 -> 0.5,  +∞ -> 1
sigmoid :: NDArray -> NDArray
sigmoid z = 1 / (1 + exp (negate z))
-- ANCHOR_END: model

-- ANCHOR: predict
-- 🔮 The probability of 1 for each row, and the guessed group.
-- 行ごとの「1 である確率」と、予想した組
predictProba :: LogisticRegression -> NDArray -> NDArray
predictProba m x = sigmoid (Np.dot x (coef m) + Np.scalar (intercept m))

predict :: LogisticRegression -> NDArray -> NDArray
predict m x = Np.map (\p -> if p >= 0.5 then 1 else 0) (predictProba m x)

-- 🏅 Accuracy: how many guesses were right.
-- 正解率：いくつ当たったか
score :: LogisticRegression -> NDArray -> NDArray -> Double
score m x y =
  let right = Np.map (\d -> if d == 0 then 1 else 0) (predict m x - y)
  in Np.item (Np.mean right)
-- ANCHOR_END: predict

-- ANCHOR: fit
-- 🏋️ Learn by gradient descent. The slope of the log loss has the
--    same shape as linear regression's: X^T (p - y) / n.
-- 勾配降下法で学ぶ。対数損失の傾きは、線形回帰と同じ形になる
fit :: Options -> NDArray -> NDArray -> LogisticRegression
fit opts x y =
  let features = Np.shape x !! 1
      n        = fromIntegral (Np.nrows x)
      start    = LogisticRegression (Np.zeros [features]) 0
      step m =
        let err   = predictProba m x - y
            gradW = Np.dot (Np.transpose x) err / Np.scalar n
            gradB = Np.item (Np.mean err)
            lr    = learningRate opts
        in LogisticRegression (coef m - Np.scalar lr * gradW)
                              (intercept m - lr * gradB)
  in iterate step start !! epochs opts
-- ANCHOR_END: fit
-- ANCHOR_END: all
