-- ANCHOR: head
-- ANCHOR: all
-- | Linear regression, learned by gradient descent:
--   import qualified MiniSklearn.Linear as Lin
-- 勾配降下法で学ぶ線形回帰。
module MiniSklearn.Linear
  ( LinearRegression (..)
  , Options (..)
  , defaultOptions
  , fit
  , predict
  , score
  , mse
  , checkShapes
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
-- ANCHOR_END: head

-- ANCHOR: model
-- 📈 A learned line: y = X · coef + intercept.
-- 学んだ直線
data LinearRegression = LinearRegression
  { coef      :: NDArray   -- one weight per feature / 特徴ごとの重み
  , intercept :: Double    -- the y when every x is 0 / x が全部 0 のときの y
  }
  deriving (Show)

-- ⚙️ How to learn: the step size and how many steps.
-- 学び方：1歩の大きさと、何歩進むか
data Options = Options
  { learningRate :: Double
  , epochs       :: Int
  }

defaultOptions :: Options
defaultOptions = Options { learningRate = 0.01, epochs = 2000 }
-- ANCHOR_END: model

-- ANCHOR: predict
-- 🔮 Predict y for each row of X.
-- X の行ごとに、y を予想する
predict :: LinearRegression -> NDArray -> NDArray
predict m x = Np.dot x (coef m) + Np.scalar (intercept m)

-- 📏 Mean squared error: how far, on average, the guesses are.
-- 平均二乗誤差：予想が、平均でどれだけずれているか
mse :: NDArray -> NDArray -> Double
mse y guess = Np.item (Np.mean ((guess - y) * (guess - y)))
-- ANCHOR_END: predict

-- ANCHOR: fit
-- 🏋️ Learn coef and intercept from X (n rows) and y (n numbers):
--    start from 0, then walk down the slope of the error, step by step.
-- X（n 行）と y（n 個）から学ぶ：0 から始めて、誤差の坂を
-- 1歩ずつ下りていく
fit :: Options -> NDArray -> NDArray -> LinearRegression
fit opts x y =
  let features = checkShapes "fit" opts x y
      n        = fromIntegral (Np.nrows x)
      start    = LinearRegression (Np.zeros [features]) 0
      step m =
        let err   = predict m x - y                  -- shape [n]
            -- The slope of the error for each weight, and for the intercept.
            -- 重みごとの誤差の傾きと、切片の傾き
            gradW = Np.dot (Np.transpose x) err / Np.scalar n
            gradB = Np.item (Np.mean err)
            lr    = learningRate opts
        in LinearRegression (coef m - Np.scalar lr * gradW)
                            (intercept m - lr * gradB)
  -- Check the shapes first (seq), then walk.
  -- 先に形を確かめて（seq）から歩く
  in features `seq` (iterate step start !! epochs opts)

-- 🚧 X must be 2-D (rows, features), y 1-D (one number per row), and
--    the steps 0 or more. Gives the number of features.
--    (A y of shape [n, 1] would broadcast into [n, n] and learn wrong.)
-- X は2次元（行、特徴）、y は1次元（行ごとに1つ）、歩数は 0 以上。
-- 特徴の数を返す（y が [n, 1] だと、ブロードキャストで [n, n] になり、
-- 黙ってまちがって学ぶ）
checkShapes :: String -> Options -> NDArray -> NDArray -> Int
checkShapes name opts x y = case (Np.shape x, Np.shape y) of
  ([rows, features], [rows'])
    | rows == rows' && epochs opts >= 0 -> features
  (sx, sy) -> error (name <> ": X must be 2-D (rows, features), y 1-D"
                     <> " (rows), epochs 0 or more; got X " <> show sx
                     <> ", y " <> show sy <> ", epochs "
                     <> show (epochs opts))
-- ANCHOR_END: fit

-- ANCHOR: score
-- 🏅 R², like sklearn's score: 1 is perfect, 0 is no better than
--    always guessing the mean. (If every y is the same, it is
--    -Infinity or NaN: there is nothing to explain.)
-- 決定係数 R²。1 なら完ぺき、0 なら平均を言い続けるのと同じ
-- （y が全部同じだと、説明するものがないので -Infinity か NaN）
score :: LinearRegression -> NDArray -> NDArray -> Double
score m x y =
  let guess    = predict m x
      meanY    = Np.mean y
      residual = Np.item (Np.sum ((y - guess) * (y - guess)))
      total    = Np.item (Np.sum ((y - meanY) * (y - meanY)))
  in 1 - residual / total
-- ANCHOR_END: score
-- ANCHOR_END: all
