-- ANCHOR: head
module Main (main) where

import Control.Exception (evaluate)
import Test.Hspec

import Answers
import qualified MiniNumpy as Np
import MiniSklearn.Datasets
import qualified MiniSklearn.Linear as Lin
import qualified MiniSklearn.Logistic as Logi

-- 🎯 Close enough (for numbers learned step by step).
-- 十分に近い（少しずつ学んだ数のため）
near :: Double -> Double -> Double -> Bool
near tolerance a b = abs (a - b) < tolerance
-- ANCHOR_END: head

-- ANCHOR: setup
main :: IO ()
main = hspec $ do
  let (x, y)   = potionPrices
      (mx, my) = monsters
-- ANCHOR_END: setup
  -- ANCHOR: linear
  -- 「線形回帰」
  describe "LinearRegression" $ do
    -- 「2000 歩で、3 と 5 の近くまで学ぶ」
    it "learns a line close to 3 * size + 5" $ do
      let m = Lin.fit Lin.defaultOptions x y
      Np.toList (Lin.coef m) `shouldBe` [3.0055619234664337]
      Lin.intercept m `shouldBe` 4.984694819027008
      Lin.score m x y `shouldBe` 0.9933832554635232

    -- 「外れ（MSE）が、464 から 0.5 に減る」
    it "makes the mean squared error small" $ do
      let m0 = Lin.LinearRegression (Np.zeros [1]) 0
          m  = Lin.fit Lin.defaultOptions x y
      Lin.mse y (Lin.predict m0 x) `shouldBe` 464.37447124801724
      Lin.mse y (Lin.predict m x) `shouldBe` 0.49974879417475565

    -- 「歩数を増やすと、scikit-learn と同じ答えになる」
    it "reaches the same answer as scikit-learn with more steps" $ do
      let m = Lin.fit (Lin.Options 0.01 20000) x y
      Np.item (Lin.coef m) `shouldSatisfy` near 1e-9 3.0011674539696
      Lin.intercept m `shouldSatisfy` near 1e-9 5.013472539723

    -- 「X や y の形がちがえば、止まる」
    it "stops when X or y has the wrong shape" $ do
      let y2 = Np.reshape [50, 1] y
      evaluate (Lin.fit Lin.defaultOptions x y2) `shouldThrow` anyErrorCall
      evaluate (Lin.fit Lin.defaultOptions (Np.reshape [50] x) y)
        `shouldThrow` anyErrorCall
  -- ANCHOR_END: linear

  -- ANCHOR: logistic
  -- 「ロジスティック回帰」
  describe "LogisticRegression" $
    -- 「危ないモンスターを、97% 見分ける」
    it "tells dangerous monsters apart" $ do
      let m = Logi.fit (Lin.Options 0.1 3000) mx my
      Logi.score m mx my `shouldBe` 0.97
      let probs = Logi.predictProba m (Np.array2 [[2, 3], [8, 9], [7, 6]])
      map (> 0.5) (Np.toList probs)
        `shouldBe` [False, True, True]
  -- ANCHOR_END: logistic

  -- 「トレーニング」
  -- ANCHOR: answers
  describe "answers" $ do
    -- 「式で一気に、scikit-learn の直線」
    it "fitClosed gives scikit-learn's line at once" $ do
      let m = fitClosed x y
      Np.item (Lin.coef m) `shouldSatisfy` near 1e-9 3.0011674539696
      Lin.intercept m `shouldSatisfy` near 1e-9 5.013472539723

    -- 「列ごとに、平均 0・ばらつき 1」
    it "standardize gives mean 0 and spread 1" $ do
      let s = standardize mx
          n = fromIntegral (Np.nrows s)
      map (near 1e-9 0) (Np.toList (Np.sumAxis 0 s)) `shouldBe` [True, True]
      map (near 1e-9 1) (Np.toList (Np.sumAxis 0 (s * s) / n))
        `shouldBe` [True, True]

    -- 「新しいデータは、学習用の平均とばらつきで変換する」
    it "transforms new data with the learned scaler" $ do
      let sc = fitScaler (Np.array2 [[0], [2]])
      Np.toList (transform sc (Np.array2 [[4], [6]])) `shouldBe` [3, 5]
      Np.toList (standardize (Np.array2 [[1, 5], [3, 5]]))
        `shouldBe` [-1, 0, 1, 0]

    -- 「5行ごとに1行をテスト用に。どの行も、ちょうど1回」
    it "splitEvery keeps every row once" $ do
      let (tx, ty, sx, sy) = splitEvery 5 mx my
          rowsOf a = [ Np.toList (Np.row i a) | i <- [0 .. Np.nrows a - 1] ]
          everyRow = rowsOf mx
          ys       = Np.toList my
      rowsOf sx `shouldBe` [ r | (i, r) <- zip [0 :: Int ..] everyRow
                               , i `mod` 5 == 0 ]
      rowsOf tx `shouldBe` [ r | (i, r) <- zip [0 :: Int ..] everyRow
                               , i `mod` 5 /= 0 ]
      Np.toList sy `shouldBe` [ v | (i, v) <- zip [0 :: Int ..] ys
                                  , i `mod` 5 == 0 ]
      Np.toList ty `shouldBe` [ v | (i, v) <- zip [0 :: Int ..] ys
                                  , i `mod` 5 /= 0 ]
  -- ANCHOR_END: answers
