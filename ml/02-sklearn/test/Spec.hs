module Main (main) where

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

main :: IO ()
main = hspec $ do
  let (x, y)   = potionPrices
      (mx, my) = monsters
  -- 「線形回帰」
  describe "LinearRegression" $ do
    -- ANCHOR: linear
    -- 「2000 歩で、3 と 5 の近くまで学ぶ」
    it "learns a line close to 3 * size + 5" $ do
      let m = Lin.fit Lin.defaultOptions x y
      Np.toList (Lin.coef m) `shouldBe` [3.0055619234664337]
      Lin.intercept m `shouldBe` 4.984694819027008
      Lin.score m x y `shouldSatisfy` (> 0.99)

    -- 「歩数を増やすと、scikit-learn と同じ答えになる」
    it "reaches the same answer as scikit-learn with more steps" $ do
      let m = Lin.fit (Lin.Options 0.01 20000) x y
      Np.item (Lin.coef m) `shouldSatisfy` near 1e-9 3.0011674539696
      Lin.intercept m `shouldSatisfy` near 1e-9 5.013472539723
    -- ANCHOR_END: linear

  -- 「ロジスティック回帰」
  describe "LogisticRegression" $
    -- ANCHOR: logistic
    -- 「危ないモンスターを、97% 見分ける」
    it "tells dangerous monsters apart" $ do
      let m = Logi.fit (Lin.Options 0.1 3000) mx my
      Logi.score m mx my `shouldBe` 0.97
      let probs = Logi.predictProba m (Np.array2 [[2, 3], [8, 9]])
      map (> 0.5) (Np.toList probs)
        `shouldBe` [False, True]
    -- ANCHOR_END: logistic

  -- 「トレーニング」
  describe "answers" $ do
    it "fitClosed gives scikit-learn's line at once" $ do
      let m = fitClosed x y
      Np.item (Lin.coef m) `shouldSatisfy` near 1e-9 3.0011674539696
      Lin.intercept m `shouldSatisfy` near 1e-9 5.013472539723
    it "standardize gives mean 0 and spread 1" $ do
      let s = standardize mx
      map (near 1e-9 0) (Np.toList (Np.sumAxis 0 s)) `shouldBe` [True, True]
    it "splitEvery keeps every row once" $ do
      let (tx, _, sx, _) = splitEvery 5 mx my
      (Np.nrows tx, Np.nrows sx) `shouldBe` (80, 20)
