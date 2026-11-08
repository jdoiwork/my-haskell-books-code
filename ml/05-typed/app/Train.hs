-- ANCHOR: all
-- | Train the network on the made-up digits, and show how it learns.
--   cabal run train-digits
-- 作りものの数字で、ネットワークを学ばせ、学んでいく様子を見せる。
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

import qualified MiniNumpy as Np
import MiniTorch.Digits
import MiniTorch.NN
import MiniTorch.Tensor

main :: IO ()
main = do
  let (xTrain, yTrain) = makeDigits 1 0.1 1000
      (xTest, yTest)   = makeDigits 2 0.1 200
      start = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
  t0 <- getMonotonicTime
  -- ANCHOR: loop
  -- 🔁 The training loop: forward, loss, backward, step.
  -- 学習のループ：前向き、損失、backward、1歩
  let train ps =
        let loss  = crossEntropy (mlp ps xTrain) yTrain
            grads = backward loss
        in (Np.item (tvalue loss), sgdStep 0.5 ps grads)
      go ps epoch
        | epoch > 300 = pure ps
        | otherwise = do
            let (l, ps') = train ps
            when (epoch `mod` 50 == 0) $
              printf "epoch %3d  loss %.4f\n" (epoch :: Int) l
            go ps' (epoch + 1)
  final <- go start 0
  -- ANCHOR_END: loop
  -- Compute the last step now, inside the timing (it is lazy).
  -- 最後の1歩も、ここで計算させて測る（遅延評価なので）
  mapM_ (evaluate . Np.item . Np.sum) (Map.elems final)
  t1 <- getMonotonicTime
  printf "train accuracy %.3f, test accuracy %.3f (%.1f s)\n"
    (accuracy final xTrain yTrain) (accuracy final xTest yTest) (t1 - t0)
  -- Show three test digits and what the network says.
  -- テストの数字を3つ、ネットワークの答えと一緒に見せる
  forM_ [3, 7, 9] $ \i -> do
    putStr (showDigit (Np.toList (Np.row i xTest)))
    let one   = Np.reshape [1, 35] (Np.row i xTest)
        guess = Np.argmaxAxis1 (tvalue (mlp final one))
    printf "-> %s (answer %d)\n\n" (show guess) (yTest !! i)
-- ANCHOR_END: all
