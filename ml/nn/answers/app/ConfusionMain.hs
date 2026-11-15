-- ANCHOR: all
-- | The answer to step 3: train as train-digits does (epochs 0 to 300, 301
--   steps), and print the confusion table of the 200 test digits.
--   cabal run confusion-digits
-- ステップ3の解答：train-digits と同じく（0 歩目から 300 歩目までの 301 回）
-- 学ばせて、テストの 200 個の混同行列を見せる
module Main (main) where

import MiniTorch.Digits
import MiniTorch.NN
import MiniTorch.Tensor

import Answers (confusion)

main :: IO ()
main = do
  let (xTrain, yTrain) = makeDigits 1 0.1 1000
      (xTest, yTest)   = makeDigits 2 0.1 200
      start = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
      -- 🔁 The same loop as train-digits, without the printing.
      -- train-digits と同じループ（表示はしない）
      step ps =
        let loss = crossEntropy (mlp ps xTrain) yTrain
        in sgdStep 0.5 ps (backward loss)
      -- train-digits's go learns at epochs 0, 1, ..., 300: 301 steps.
      -- train-digits の go は、epoch 0, 1, …, 300 で学ぶ：301 回
      lastEpoch = 300
      final = iterate step start !! (lastEpoch + 1)
  mapM_ print (confusion final xTest yTest)
-- ANCHOR_END: all
