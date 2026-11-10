-- ANCHOR: all
-- | The answer to step 3: train as train-digits does (300 steps), and print
--   the confusion table of the 200 test digits.
--   cabal run confusion-digits
-- ステップ3の解答：train-digits と同じく 300 歩学ばせて、テストの 200 個の
-- 混同行列を見せる
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
      final = iterate step start !! 301
  mapM_ print (confusion final xTest yTest)
-- ANCHOR_END: all
