-- ANCHOR: all
-- | Train the digit network for 100 steps with SGD, momentum, and Adam.
--   cabal run review8-optim
-- 数字のネットワークを、SGD・Momentum・Adam で 100 歩ずつ学ばせる
module Main (main) where

import Control.Monad (forM_)
import Text.Printf (printf)

import MiniTorch.Digits (makeDigits)
import MiniTorch.NN (accuracy, initParams)
import Review.Optim

main :: IO ()
main = do
  let (xTrain, yTrain) = makeDigits 1 0.1 1000
      (xTest, yTest)   = makeDigits 2 0.1 200
      start = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
      runs  =
        [ ("SGD     ", sgd 0.5)
        , ("Momentum", momentum 0.1 0.9)
        , ("Adam    ", adam defaultAdam { learningRate = 0.01 })
        ]
  forM_ runs $ \(name, opt) -> do
    let (losses, final) = trainWith opt 101 start xTrain yTrain
        at k = losses !! k
    printf "%s  loss %.4f %.4f %.4f %.4f %.4f  test %.3f\n"
      (name :: String) (at 0) (at 25) (at 50) (at 75) (at 100)
      (accuracy final xTest yTest)
-- ANCHOR_END: all
