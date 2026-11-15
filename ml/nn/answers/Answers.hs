-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the neural network chapter.
-- ニューラルネットワークの章のトレーニングの解答。
module Answers
  ( mlp3
  , accuracyWith
  , batches
  , trainEpoch
  , confusion
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.NN
import MiniTorch.Tensor
-- ANCHOR_END: head

-- ANCHOR: mlp3
-- 🧠 Three layers: Linear -> ReLU -> Linear -> ReLU -> Linear.
-- 3層：Linear -> ReLU -> Linear -> ReLU -> Linear
mlp3 :: Params -> NDArray -> Tensor
mlp3 ps x =
  let hidden1 = relu (linear "fc1" ps (constant x))
      hidden2 = relu (linear "fc2" ps hidden1)
  in  linear "fc3" ps hidden2

-- 🏅 accuracy, for any model (accuracy itself always uses mlp).
-- どのモデルでも測れる accuracy（accuracy は、いつも mlp を使う）
accuracyWith :: (Params -> NDArray -> Tensor)
             -> Params -> NDArray -> [Int] -> Double
accuracyWith model ps x labels =
  let guesses = Np.argmaxAxis1 (tvalue (model ps x))
      right   = length (filter id (zipWith (==) guesses labels))
  in fromIntegral right / fromIntegral (length labels)
-- ANCHOR_END: mlp3

-- ANCHOR: batches
-- 📦 Cut the data into mini-batches of `size` rows (like DataLoader).
-- データを size 行ずつの小さな束（ミニバッチ）に切る
batches :: Int -> NDArray -> [Int] -> [(NDArray, [Int])]
batches size x labels
  | size <= 0 = error "batches: the size must be 1 or more"
  | otherwise =
      [ ( Np.array2 [ Np.toList (Np.row i x) | i <- is ]
        , [ labels !! i | i <- is ] )
      | start <- [0, size .. Np.nrows x - 1]
      , let is = [start .. min (Np.nrows x) (start + size) - 1] ]

-- 🔁 One epoch: one sgdStep per batch, left to right (foldl).
-- 1周：束ごとに sgdStep を1歩ずつ、左から順に（foldl）
trainEpoch :: Double -> Params -> [(NDArray, [Int])] -> Params
trainEpoch lr = foldl step
  where
    step ps (x, ys) = sgdStep lr ps (backward (crossEntropy (mlp ps x) ys))
-- ANCHOR_END: batches

-- ANCHOR: confusion
-- 🤔 A 10 x 10 table: row = the answer, column = the guess.
-- 10 × 10 の表：行が正解、列が予想
confusion :: Params -> NDArray -> [Int] -> [[Int]]
confusion ps x labels =
  let guesses = Np.argmaxAxis1 (tvalue (mlp ps x))
      pairs   = zip labels guesses
  in [ [ length (filter (== (a, g)) pairs) | g <- [0 .. 9] ] | a <- [0 .. 9] ]
-- ANCHOR_END: confusion
-- ANCHOR_END: all
