-- ANCHOR: all
-- | Answers to the training of "Counting and probability".
-- 「数え上げと確率」のトレーニングの解答
module CountingAnswers
  ( pullsFor
  , birthdayTrials
  ) where

import Data.List (nub)
import System.Random (StdGen, mkStdGen, uniformR)

import Math.Counting

-- ANCHOR: pullsFor
-- 🎰 Step 2: the fewest pulls for at least one rare with chance `goal`.
-- ステップ2：レアが1回以上出る確率が goal 以上になる、いちばん少ない回数
pullsFor :: Double -> Double -> Int
pullsFor p goal =
  length (takeWhile (< goal) [ atLeastOnce p n | n <- [0 ..] ])
-- ANCHOR_END: pullsFor

-- ANCHOR: birthdayTrials
-- 🎂 Step 3: gather n people with random birthdays, many times;
--    the share of gatherings where two people share a birthday.
-- ステップ3：誕生日がでたらめな n 人を何度も集め、同じ誕生日の2人がいた割合
birthdayTrials :: Int -> Int -> Int -> Double
birthdayTrials n trials seed = go trials (mkStdGen seed) 0
  where
    go :: Int -> StdGen -> Int -> Double
    go 0 _   hits = fromIntegral hits / fromIntegral trials
    go k gen hits =
      let (days, gen') = birthdays n gen
          shared       = length (nub days) < n
      in  go (k - 1) gen' (if shared then hits + 1 else hits)

-- 🎂 n random days in 1..365.
-- 1〜365 の、でたらめな日を n 個
birthdays :: Int -> StdGen -> ([Int], StdGen)
birthdays 0 gen = ([], gen)
birthdays n gen =
  let (day, gen')   = uniformR (1, 365) gen
      (rest, gen'') = birthdays (n - 1) gen'
  in  (day : rest, gen'')
-- ANCHOR_END: birthdayTrials
-- ANCHOR_END: all
