-- ANCHOR: all
-- | Counting and chance: parties, dice, gacha, and birthdays.
-- 数え上げと確率：パーティー、サイコロ、ガチャ、誕生日
module Math.Counting
  ( factorial
  , arrangements
  , combinations
  , teams
  , twoDice
  , probability
  , atLeastOnce
  , pullsUntilRare
  , averagePulls
  , sameBirthday
  ) where

import Data.Ratio ((%))
import System.Random (StdGen, mkStdGen, uniformR)

-- ANCHOR: count
-- 🔢 n! = n * (n - 1) * ... * 1: ways to line up n heroes.
-- n 人を一列に並べる並べ方
factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- 🚶 Ways to line up k of n heroes (order matters): n! / (n - k)!.
-- n 人から k 人を選んで並べる（順番が大事）
arrangements :: Integer -> Integer -> Integer
arrangements n k = product [n - k + 1 .. n]

-- 👥 Ways to pick k of n heroes (order does not matter).
-- n 人から k 人を選ぶ（順番は気にしない）
combinations :: Integer -> Integer -> Integer
combinations n k = arrangements n k `div` factorial k
-- ANCHOR_END: count

-- ANCHOR: teams
-- 🧑‍🤝‍🧑 All the teams of k members.
-- k 人のチームを、全部
teams :: Int -> [a] -> [[a]]
teams 0 _        = [[]]
teams _ []       = []
teams k (x : xs) = map (x :) (teams (k - 1) xs) ++ teams k xs
-- ANCHOR_END: teams

-- ANCHOR: dice
-- 🎲 All 36 outcomes of two dice, each as likely as any other.
-- サイコロ2つの、36 通りの出方（どれも同じくらい出やすい）
twoDice :: [(Int, Int)]
twoDice = [ (a, b) | a <- [1 .. 6], b <- [1 .. 6] ]

-- 🎯 The chance of an event, when every outcome is equally likely:
--    (outcomes where it happens) / (all outcomes), as an exact fraction.
-- どれも同じくらい出やすいときの確率：（起きる場合の数）/（全部の数）
probability :: (a -> Bool) -> [a] -> Rational
probability event outcomes =
  fromIntegral (length (filter event outcomes))
    % fromIntegral (length outcomes)
-- ANCHOR_END: dice

-- ANCHOR: gacha
-- 🎰 The chance of at least one rare in n pulls: 1 - (1 - p) ^ n.
--    (The opposite of "no rare at all".)
-- n 回引いて、レアが1回以上出る確率（「1回も出ない」の反対）
atLeastOnce :: Double -> Int -> Double
atLeastOnce p n = 1 - (1 - p) ^ n
-- ANCHOR_END: gacha

-- ANCHOR: monte
-- 🎰 Pull until a rare comes; how many pulls did it take?
-- レアが出るまで引く。何回かかった？
pullsUntilRare :: Double -> StdGen -> (Int, StdGen)
pullsUntilRare p = go 1
  where
    go count gen =
      let (u, gen') = uniformR (0, 1) gen
      in  if u < p then (count, gen') else go (count + 1) gen'

-- 📊 Do it many times and take the average (the Monte Carlo method).
-- 何度もやって、平均をとる（モンテカルロ法）
averagePulls :: Double -> Int -> Int -> Double
averagePulls p trials seed = go trials (mkStdGen seed) 0
  where
    go :: Int -> StdGen -> Int -> Double
    go 0 _   total = fromIntegral total / fromIntegral trials
    go k gen total =
      let (pulls, gen') = pullsUntilRare p gen
      in  go (k - 1) gen' (total + pulls)
-- ANCHOR_END: monte

-- ANCHOR: birthday
-- 🎂 The chance that some two of n people share a birthday (365 days):
--    1 - (365/365) * (364/365) * ... * ((365 - n + 1)/365).
-- n 人の中に、誕生日が同じ2人がいる確率
sameBirthday :: Int -> Int -> Double
sameBirthday days n =
  let d          = fromIntegral days
      allDiffer  = product [ (d - fromIntegral k) / d | k <- [0 .. n - 1] ]
  in  1 - allDiffer
-- ANCHOR_END: birthday
-- ANCHOR_END: all
