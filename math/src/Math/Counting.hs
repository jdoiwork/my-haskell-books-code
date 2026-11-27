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

-- ANCHOR: imports
import Data.Ratio ((%))
import System.Random (StdGen, mkStdGen, uniformR)
-- ANCHOR_END: imports

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
--    There is no way to pick fewer than 0, or more than n.
-- n 人から k 人を選ぶ（順番は気にしない）。
-- 0 人より少なく、n 人より多くは選べないので、0 通り
combinations :: Integer -> Integer -> Integer
combinations n k
  | k < 0 || k > n = 0
  | otherwise      = arrangements n k `div` factorial k
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
--    There must be at least one outcome (else it divides by 0).
-- どれも同じくらい出やすいときの確率：（起きる場合の数）/（全部の数）。
-- 出方は1つ以上あること（なければ 0 で割ってしまう）
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
--    p must be more than 0 (or no rare ever comes).
-- レアが出るまで引く。何回かかった？ p は 0 より大きいこと
-- （0 だと、レアはいつまでも出ない）
pullsUntilRare :: Double -> StdGen -> (Int, StdGen)
pullsUntilRare p = go 1
  where
    go !count gen =
      let (u, gen') = uniformR (0, 1) gen
      in  if u < p then (count, gen') else go (count + 1) gen'

-- 📊 Do it many times and take the average (the Monte Carlo method).
--    trials must be 1 or more. The ! work out the total and the generator
--    every turn, so that unfinished work does not pile up.
-- 何度もやって、平均をとる（モンテカルロ法）。trials は 1 以上。
-- ! で、合計と乱数の生成器を毎回計算しておき、計算の約束をためない
averagePulls :: Double -> Int -> Int -> Double
averagePulls p trials seed = go trials (mkStdGen seed) 0
  where
    go :: Int -> StdGen -> Int -> Double
    go 0 _    total = fromIntegral total / fromIntegral trials
    go k !gen !total =
      let (pulls, gen') = pullsUntilRare p gen
      in  go (k - 1) gen' (total + pulls)
-- ANCHOR_END: monte

-- ANCHOR: birthday
-- 🎂 The chance that some two of n people share a birthday (365 days):
--    1 - (365/365) * (364/365) * ... * ((365 - n + 1)/365).
--    Every day is equally likely, and the people are independent.
-- n 人の中に、誕生日が同じ2人がいる確率。どの日も同じくらいで、
-- 1人1人の誕生日は、ほかの人と関係なく決まるとする
sameBirthday :: Int -> Int -> Double
sameBirthday days n =
  let d          = fromIntegral days
      allDiffer  = product [ (d - fromIntegral k) / d | k <- [0 .. n - 1] ]
  in  1 - allDiffer
-- ANCHOR_END: birthday
-- ANCHOR_END: all
