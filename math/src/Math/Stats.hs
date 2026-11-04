-- ANCHOR: all
-- | Statistics: one number for many numbers, and where it fools us.
-- 統計：たくさんの数を1つの数で。そして、だまされるところ
module Math.Stats
  ( mean
  , median
  , variance
  , stdDev
  , percentile
  , histogram
  , showHistogram
  , correlation
  , memberGold
  , latencies
  , Trip (..)
  , trips
  ) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import System.Random (mkStdGen, uniformR)

-- ANCHOR: center
-- ⚖️ The mean: add them up, divide by how many.
-- 平均：全部足して、個数で割る
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- 🎯 The median: line them up, take the middle one
--    (the mean of the two middle ones when the count is even).
-- 中央値：並べて、真ん中を取る（偶数個なら、真ん中2つの平均）
median :: [Double] -> Double
median xs =
  let sorted = sort xs
      n      = length xs
      middle = n `div` 2
  in  if odd n
        then sorted !! middle
        else (sorted !! (middle - 1) + sorted !! middle) / 2
-- ANCHOR_END: center

-- ANCHOR: spread
-- 📏 The variance: the mean of the squared distances from the mean.
-- 分散：平均からの距離の2乗の、平均
variance :: [Double] -> Double
variance xs =
  let m = mean xs
  in  mean [ (x - m) ^ (2 :: Int) | x <- xs ]

-- 📏 The standard deviation: the square root of the variance,
--    back in the same unit as the data.
-- 標準偏差：分散の平方根。データと同じ単位に戻る
stdDev :: [Double] -> Double
stdDev xs = sqrt (variance xs)
-- ANCHOR_END: spread

-- ANCHOR: percentile
-- 📊 The p-th percentile (nearest rank): the smallest value that is
--    at least p% of the data. percentile 99 = "99% are this or less".
-- p パーセンタイル：データの p% が、その値以下になる、いちばん小さな値
percentile :: Double -> [Double] -> Double
percentile p xs =
  let sorted = sort xs
      n      = length xs
      rank   = ceiling (p / 100 * fromIntegral n) :: Int
  in  sorted !! max 0 (rank - 1)
-- ANCHOR_END: percentile

-- ANCHOR: histogram
-- 📶 Count how many values fall in each bucket of a given width.
-- 決まった幅の区間ごとに、いくつ入るかを数える
histogram :: Double -> [Double] -> Map.Map Int Int
histogram width xs =
  Map.fromListWith (+) [ (floor (x / width), 1) | x <- xs ]

-- 📶 Draw it with # marks, one # for every `per` values.
-- # で描く。per 個ごとに # を1つ
showHistogram :: Double -> Int -> Map.Map Int Int -> String
showHistogram width per counts =
  unlines
    [ pad 9 (show (fromIntegral bucket * width :: Double))
        <> " | " <> replicate (count `div` per) '#'
        <> " " <> show count
    | (bucket, count) <- Map.toList counts ]
  where
    pad k s = replicate (k - length s) ' ' <> s
-- ANCHOR_END: histogram

-- ANCHOR: correlation
-- 🔗 The correlation: +1 when two things rise together, -1 when one
--    falls as the other rises, near 0 when there is no straight link.
-- 相関係数：一緒に増えるなら +1、片方が増えると減るなら -1、
-- まっすぐな関係がなければ 0 に近い
correlation :: [Double] -> [Double] -> Double
correlation xs ys =
  let mx  = mean xs
      my  = mean ys
      cov = mean (zipWith (\x y -> (x - mx) * (y - my)) xs ys)
  in  cov / (stdDev xs * stdDev ys)
-- ANCHOR_END: correlation

-- ANCHOR: data
-- 💰 The gold of the guild's 11 members. One of them slew a dragon.
-- ギルドの 11 人の所持金。1人はドラゴンを倒した
memberGold :: [Double]
memberGold = [30, 45, 50, 52, 60, 65, 70, 80, 90, 120, 100000]

-- ⏱️ 10000 response times of the guild API, in milliseconds:
--    usually 15..25 ms, but 2% of them wait for the disk (200..800 ms).
-- ギルドの API の応答時間 1 万件（ミリ秒）。ふだんは 15〜25 ms、
-- でも 2% はディスクを待つ（200〜800 ms）
latencies :: [Double]
latencies = go (10000 :: Int) (mkStdGen 7)
  where
    go 0 _   = []
    go k gen =
      let (u, gen1)    = uniformR (0, 1 :: Double) gen
          (fast, gen2) = uniformR (15, 25) gen1
          (slow, gen3) = uniformR (200, 800) gen2
      in  (if u < 0.02 then slow else fast) : go (k - 1) gen3

-- 🗺️ A trip to the dungeon: how deep, potions bought, times defeated.
--    Deeper trips need more potions AND bring more defeats.
-- ダンジョンへの冒険：深さ、買ったポーション、負けた回数。
-- 深いほど、ポーションも、負けも増える
data Trip = Trip
  { depth    :: Double
  , potions  :: Double
  , defeats  :: Double
  }
  deriving (Show)

trips :: [Trip]
trips = go (500 :: Int) (mkStdGen 11)
  where
    go 0 _   = []
    go k gen =
      let (d, gen1)  = uniformR (1, 10 :: Int) gen
          (n1, gen2) = uniformR (-2, 2) gen1
          (n2, gen3) = uniformR (-2, 2) gen2
          dd         = fromIntegral d
      in  Trip dd (3 * dd + n1) (dd / 2 + n2) : go (k - 1) gen3
-- ANCHOR_END: data
-- ANCHOR_END: all
