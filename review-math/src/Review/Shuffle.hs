-- ANCHOR: all
-- | A fair lottery: two ways to shuffle, and how to tell which is fair.
-- 公平なくじ引き：2つのシャッフルと、どちらが公平かの見分け方
module Review.Shuffle
  ( swapAt
  , naiveWith
  , fisherYatesWith
  , naiveChoices
  , fisherYatesChoices
  , exactCounts
  , Tally (..)
  , tallyOf
  , simulate
  , randomChoices
  ) where

import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import System.Random (StdGen, mkStdGen, uniformR)

-- ANCHOR: swap
-- 🔀 Swap the items at positions i and j.
-- i 番目と j 番目を入れかえる
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs =
  let s  = Seq.fromList xs
      si = Seq.index s i
      sj = Seq.index s j
  in  toList (Seq.update j si (Seq.update i sj s))
-- ANCHOR_END: swap

-- ANCHOR: shuffles
-- 🙈 The naive shuffle: for every position i (0, 1, ..., n-1),
--    swap it with ANY position. The choices say which.
-- 素朴なシャッフル：どの位置 i も、どこかの位置と入れかえる
naiveWith :: [Int] -> [a] -> [a]
naiveWith choices xs =
  foldl (\ys (i, j) -> swapAt i j ys) xs (zip [0 ..] choices)

-- 🃏 Fisher-Yates: for i = n-1 down to 1, swap position i with a
--    position from 0 to i (never one already settled).
-- フィッシャー－イェーツ：i = n-1 から 1 まで、0〜i の位置と入れかえる
-- （もう決まった位置とは入れかえない）
fisherYatesWith :: [Int] -> [a] -> [a]
fisherYatesWith choices xs =
  foldl (\ys (i, j) -> swapAt i j ys) xs
        (zip [length xs - 1, length xs - 2 .. 1] choices)
-- ANCHOR_END: shuffles

-- ANCHOR: exact
-- 🎲 All the choice lists each shuffle can make, equally likely.
--    The naive one: n choices of n each (n^n lists).
--    Fisher-Yates: n, n-1, ..., 2 choices (n! lists).
-- それぞれのシャッフルが作れる「選び方」の全部（どれも同じ確率）
naiveChoices :: Int -> [[Int]]
naiveChoices n = replicateM n [0 .. n - 1]

fisherYatesChoices :: Int -> [[Int]]
fisherYatesChoices n = sequence [ [0 .. i] | i <- [n - 1, n - 2 .. 1] ]

-- 📊 For every choice list, shuffle [1..n], and count the results.
-- 選び方ごとに [1..n] をシャッフルして、結果を数える
exactCounts :: ([Int] -> [Int] -> [Int]) -> [[Int]] -> Int
            -> Map [Int] Int
exactCounts shuffle choiceLists n =
  Map.fromListWith (+) [ (shuffle cs [1 .. n], 1) | cs <- choiceLists ]
-- ANCHOR_END: exact

-- ANCHOR: tally
-- 🧮 A tally of results that can be combined (a monoid), so that many
--    batches of trials can be counted separately and then merged.
-- 合わせられる結果の集計（モノイド）。何回分もの試行を別々に数えて合わせる
newtype Tally = Tally (Map [Int] Int)
  deriving (Show, Eq)

instance Semigroup Tally where
  Tally a <> Tally b = Tally (Map.unionWith (+) a b)

instance Monoid Tally where
  mempty = Tally Map.empty

tallyOf :: [Int] -> Tally
tallyOf result = Tally (Map.singleton result 1)
-- ANCHOR_END: tally

-- ANCHOR: simulate
-- 🎰 Random choices for a shuffle of n items: for the naive shuffle,
--    each choice is 0..n-1; for Fisher-Yates, 0..i.
-- n 個のシャッフルのための、でたらめな選び方
randomChoices :: [Int] -> StdGen -> ([Int], StdGen)
randomChoices []           gen = ([], gen)
randomChoices (top : rest) gen =
  let (c, gen1)  = uniformR (0, top) gen
      (cs, gen2) = randomChoices rest gen1
  in  (c : cs, gen2)

-- 🔁 Shuffle [1..n] many times with random choices (Monte Carlo).
--    `tops` gives the largest choice at each step.
-- ランダムな選び方で、何度もシャッフルする（モンテカルロ法）
simulate :: ([Int] -> [Int] -> [Int]) -> [Int] -> Int -> Int -> Int -> Tally
simulate shuffle tops n trials seed = go trials (mkStdGen seed) mempty
  where
    go 0 _   acc = acc
    go k gen acc =
      let (cs, gen') = randomChoices tops gen
      in  go (k - 1) gen' (acc <> tallyOf (shuffle cs [1 .. n]))
-- ANCHOR_END: simulate
-- ANCHOR_END: all
