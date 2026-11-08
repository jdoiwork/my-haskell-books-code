-- ANCHOR: all
-- | Ratings that cannot go out of range: stars and scores.
-- 範囲を出られない評価：★と点
-- ANCHOR: header
module Tsundoku.Rating
  ( Stars          -- no constructor: use `stars` / コンストラクターは出さない
  , stars
  , starsValue
  , allStars
  , Score          -- no constructor: use `score` / コンストラクターは出さない
  , score
  , scoreValue
  , starsToScore
  , scoreToStars
  , Rating (..)
  , ratingScore
  ) where

import Data.Ord (clamp)

import Tsundoku.Types (Mistake (..))
-- ANCHOR_END: header

-- ANCHOR: types
-- ⭐ Stars from 1 to 5, by halves. Kept as the number of half stars
--    (2 to 10), so 3.5 stars is exactly 7, with no rounding error.
-- 1〜5 の★（0.5刻み）。半分の★がいくつかで持つ（2〜10）。
-- ★3.5 はちょうど 7 で、丸めの誤差がない
newtype Stars = Stars { halves :: Int }
  deriving (Show, Eq, Ord)

-- 💯 A score from 0 to 100.
-- 0〜100 の点
newtype Score = Score Int
  deriving (Show, Eq, Ord)
-- ANCHOR_END: types

-- ANCHOR: smart
-- 🚪 The only way in: 1 to 5, by halves. Anything else is a mistake.
-- ★を作る、ただ1つの入り口：1〜5 で、0.5刻み。それ以外はまちがい
stars :: Double -> Either Mistake Stars
stars x
  | x < 1 || x > 5               = Left (BadStars x)
  | doubled /= fromIntegral whole = Left (BadStars x)
  | otherwise                     = Right (Stars whole)
  where
    doubled = x * 2
    whole   = round doubled :: Int

-- 🚪 The only way in for scores: 0 to 100.
-- 点を作る、ただ1つの入り口：0〜100
score :: Int -> Either Mistake Score
score n
  | n < 0 || n > 100 = Left (BadScore n)
  | otherwise        = Right (Score n)

-- 👀 Look inside (reading is fine; only making is guarded).
-- 中身を見る（読むのはよい。守るのは作るときだけ）
starsValue :: Stars -> Double
starsValue (Stars h) = fromIntegral h / 2

scoreValue :: Score -> Int
scoreValue (Score n) = n

-- 📋 Every possible number of stars, from 1 to 5.
-- ありうる★の全部（1〜5）
allStars :: [Stars]
allStars = map Stars [2 .. 10]
-- ANCHOR_END: smart

-- ANCHOR: convert
-- 🔁 Stars to a score: 1 star is 20 points, 5 stars are 100.
--    Every star count has its own score, so nothing is lost.
-- ★を点に：★1 が 20 点、★5 が 100 点。★ごとに別の点になるので、何も失わない
starsToScore :: Stars -> Score
starsToScore (Stars h) = Score (h * 10)

-- 🔁 A score to stars: to the nearest half star, rounding halves up
--    (25 points is 1.5 stars), and never below 1 star.
-- 点を★に：いちばん近い半分の★へ。ちょうど真ん中は切り上げ
-- （25 点は★1.5）。★1 より下にはしない
scoreToStars :: Score -> Stars
scoreToStars (Score n) = Stars (clamp (2, 10) ((n + 5) `div` 10))
-- ANCHOR_END: convert

-- ANCHOR: rating
-- 🗳️ A rating, given in stars or in points.
-- 評価。★で付けても、点で付けてもよい
data Rating
  = ByStars Stars
  | ByScore Score
  deriving (Show, Eq)

-- 💯 Any rating as a score, so that both kinds can be compared.
-- どちらの評価も点にそろえる（比べられるように）
ratingScore :: Rating -> Score
ratingScore (ByStars s) = starsToScore s
ratingScore (ByScore p) = p
-- ANCHOR_END: rating
-- ANCHOR_END: all
