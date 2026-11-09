-- ANCHOR: all
-- | Gathering ratings: tallies, averages that are not fooled by a
--   single ★5, and rankings. The pure core: numbers in, numbers out.
-- 評価を集める：件数と合計、★5 が1件だけでもだまされない平均、
-- ランキング。純粋な核：数を受け取って、数を返す
-- ANCHOR: header
module Tsundoku.Ranking
  ( -- * Counting / 数える
    Tally (..)
  , tally
  , mean
    -- * Not fooled by a few / 少ない件数にだまされない
  , Prior (..)
  , bayesian
  , usualPoints
  , shelfPrior
  , liked
  , wilsonLower
    -- * Rankings / ランキング
  , rankCards
  ) where

-- sorting, Maybe, and exact fractions
-- 並べ替え、Maybe、ぴったりの分数
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Ratio ((%))

import Tsundoku.Card
import Tsundoku.Rating (Score, scoreValue)
-- ANCHOR_END: header

-- ANCHOR: tally
-- 🧮 How many ratings, and their points added up. An average does
--    not add up, but a tally does.
-- 評価の件数と、点の合計。平均は足せないが、件数と合計なら足せる
data Tally = Tally
  { tallied :: Int    -- how many ratings / 件数
  , points  :: Int    -- all the points / 点の合計
  }
  deriving (Show, Eq)

instance Semigroup Tally where
  Tally n1 p1 <> Tally n2 p2 = Tally (n1 + n2) (p1 + p2)

instance Monoid Tally where
  mempty = Tally 0 0

-- 🧺 Tally a list of ratings: each rating is one, with its points.
-- 評価のリストを数える：評価1つが、1件とその点
tally :: [Score] -> Tally
tally = foldMap one
  where
    one s = Tally 1 (scoreValue s)

-- ➗ The plain average, as an exact fraction. No ratings, no average.
-- ふつうの平均（ぴったりの分数で）。評価がなければ、平均もない
mean :: Tally -> Maybe Rational
mean (Tally n p)
  | n <= 0    = Nothing
  | otherwise = Just (toInteger p % toInteger n)
-- ANCHOR_END: tally

-- ANCHOR: bayesian
-- 🧭 What I already believe before looking at a work: "a work I
--    rate usually gets `usual` points", worth `weight` ratings.
-- 作品を見る前から思っていること：「わたしの作品は、ふつう
-- usual 点くらい」。それを、weight 件ぶんの評価として混ぜる
data Prior = Prior
  { weight :: Int        -- how many ratings it is worth / 何件ぶんか
  , usual  :: Rational   -- the usual points / ふつうの点
  }
  deriving (Show, Eq)

-- ⚖️ The Bayesian average: mix in `weight` ratings of `usual` points,
--    then take the average. Few ratings stay near the usual points;
--    many ratings go to their own average.
-- ベイズ平均：usual 点の評価を weight 件ぶん混ぜてから、平均する。
-- 件数が少ないと、ふつうの点の近くにいる。多いと、自分の平均に近づく
bayesian :: Prior -> Tally -> Rational
bayesian (Prior c m) (Tally n p)
  | c' + n <= 0 = m
  | otherwise   = (fromIntegral c' * m + fromIntegral p)
                / fromIntegral (c' + n)
  where
    c' = max 0 c   -- a weight below 0 means nothing / 0 未満は 0 とみなす
-- ANCHOR_END: bayesian

-- ANCHOR: usual
-- 🗳️ The usual points of my shelf: the average of each work's own
--    average, one vote per work (a serial of 30 episodes does not get
--    30 votes). With nothing rated yet, the middle: 50.
-- わたしの本棚の「ふつうの点」：作品ごとの平均の、平均。1作品1票
-- （30話の連載が 30票を持たない）。まだ何も評価していなければ、真ん中の 50
usualPoints :: [Tally] -> Rational
usualPoints tallies = case mapMaybe mean tallies of
  []    -> 50
  means -> sum means / fromIntegral (length means)

-- 📚 The prior from my own shelf, worth `c` ratings.
-- わたしの本棚から作る、c 件ぶんの「ふつう」
shelfPrior :: Int -> [Card] -> Prior
shelfPrior c cards = Prior c (usualPoints (map (tally . ratingsOf) cards))
-- ANCHOR_END: usual

-- ANCHOR: wilson
-- 👍 How many ratings were "good" (at least `line` points), and how
--    many there were in all.
-- 「よかった」（line 点以上）の件数と、全部の件数
liked :: Int -> [Score] -> (Int, Int)
liked line scores =
  (length [ s | s <- scores, scoreValue s >= line ], length scores)

-- 🛡️ The lower bound of the Wilson score interval (95%): a careful
--    guess of the share of good ratings. 1 good out of 1 is not 100%.
-- Wilson スコア区間の下限（95%）：「よかった」の割合の、控えめな見積もり。
-- 1件中1件よかったは、100% ではない
wilsonLower :: (Int, Int) -> Double
wilsonLower (good, n)
  | n <= 0    = 0
  | otherwise = max 0 ((p + z2 / (2 * k) - z * sqrt spread) / (1 + z2 / k))
  where
    k      = fromIntegral n
    p      = fromIntegral good / k
    z      = 1.96               -- 95%
    z2     = z * z
    spread = p * (1 - p) / k + z2 / (4 * k * k)
    -- `max 0` hides a tiny minus (like -1e-17) from rounding at p = 0
    -- max 0 は、p = 0 のときの丸めの小さなマイナスを 0 にする
-- ANCHOR_END: wilson

-- ANCHOR: rankCards
-- 🏆 Rank the cards I rated, by any key made from their ratings,
--    highest first. Ties keep the order of the shelf.
-- 評価したカードを、評価から作った好きな値で、高い順に並べる。
-- 同点なら、本棚の順のまま
rankCards :: Ord k => ([Score] -> k) -> [Card] -> [(k, Card)]
rankCards key cards = sortOn (Down . fst)
  [ (key given, card)
  | card <- cards
  , let given = ratingsOf card
  , not (null given)
  ]
-- ANCHOR_END: rankCards
-- ANCHOR_END: all
