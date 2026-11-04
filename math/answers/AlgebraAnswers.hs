-- ANCHOR: all
-- | Answers to the training of "Associativity and monoids".
-- 「結合法則とモノイド」のトレーニングの解答
module AlgebraAnswers
  ( Tally (..)
  , tally
  , Top3 (..)
  , top3
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)

-- ANCHOR: tally
-- 🧮 Step 2: word counts that can be combined, like the word counts of
--    many files.
-- ステップ2：合わせられる単語の数。たくさんのファイルの単語の数のように
newtype Tally = Tally (Map String Int)
  deriving (Show, Eq)

instance Semigroup Tally where
  Tally a <> Tally b = Tally (Map.unionWith (+) a b)

instance Monoid Tally where
  mempty = Tally Map.empty

tally :: String -> Tally
tally text = Tally (Map.fromListWith (+) [ (w, 1) | w <- words text ])
-- ANCHOR_END: tally

-- ANCHOR: top3
-- 🏆 Step 3: the three best scores. Combining keeps only three.
-- ステップ3：上位3つの得点。合わせても、3つだけ残す
newtype Top3 = Top3 [Int]
  deriving (Show, Eq)

instance Semigroup Top3 where
  Top3 a <> Top3 b = Top3 (take 3 (sortBy (comparing Down) (a ++ b)))

instance Monoid Top3 where
  mempty = Top3 []

top3 :: Int -> Top3
top3 score = Top3 [score]
-- ANCHOR_END: top3
-- ANCHOR_END: all
