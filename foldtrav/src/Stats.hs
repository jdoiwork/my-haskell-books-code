-- ANCHOR: all
-- | Several answers from one walk over the tree, thanks to Monoids.
-- Monoid のおかげで、木を1回たどるだけで、いくつもの答え
module Stats
  ( Report (..)
  , report
  ) where

-- ANCHOR: imports
import Data.Semigroup (Max (..), Min (..), Sum (..))
-- ANCHOR_END: imports

-- ANCHOR: report
-- 📊 How many, the total, the cheapest and the dearest.
-- 個数、合計、いちばん安いもの、いちばん高いもの
data Report = Report
  { count    :: Int
  , total    :: Int
  , cheapest :: Int
  , dearest  :: Int
  }
  deriving (Show, Eq)

-- 🧮 One foldMap: each price becomes a 4-tuple of Monoids,
--    and the tuples are joined part by part.
-- foldMap を1回：値段を Monoid の4つ組にして、部品ごとにつなぐ
report :: Foldable t => t Int -> Report
report prices =
  let (Sum n, Sum s, Min lo, Max hi) =
        foldMap (\p -> (Sum 1, Sum p, Min p, Max p)) prices
  in  Report { count = n, total = s, cheapest = lo, dearest = hi }
-- ANCHOR_END: report
-- ANCHOR_END: all
