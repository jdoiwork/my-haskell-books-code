-- ANCHOR: all
-- | Training step 3: a separate ranking for each kind of work.
-- トレーニングのステップ3：作品の種類ごとに、別のランキング
-- ANCHOR: head
module ShelfAnswers.ByKind
  ( Kind (..)
  , kindOf
  , rankKind
  ) where

import Tsundoku.Card
import Tsundoku.Ranking
import Tsundoku.Types (Contents (..), Work (..))
-- ANCHOR_END: head

-- ANCHOR: kind
-- 🗂️ The kinds of works, without what is inside them.
-- 作品の種類（中身は持たない）
data Kind = Books | Serials | Dramas | Films | Games
  deriving (Show, Eq)

kindOf :: Contents -> Kind
kindOf what = case what of
  Book   _ -> Books
  Serial _ -> Serials
  Series _ -> Dramas
  Film   _ -> Films
  Game     -> Games

-- 🏆 Rank only the works of one kind, against the usual points of
--    that kind alone.
-- 1つの種類の作品だけを、その種類だけの「ふつうの点」と比べて並べる
rankKind :: Int -> Kind -> [Card] -> [(Rational, Card)]
rankKind c kind cards =
  rankCards (bayesian (shelfPrior c same) . tally) same
  where
    same = [ card | card <- cards, kindOf (contents (work card)) == kind ]
-- ANCHOR_END: kind
-- ANCHOR_END: all
