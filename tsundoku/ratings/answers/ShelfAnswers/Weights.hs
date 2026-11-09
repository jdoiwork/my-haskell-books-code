-- ANCHOR: all
-- | Training step 1: how the weight of the prior moves the top.
-- トレーニングのステップ1：「何件ぶん」で、1位がどう動くか
-- ANCHOR: head
module ShelfAnswers.Weights
  ( leaderFor
  ) where

import Data.Text (Text)

import Tsundoku.Card
import Tsundoku.Ranking
import Tsundoku.Types (Work (..))
-- ANCHOR_END: head

-- ANCHOR: leader
-- 🥇 The title at the top of my shelf, with a prior worth `c` ratings.
-- 「ふつう」を c 件ぶん混ぜたときの、本棚の1位の題
leaderFor :: Int -> [Card] -> Maybe Text
leaderFor c cards =
  case rankCards (bayesian (shelfPrior c cards) . tally) cards of
    []              -> Nothing
    (_, card) : _   -> Just (title (work card))
-- ANCHOR_END: leader
-- ANCHOR_END: all
