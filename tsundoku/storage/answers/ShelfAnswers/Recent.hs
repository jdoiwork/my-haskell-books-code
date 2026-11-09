-- ANCHOR: all
-- | Training step 2: later episodes weigh more.
-- トレーニングのステップ2：後の話ほど重くする
-- ANCHOR: head
module ShelfAnswers.Recent
  ( laterHeavier
  , recentForm
  ) where

import Data.Map.Strict qualified as Map

import Tsundoku.Card
import Tsundoku.Rating (Score, scoreValue)
-- ANCHOR_END: head

-- ANCHOR: recent
-- ⏳ An average where the i-th rating counts i times: the last ones
--    weigh the most. No ratings, no average.
-- i 番目の評価を i 回数える平均。後ろほど重い。評価がなければ、平均もない
laterHeavier :: [Score] -> Maybe Rational
laterHeavier []     = Nothing
laterHeavier scores = Just (weighted / fromIntegral (sum weights))
  where
    weights  = [1 .. length scores]
    weighted = sum [ fromIntegral (w * scoreValue s)
                   | (w, s) <- zip weights scores ]

-- 📈 How a serial is doing lately: its episode ratings, from the
--    first episode to the newest, later ones heavier.
-- 連載の最近の調子：話ごとの評価を、第1話から最新話の順に、後ろほど重く
recentForm :: Card -> Maybe Rational
recentForm card = laterHeavier (Map.elems (entryRatings card))
-- ANCHOR_END: recent
-- ANCHOR_END: all
