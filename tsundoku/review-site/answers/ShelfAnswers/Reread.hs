-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training: a shelf that fills itself — works I loved (★4 or more)
--   and finished more than a year ago. Time to read them again?
-- トレーニング：ひとりでに埋まる本棚。★4 以上で、読み終えてから1年より
-- たったもの。そろそろ読み返す？
module ShelfAnswers.Reread (wantReread) where

import Data.Time (Day, addDays)

import Tsundoku.Card (Card (..))
import Tsundoku.Rating (scoreValue)
import Tsundoku.Review (finishedOn)
import Tsundoku.Types

-- 🔁 On this day, the works to read again, in the shelf's order.
-- この日に、読み返したい作品（本棚の順で）
wantReread :: Day -> [(WorkId, Card)] -> [(WorkId, Card)]
wantReread today cards =
  [ wc | wc@(_, c) <- cards
       , maybe False (>= 80) (scoreValue <$> c.rating)
       , Just d <- [finishedOn c]
       , d < addDays (-365) today ]
-- ANCHOR_END: all
