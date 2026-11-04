{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- Answers for the training of the libraries chapter.
-- よく使うライブラリの章のトレーニングの解答
module LibAnswers
  ( cheapestSlot
  , rounds
  , isOverdue
  ) where

import Data.Sequence (Seq)
import Data.Time (Day)
import Data.Vector qualified as V

import Deadline (deadline)
import Queue (nextTurn)
import Shelf (prices)

-- ANCHOR: cheapestSlot
-- 🔍 The slot of the cheapest potion.
-- いちばん安い薬の段
cheapestSlot :: Int
cheapestSlot = V.minIndex prices
-- ANCHOR_END: cheapestSlot

-- ANCHOR: rounds
-- 🔄 Who acts in the first n turns.
-- 最初の n ターンで、誰が行動するか
rounds :: Int -> Seq a -> [a]
rounds n queue
  | n <= 0    = []
  | otherwise =
      case nextTurn queue of
        Nothing              -> []
        Just (who, nextLine) -> who : rounds (n - 1) nextLine
-- ANCHOR_END: rounds

-- ANCHOR: isOverdue
-- ⌛ Is the deadline of a quest posted on that day already past?
-- その日に貼り出された依頼は、もう締め切りを過ぎている？
isOverdue :: Day -> Day -> Bool
isOverdue today postedDay = today > deadline postedDay
-- ANCHOR_END: isOverdue
-- ANCHOR_END: all
