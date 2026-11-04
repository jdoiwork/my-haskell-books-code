-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏋️ Answers for the training.
-- トレーニングの解答
module TuningAnswers (totalsSorted) where

import Data.List          (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text          (Text)

import Report

-- ANCHOR: sorted
-- 🗂️ Sort by hero, then add up each group: O(n log n), without a Map.
-- 勇者で並べ替えてから、組ごとに足す：O(n log n)、Map なしで
totalsSorted :: [Hit] -> [(Text, Int)]
totalsSorted hits = map total (NE.groupBy sameHero (sortOn (.hero) hits))
  where
    sameHero a b = a.hero == b.hero
    total (h :| rest) = (h.hero, sum (map (.damage) (h : rest)))
-- ANCHOR_END: sorted
-- ANCHOR_END: all
