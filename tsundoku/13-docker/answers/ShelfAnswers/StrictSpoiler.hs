-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training: hide more. A hidden note no longer says the title of the
--   entry it is on (a title can be a spoiler too: 「第9章 司書の死」).
--   It says only where the entry is in the table, by its place.
-- トレーニング：もっと隠す。隠した感想は、書いた項目の題も言わない（題も
-- ネタバレになりうる：「第9章 司書の死」）。目次の何番目の項目かだけを言う
module ShelfAnswers.StrictSpoiler (strictHint) where

import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Types

-- 🙈 When the note will show, without the entry's title.
-- 感想がいつ見えるかを、項目の題を言わずに
strictHint :: Contents -> Target -> Text
strictHint contents target = case target of
  OnWork    -> "ネタバレ（読み終えたら見えます）"
  OnEntry e -> case lookup e (zip (entryIds contents) [1 :: Int ..]) of
    Just n  -> "ネタバレ（目次の " <> T.pack (show n) <> " 番目を読んだら見えます）"
    Nothing -> "ネタバレ"
-- ANCHOR_END: all
