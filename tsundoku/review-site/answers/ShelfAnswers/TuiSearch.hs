-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}
-- | Training: the hits of a search, as lines for the terminal. The
--   words that matched go in 【】 (the browser uses <mark>).
-- トレーニング：検索のヒットを、ターミナルの行に。当たった語は【】で囲む
-- （ブラウザーは <mark> を使う）
module ShelfAnswers.TuiSearch (hitLines) where

import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Api.Json (HitItem (..), Snip (..))

-- 🔎 Two lines for each hit: where, and the snippet.
-- ヒットごとに2行：どこか、と抜き出し
hitLines :: [HitItem] -> [Text]
hitLines hits = concat
  [ [ T.pack (show i) <> ". " <> T.intercalate " > " (h.title : h.trail)
    , "   " <> foldMap piece h.snippet ]
  | (i, h) <- zip [1 :: Int ..] hits ]
  where
    piece s = if s.match then "【" <> s.text <> "】" else s.text
-- ANCHOR_END: all
