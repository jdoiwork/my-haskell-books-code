-- ANCHOR: all
-- ANCHOR: header
-- | What the groups have found so far, while matching: a table from
--   the group's number to its span. A group that has found nothing yet
--   is not in it.
-- 探しているあいだに、グループがここまでに見つけたもの：グループの番号から、
-- その場所への表。まだ何も見つけていないグループは、表にない
module MiniRegex.Caps
  ( Caps
  , noCaps
  , remember
  , recall
  , groupList
  , keepOut
  , keptFrom
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import MiniRegex.Cursor
import MiniRegex.Types (Span (..))
-- ANCHOR_END: imports

-- ANCHOR: caps
-- 🗂️ Group number to span.
-- グループの番号から、場所へ
type Caps = IntMap Span

-- 🫙 Nothing found yet.
-- まだ何も見つけていない
noCaps :: Caps
noCaps = IntMap.empty

-- ✍️ Group n found the text from one cursor to another. A group in a
--    repeat writes over what it found the time before.
-- n 番のグループが、しおりから、しおりまでの文字列を見つけた。
-- くり返しの中のグループは、前の回に見つけたものを上書きする
remember :: Int -> Cursor -> Cursor -> Caps -> Caps
remember n from to = IntMap.insert n (Span (taken from to) (at from) (at to))

-- 🔎 What group n has found (for \1).
-- n 番のグループが見つけたもの（\1 のため）
recall :: Int -> Caps -> Maybe Span
recall = IntMap.lookup

-- 📋 Groups 1 to n, in order; Nothing for the ones that found nothing.
-- 1番から n 番までのグループを順に。何も見つけなかったものは Nothing
groupList :: Int -> Caps -> [Maybe Span]
groupList n caps = [IntMap.lookup i caps | i <- [1 .. n]]
-- ANCHOR_END: caps

-- ANCHOR: keep
-- ✂️ \K: write down where the match is to start, in drawer 0 (no group
--    has the number 0; group 0 is the whole match).
-- \K：一致が始まることにする場所を、0番の引き出しに書く（0番の
-- グループはない。0番は一致の全体）
keepOut :: Cursor -> Caps -> Caps
keepOut c = IntMap.insert 0 (Span "" (at c) (at c))

-- 📍 Where the last \K was, if there was one.
-- 最後の \K の場所（あれば）
keptFrom :: Caps -> Maybe Int
keptFrom caps = spanStart <$> IntMap.lookup 0 caps
-- ANCHOR_END: keep
-- ANCHOR_END: all
