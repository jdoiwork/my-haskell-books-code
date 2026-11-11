-- ANCHOR: all
-- | How far I have read: fold the table of contents with a monoid.
--   Pure functions only; how to show it is the job of Tsundoku.Bar.
-- どこまで読んだか：目次の木を、モノイドで畳む。
-- 純粋な関数だけ。どう見せるかは Tsundoku.Bar の仕事
-- ANCHOR: header
module Tsundoku.Progress
  ( -- * Progress / 進み具合
    Progress (..)
  , progressOf
  , percent
  , remaining
    -- * Bookmarks and growing tables / しおりと、育つ目次
  , readThrough
  , nextEntryId
  , grow
    -- * The reading log / 読書ログ
  , finishedIn
  , perDay
  , streak
  , lastWeek
  , finishBy
  ) where

-- sets and maps, days, and trees
-- 集合と辞書、日付、木
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day, addDays)
import Data.Tree (Tree (..), flatten)

import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: progress
-- 📊 How far: how much I have read, out of how much there is.
-- 進み具合：全体のうち、どれだけ読んだか
data Progress u = Progress
  { done  :: u
  , total :: u
  }
  deriving (Show, Eq)

-- ➕ Two pieces of progress add up, part by part.
-- 進み具合どうしは、読んだ量と全体の量を、それぞれ足す
instance Semigroup u => Semigroup (Progress u) where
  Progress d1 t1 <> Progress d2 t2 = Progress (d1 <> d2) (t1 <> t2)

-- 🫙 No entries at all: nothing read, out of nothing.
-- 項目が1つもない：何もないうちの、何も読んでいない
instance Monoid u => Monoid (Progress u) where
  mempty = Progress mempty mempty
-- ANCHOR_END: progress

-- ANCHOR: progressOf
-- 🌳 Fold a table into progress: every entry gives its own piece,
--    and the pieces just add up.
-- 目次を畳んで進み具合にする。項目ごとに1切れ作って、足すだけ
progressOf :: Monoid u => Set EntryId -> Toc u -> Progress u
progressOf finished toc = foldMap (foldMap piece) toc
  where
    piece entry
      | entryId entry `Set.member` finished = Progress amountOf amountOf
      | otherwise                           = Progress mempty   amountOf
      where
        amountOf = size entry
-- ANCHOR_END: progressOf

-- ANCHOR: percent
-- 💯 Progress in percent, rounded (halves go up). But never 100
--    before the end, and never 0 after the start.
-- 百分率。四捨五入（ちょうど半分は切り上げ）。ただし、読み終わるまでは
-- 100 にせず、読み始めたら 0 にしない
percent :: Amount u => Progress u -> Int
percent p
  | t == 0    = 0
  | d >= t    = 100
  | d <= 0    = 0
  | otherwise = max 1 (min 99 rounded)
  where
    d       = count (done p)
    t       = count (total p)
    rounded = (d * 200 + t) `div` (2 * t)

-- ⏳ How much is left to read.
-- 残りの量
remaining :: Amount u => Progress u -> u
remaining p = amount (count (total p) - count (done p))
-- ANCHOR_END: percent

-- ANCHOR: readThrough
-- 🔖 A bookmark: everything up to this entry (itself too), from top
--    to bottom, counts as read.
-- しおり：上から順に、この項目まで（この項目も）読んだことにする
readThrough :: EntryId -> Toc u -> Either Mistake (Set EntryId)
readThrough mark toc =
  case break (== mark) ids of
    (before, _ : _) -> Right (Set.fromList (before ++ [mark]))
    (_     , []   ) -> Left (NoSuchEntry mark)
  where
    ids = map entryId (concatMap flatten toc)
-- ANCHOR_END: readThrough

-- ANCHOR: grow
-- 🆕 A number nobody has used yet: one more than the largest.
-- まだ誰も使っていない番号：いちばん大きい番号の次
nextEntryId :: Toc u -> EntryId
nextEntryId toc =
  case map entryId (concatMap flatten toc) of
    []  -> EntryId 1
    ids -> let EntryId n = maximum ids in EntryId (n + 1)

-- 🌱 A new episode comes out: add it at the end of the last arc
--    (or season), with a new number. Old numbers never change.
-- 新しい話が出た：最後の章（シーズン）の終わりに、新しい番号で足す。
-- 前からある番号は、変えない
grow :: (Text, u) -> Toc u -> Toc u
grow (newHeading, newSize) toc =
  case reverse toc of
    -- the last arc has episodes: add the new one after them
    -- 最後の章に話がある：その話の後ろに足す
    Node top kids@(_ : _) : older ->
      reverse older ++ [Node top (kids ++ [leaf])]
    -- a serial without arcs (or an empty table): add it at the end
    -- 章のない連載（か、空の目次）：森の最後に足す
    _ -> toc ++ [leaf]
  where
    leaf = Node (Entry (nextEntryId toc) newHeading newSize) []
-- ANCHOR_END: grow

-- ANCHOR: log
-- ✅ Every entry the log says I have read to the end.
-- ログによると、読み終えた項目の全部
finishedIn :: [LogLine] -> Set EntryId
finishedIn logLines = Set.fromList (map readEntry logLines)

-- 📅 How much I read on each day: the sizes of the entries I
--    finished that day, added up.
-- 日ごとに読んだ量：その日に読み終えた項目の量を、足したもの
perDay :: Monoid u => Toc u -> [LogLine] -> Map Day u
perDay toc logLines =
  Map.fromListWith (<>)
    [ (readOn line, size entry)
    | line  <- logLines
    , Just entry <- [Map.lookup (readEntry line) entries]
    ]
  where
    entries = Map.fromList
      [ (entryId entry, entry) | entry <- concatMap flatten toc ]
-- ANCHOR_END: log

-- ANCHOR: streak
-- 🔥 How many days in a row I have read, up to today. If I have
--    not read yet today, the streak still counts up to yesterday.
-- 今日まで、何日続けて読んだか。今日まだ読んでいなくても、
-- 昨日までの連続は、まだ切れていない
streak :: Day -> Set Day -> Int
streak today days = length (takeWhile (`Set.member` days) back)
  where
    start = if today `Set.member` days then today else addDays (-1) today
    back  = iterate (addDays (-1)) start
-- ANCHOR_END: streak

-- ANCHOR: finishBy
-- 🗓️ How much I read in the last 7 days, today included.
-- 今日を含めた、最近7日に読んだ量
lastWeek :: Monoid u => Day -> Map Day u -> u
lastWeek today daily =
  mconcat [ Map.findWithDefault mempty d daily | d <- week ]
  where
    week = [ addDays (-k) today | k <- [0 .. 6] ]

-- 🏁 At the pace of the last 7 days, on which day will I finish?
--    Nothing if I read nothing last week.
-- 最近7日のペースなら、何日に読み終わるか。
-- 最近7日に何も読んでいなければ Nothing
finishBy :: Amount u => Day -> u -> u -> Maybe Day
finishBy today left week
  | l <= 0    = Just today
  | w <= 0    = Nothing
  | otherwise = Just (addDays (fromIntegral days) today)
  where
    l    = count left
    w    = count week
    -- the smallest n with n * w >= 7 * l (no floating point)
    -- n * w >= 7 * l となる、いちばん小さい n（浮動小数点を使わない）
    days = (7 * l + w - 1) `div` w
-- ANCHOR_END: finishBy
-- ANCHOR_END: all
