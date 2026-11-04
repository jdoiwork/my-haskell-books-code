-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write Text values as "..." literals.
-- "..." の書き方で Text の値を書けるようにする
{-# LANGUAGE OverloadedStrings #-}
-- Let us write record.hero to read a field.
-- record.hero のようにフィールドを読めるようにする
{-# LANGUAGE OverloadedRecordDot #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
-- The stats of the hall of fame: count the records for each hero.
-- Everything here is pure.
-- 殿堂の集計：勇者ごとに記録を数える
-- ここはすべて純粋
module Stats where

-- Text: efficient text. T.xxx: functions for Text.
-- Text：効率のよい文字列。T.xxx：Text 用の関数
import Data.Text (Text)
import Data.Text qualified as T

-- Map: a dictionary, to add up the numbers for each hero.
-- Map：勇者ごとに数を足し合わせるための辞書
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- The records of the hall of fame
-- 殿堂の記録
import Records
-- ANCHOR_END: module

-- ANCHOR: statsByHero
-- 📊 For each hero: (name, battles, wins), in name order.
-- 勇者ごとの（名前, 戦った回数, 勝った回数）。名前の順
statsByHero :: [Record] -> [(Text, Int, Int)]
statsByHero records =
  let pairs  = [ (r.hero, (1, winPoint r)) | r <- records ]
      totals = Map.fromListWith addUp pairs
  in  [ (name, battles, wins) | (name, (battles, wins)) <- Map.toList totals ]
  where
    -- 1 for a win, 0 otherwise / 勝ちなら 1、それ以外は 0
    winPoint r = if r.outcome == Won then 1 else 0
    -- add up (battles, wins) / （戦った回数, 勝った回数）を足し合わせる
    addUp (b1, w1) (b2, w2) = (b1 + b2, w1 + w2)
-- ANCHOR_END: statsByHero

-- ANCHOR: fullStatsByHero
-- 📊 For each hero: (name, won, lost, escaped), in name order.
-- 勇者ごとの（名前, 勝ち, 負け, 逃げ）の回数。名前の順
fullStatsByHero :: [Record] -> [(Text, Int, Int, Int)]
fullStatsByHero records =
  let pairs  = [ (r.hero, points r.outcome) | r <- records ]
      totals = Map.fromListWith addUp pairs
  in  [ (name, w, l, e) | (name, (w, l, e)) <- Map.toList totals ]
  where
    -- one outcome as (won, lost, escaped)
    -- 結末1つを（勝ち, 負け, 逃げ）の形にする
    points Won     = (1, 0, 0)
    points Lost    = (0, 1, 0)
    points Escaped = (0, 0, 1)
    -- add up two (won, lost, escaped)
    -- （勝ち, 負け, 逃げ）を2つ足し合わせる
    addUp (w1, l1, e1) (w2, l2, e2) = (w1 + w2, l1 + l2, e1 + e2)
-- ANCHOR_END: fullStatsByHero

-- ANCHOR: aliases
-- 🏷️ Short names for the two kinds of Map we use.
-- 使う2種類の Map に、短い名前を付ける

-- outcome -> how many times / 結末 → 回数
type OutcomeCounts = Map Outcome Int

-- hero's name -> their outcome counts / 勇者の名前 → その勇者の結末ごとの回数
type HeroStats = Map Text OutcomeCounts
-- ANCHOR_END: aliases

-- ANCHOR: countOutcomes
-- 📊 For each hero: how many times each outcome happened.
--    A new outcome needs no change here.
-- 勇者ごとに、それぞれの結末が何回あったか
-- 結末が増えても、ここは変えなくてよい
countOutcomes :: [Record] -> HeroStats
countOutcomes records =
  -- ANCHOR: countPairs
  let pairs = [ (r.hero, Map.singleton r.outcome 1) | r <- records ]
  -- ANCHOR_END: countPairs
  -- ANCHOR: countMerge
  in  Map.fromListWith (Map.unionWith (+)) pairs
  -- ANCHOR_END: countMerge
-- ANCHOR_END: countOutcomes

-- ANCHOR: formatCounts
-- 📋 Every outcome, from the first to the last (Enum and Bounded).
-- 最初から最後まで、すべての結末（Enum と Bounded のおかげ）
allOutcomes :: [Outcome]
allOutcomes = [minBound .. maxBound]

-- 🖨️ "won 2, lost 0, escaped 1": every outcome, even if it is 0.
-- すべての結末の回数を並べる。0 回のものも出す
formatCounts :: OutcomeCounts -> Text
formatCounts counts =
  T.intercalate ", "
    [ outcomeText o <> " " <> T.pack (show (Map.findWithDefault 0 o counts))
    | o <- allOutcomes
    ]
-- ANCHOR_END: formatCounts
-- ANCHOR_END: all
