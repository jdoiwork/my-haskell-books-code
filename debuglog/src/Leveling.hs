-- ANCHOR: all
-- 🎚️ Experience points and levels.
-- 経験値とレベル
module Leveling where

-- ANCHOR: import
-- trace and friends: print while computing (for debugging only).
-- trace の仲間たち：計算しながら表示する（デバッグ専用）
import Debug.Trace (traceShow)
-- ANCHOR_END: import

-- ANCHOR: buggy
-- 🐛 Level 1 -> 2 needs 100 exp, 2 -> 3 needs 200 more, and so on.
--    This version has a bug. Can you find it?
-- レベル1→2に100、2→3にさらに200……の経験値がいる
-- この版にはバグがある。見つけられるかな？
levelForBuggy :: Int -> Int
levelForBuggy total = go 1 total
  where
    go lv rest
      | rest <= lv * 100 = lv
      | otherwise        = go (lv + 1) (rest - lv * 100)
-- ANCHOR_END: buggy

-- ANCHOR: traced
-- 🔍 The same function, with traceShow on every step.
-- 同じ関数の、各ステップに traceShow を入れた版
levelForTraced :: Int -> Int
levelForTraced total = go 1 total
  where
    go lv rest
      | traceShow (lv, rest) False = undefined
      | rest <= lv * 100 = lv
      | otherwise        = go (lv + 1) (rest - lv * 100)
-- ANCHOR_END: traced

-- ANCHOR: fixed
-- ✅ Fixed: with exactly enough exp, you move on to the next level.
-- 直した版：ちょうど足りたら、次のレベルに進む
levelFor :: Int -> Int
levelFor total = go 1 total
  where
    go lv rest
      | rest < lv * 100 = lv
      | otherwise       = go (lv + 1) (rest - lv * 100)
-- ANCHOR_END: fixed
-- ANCHOR_END: all
