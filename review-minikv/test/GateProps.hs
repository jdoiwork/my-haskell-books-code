-- ANCHOR: all
-- | Properties of the gate.
-- 門番の性質
-- ANCHOR: head
module GateProps
  ( Knocks (..)
  , rule
  , prop_slidingDraft
  , prop_slidingTwice
  , prop_firstOnesPass
  ) where

import Data.List (sort)
import Test.QuickCheck

import MiniKv.Types (Millis)
import Review.Gate
-- ANCHOR_END: head

-- ANCHOR: knocks
-- 🎲 Times of knocks, sorted, within the first two minutes.
--    Each time shrinks on its own (then we sort again).
-- 門をたたく時刻（小さい順）。最初の 2 分のどこか。
-- 時刻を1つずつ縮める（縮めたら、また並べ直す）
newtype Knocks = Knocks [Millis]
  deriving (Show)

instance Arbitrary Knocks where
  arbitrary = Knocks . sort <$> listOf (chooseInt (0, 120000))
  shrink (Knocks ts) = map (Knocks . sort) (shrink ts)

-- 3 times a minute.
-- 1分に3回まで
rule :: Rule
rule = Rule { limit = 3, windowSeconds = 60 }

-- 🔢 For each time t that was let in, count how many were let in
--    during [t, t + 60 s); answer the largest count.
-- 通れた時刻 t ごとに、[t, t + 60 秒) のあいだに通れた回数を数え、
-- いちばん多い回数を答える
busiest :: [Millis] -> Int
busiest ts = maximum (0 : counts)
  where
    counts = [ length (filter (inWindow t) passed) | t <- passed ]
    passed = [ t | (t, ok) <- zip ts (allowAll rule "lambda" ts), ok ]
    inWindow t u = t <= u && u < t + 60000
-- ANCHOR_END: knocks

-- ANCHOR: draft
-- ❌ Step 2 (draft): "in any 60 seconds, at most 3 get in".
-- ステップ2（下書き）：「どの 60 秒をとっても、通れるのは 3 回まで」
prop_slidingDraft :: Knocks -> Bool
prop_slidingDraft (Knocks ts) = busiest ts <= limit rule
-- ANCHOR_END: draft

-- ANCHOR: props
-- ✅ What a fixed window really promises: at most twice the limit.
-- 決まった窓が、本当に約束できること：上限の 2 倍まで
prop_slidingTwice :: Knocks -> Bool
prop_slidingTwice (Knocks ts) = busiest ts <= 2 * limit rule

-- ✅ The first three knocks always get in.
-- 最初の 3 回は、いつでも通れる
prop_firstOnesPass :: Knocks -> Bool
prop_firstOnesPass (Knocks ts) =
  and (take (limit rule) (allowAll rule "lambda" ts))
-- ANCHOR_END: props
-- ANCHOR_END: all
