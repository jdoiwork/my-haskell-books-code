-- ANCHOR: all
-- | Choosing quests that do not overlap, for the most reward.
-- 重ならない依頼を選んで、報酬をいちばん多くする
module Review.Quests
  ( Quest (..)
  , board
  , compatible
  , totalReward
  , plansSpec
  , bestRewardSpec
  , greedyReward
  , bestRewardSlow
  , bestReward
  ) where

import Data.Array
import Data.List (sortOn)

-- ANCHOR: quest
-- 📜 A quest takes the hours from start up to (not including) end.
-- 依頼は、start 時から end 時の手前までかかる
data Quest = Quest
  { title  :: String
  , start  :: Int
  , end    :: Int
  , reward :: Int
  }
  deriving (Show, Eq)

board :: [Quest]
board =
  [ Quest "slay the slime"    0 2  10
  , Quest "guard the bridge"  1 7  55
  , Quest "find the herb"     2 4  15
  , Quest "escort the bard"   4 6  15
  , Quest "hunt the wolf"     7 10 20
  ]

-- 🤝 Two quests fit together when one ends before the other starts.
-- 片方が終わってから、もう片方が始まるなら、両方受けられる
compatible :: Quest -> Quest -> Bool
compatible a b = end a <= start b || end b <= start a

totalReward :: [Quest] -> Int
totalReward qs = sum (map reward qs)
-- ANCHOR_END: quest

-- ANCHOR: spec
-- 📋 Step 1: every plan, by trying "skip it" and "take it".
-- ステップ1：「受けない」と「受ける」を全部試して、すべての予定を作る
plansSpec :: [Quest] -> [[Quest]]
plansSpec []       = [[]]
plansSpec (q : qs) = do
  rest <- plansSpec qs
  rest : [q : rest | all (compatible q) rest]

bestRewardSpec :: [Quest] -> Int
bestRewardSpec qs = maximum (map totalReward (plansSpec qs))
-- ANCHOR_END: spec

-- ANCHOR: greedy
-- 🏃 Step 2: the greedy rule from the greedy chapter:
--    take whatever ends first, if it fits.
-- ステップ2：貪欲法の章の決まり：先に終わるものから、入るなら受ける
greedyReward :: [Quest] -> Int
greedyReward qs = totalReward (foldl takeIfFree [] (sortOn end qs))
  where
    takeIfFree taken q
      | all (compatible q) taken = q : taken
      | otherwise                = taken
-- ANCHOR_END: greedy

-- ANCHOR: dpSlow
-- 🧮 Step 3: sort by end. best i is the most reward using only the
--    first i quests: either skip quest i, or take it and add the best
--    of the quests that end before it starts.
-- ステップ3：終わる順に並べる。best i は、最初の i 個だけを使ったときの
-- いちばん多い報酬。i 番目を受けないか、受けて、それが始まる前に
-- 終わる依頼たちの best を足すか
bestRewardSlow :: [Quest] -> Int
bestRewardSlow qs = table ! n
  where
    n      = length qs
    sorted = listArray (1, n) (sortOn end qs)
    table  = listArray (0, n) [best i | i <- [0 .. n]] :: Array Int Int
    best 0 = 0
    best i =
      let q      = sorted ! i
          skip   = table ! (i - 1)
          before = length (takeWhile (\j -> end (sorted ! j) <= start q)
                                     [1 .. i - 1])
          takeIt = reward q + table ! before
      in max skip takeIt
-- ANCHOR_END: dpSlow

-- ANCHOR: dp
-- 🔍 Step 4: the quests that end before q starts are a prefix,
--    so find its length by binary search.
-- ステップ4：q が始まる前に終わる依頼は、前のほうにまとまっているので、
-- その数を二分探索で求める
bestReward :: [Quest] -> Int
bestReward qs = table ! n
  where
    n      = length qs
    sorted = listArray (1, n) (sortOn end qs)
    table  = listArray (0, n) [best i | i <- [0 .. n]] :: Array Int Int
    best 0 = 0
    best i =
      let q      = sorted ! i
          skip   = table ! (i - 1)
          takeIt = reward q + table ! endsBefore (start q) 0 (i - 1)
      in max skip takeIt
    -- The largest j in [lo, hi] whose quest ends by time t
    -- (lo itself is always fine).
    -- 時刻 t までに終わる、[lo, hi] の中でいちばん大きい j
    -- （lo はいつも条件を満たす）
    endsBefore :: Int -> Int -> Int -> Int
    endsBefore t lo hi
      | lo >= hi                 = lo
      | end (sorted ! mid) <= t  = endsBefore t mid hi
      | otherwise                = endsBefore t lo (mid - 1)
      where
        mid = (lo + hi + 1) `div` 2
-- ANCHOR_END: dp
-- ANCHOR_END: all
