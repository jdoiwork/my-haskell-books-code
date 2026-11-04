-- ANCHOR: all
-- | Instances you have used without noticing: pairs, functions, ZipList.
-- 気づかずに使っていたインスタンス：組、関数、ZipList
module Instances
  ( loot
  , found
  , doubleAfterHeal
  , pairwise
  , everyPair
  ) where

import Control.Applicative (ZipList (..))

-- ANCHOR: pair
-- 🎁 A pair is a Functor over its second part.
-- 組は、2つ目について Functor
loot :: (String, Int)
loot = fmap (* 2) ("goblin", 15)

-- 📜 With a Monoid on the left, a pair is even Applicative:
--    the left parts are joined with <>.
-- 左が Monoid なら、組は Applicative にもなる：左どうしは <> でつなぐ
found :: (String, Int)
found = (+) <$> ("sword, ", 10) <*> ("shield", 5)
-- ANCHOR_END: pair

-- ANCHOR: function
-- 🧪 A function is a Functor too: fmap is "do this afterwards" (.).
-- 関数も Functor：fmap は「あとでこれをする」（.）
doubleAfterHeal :: Int -> Int
doubleAfterHeal = fmap (* 2) (+ 10)
-- ANCHOR_END: function

-- ANCHOR: zip
-- 🤐 Two Applicatives for lists: every pair, or side by side.
-- リストの Applicative は2つ：すべての組か、横に並べるか
everyPair :: [Int]
everyPair = (+) <$> [1, 2, 3] <*> [10, 20, 30]

pairwise :: [Int]
pairwise = getZipList ((+) <$> ZipList [1, 2, 3] <*> ZipList [10, 20, 30])
-- ANCHOR_END: zip
-- ANCHOR_END: all
