-- ANCHOR: all
-- 🏋️ Answers for the training.
-- トレーニングの解答
module DataAnswers (size, largest, mostCommon) where

import Data.Vector.Unboxed qualified as VU

import Tree

-- ANCHOR: size
-- 🔢 How many values are in the tree.
-- 木の中の値の数
size :: Tree a -> Int
size Leaf                = 0
size (Node left _ right) = size left + 1 + size right

-- 🏔️ The largest value: keep going right.
-- いちばん大きい値：右へ右へと進む
largest :: Tree a -> Maybe a
largest Leaf                = Nothing
largest (Node _ v Leaf)     = Just v
largest (Node _ _ right)    = largest right
-- ANCHOR_END: size

-- ANCHOR: common
-- 📊 The damage that came up most often (the box with the most).
-- いちばん多く出たダメージ（数がいちばん多い箱）
mostCommon :: VU.Vector Int -> Int
mostCommon counts = VU.maxIndex counts
-- ANCHOR_END: common
-- ANCHOR_END: all
