-- ANCHOR: all
-- | A tree, written down as lines with depths (the way a database
--   reads it back), and back to a tree again.
-- 木を「深さつきの行」に書き下ろす（データベースが読み戻す形）。
-- そして、行から木に戻す
-- ANCHOR: header
module Tsundoku.Outline
  ( outline
  , fromOutline
  ) where

-- the rose tree of containers
-- containers の rose tree
import Data.Tree (Forest, Tree (..))
-- ANCHOR_END: header

-- ANCHOR: outline
-- 📜 Write a forest down from top to bottom, each line with its depth
--    (0 for the top). [第1部, 第1章, 1.1] becomes [(0,…), (1,…), (2,…)].
-- 森を上から順に、深さつきの行にする（いちばん上は深さ 0）。
-- 第1部・第1章・1.1 なら、[(0,…), (1,…), (2,…)]
outline :: Forest a -> [(Int, a)]
outline = concatMap (linesOf 0)
  where
    linesOf depth (Node x children) =
      (depth, x) : concatMap (linesOf (depth + 1)) children

-- 🌳 Grow the forest back from the lines. A line one step deeper is a
--    child of the line above it.
-- 行から森を育て直す。1段深い行は、すぐ上の行の子
fromOutline :: [(Int, a)] -> Forest a
fromOutline rows = fst (forestAt 0 rows)

-- Take the trees at this depth, and give back the lines left over.
-- この深さの木を取り出し、残りの行を返す
forestAt :: Int -> [(Int, a)] -> (Forest a, [(Int, a)])
forestAt depth ((d, x) : rest)
  | d == depth =
      let (children, afterChildren) = forestAt (depth + 1) rest
          (siblings, afterSiblings) = forestAt depth afterChildren
      in  (Node x children : siblings, afterSiblings)
forestAt _ rest = ([], rest)
-- ANCHOR_END: outline
-- ANCHOR_END: all
