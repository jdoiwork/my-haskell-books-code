-- ANCHOR: all
-- This file does not compile on purpose: GHC shows candidates for the hole.
-- わざとコンパイルできないファイル。GHC が「穴」に入る候補を教えてくれる
module Hole where

import Data.List (partition)

-- Split a list into the elements that pass and the ones that do not
-- 条件を満たすものと、満たさないものに分けたい
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy = _
-- ANCHOR_END: all
