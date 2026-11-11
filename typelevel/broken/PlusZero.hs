-- | This file must NOT compile: GHC does not know that n + 0 = n.
-- このファイルはコンパイルできないはず：GHC は n + 0 = n を知らない
module PlusZero where

import Vec

-- ANCHOR: plusZero
appendNothing :: Vec n a -> Vec n a
appendNothing xs = vappend xs VNil
-- ANCHOR_END: plusZero
