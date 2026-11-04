-- | This file must NOT compile: the box tries to escape from runST.
-- このファイルはコンパイルできないはず：箱が runST から逃げ出そうとする
module Escape where

import Control.Monad.ST
import Data.STRef

-- ANCHOR: escape
-- 🏃 Take the box itself out of runST, to change it later.
-- 箱そのものを runST の外に持ち出して、あとで書き換えたい
leak :: STRef s Int
leak = runST (newSTRef 0)
-- ANCHOR_END: escape
