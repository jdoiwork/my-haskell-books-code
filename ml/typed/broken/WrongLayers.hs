-- | This file must NOT compile: the first layer gives 32 numbers,
--   but the second layer takes 16.
-- このファイルはコンパイルできないはず：1つ目の層は 32 個を出すのに、
-- 2つ目の層は 16 個を受け取る
module WrongLayers where

import MiniTorch.TypedNN

-- ANCHOR: wrong
build :: Linear 35 32 -> Linear 16 10 -> MLP 35 32 10
build first second = MLP { fc1 = first, fc2 = second }
-- ANCHOR_END: wrong
