-- | This file must NOT compile: the data has 36 columns,
--   but the network wants 35.
-- このファイルはコンパイルできないはず：データは 36 列、ネットワークは 35 列
module WrongColumns where

import MiniTorch.Typed
import MiniTorch.TypedNN

-- ANCHOR: wrong
guess :: MLP 35 32 10 -> Matrix 4 36 -> [Int]
guess net x = predict net x
-- ANCHOR_END: wrong
