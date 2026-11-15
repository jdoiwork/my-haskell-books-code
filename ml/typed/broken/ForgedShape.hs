-- ANCHOR: all
-- | Does NOT compile: coerce cannot change the shape in the type,
--   because Matrix's parameters have the role nominal.
-- コンパイルできない：Matrix の引数の役割（role）が nominal なので、
-- coerce で型の形を付け替えられない
module ForgedShape where

import Data.Coerce (coerce)

import MiniTorch.Typed

-- 🎭 A 2 × 3 matrix, relabelled as 3 × 2 without moving a number.
-- 2 × 3 の行列に、数を動かさずに 3 × 2 の札を付け直そうとする
forged :: Matrix 2 3 -> Matrix 3 2
forged = coerce
-- ANCHOR_END: all
