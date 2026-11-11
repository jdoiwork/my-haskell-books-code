-- | This file must NOT compile: coerce cannot unlock a chest,
--   because the role of Chest's parameter is nominal.
-- このファイルはコンパイルできないはず：Chest の引数の role が nominal なので、
-- coerce で鍵を外すことはできない
module CoerceOpen where

import Data.Coerce (coerce)

import Chest

-- ANCHOR: coerceOpen
sneak :: [String]
sneak = open (coerce (newChest ["potion", "sword"]))
-- ANCHOR_END: coerceOpen
