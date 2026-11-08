-- ANCHOR: all
-- 🧪 A shelf of potions: a Vector, where any slot is one step away.
-- 薬の棚：どの段にも一歩で届く Vector
-- ANCHOR: imports
module Shelf where

import Data.Vector (Vector)
import Data.Vector qualified as V
-- ANCHOR_END: imports

-- ANCHOR: shelf
-- 💰 The prices of the potions, from slot 0.
-- 薬の値段。0番の段から順に
prices :: Vector Int
prices = V.fromList [50, 120, 30, 300, 80]

-- 🔍 The price at a slot. A slot that does not exist is Nothing.
-- その段の値段。ない段なら Nothing
priceAt :: Int -> Maybe Int
priceAt slot = prices V.!? slot

-- 🏷️ 10% off, for every potion.
-- 全部の薬を1割引に
onSale :: Vector Int
onSale = V.map (\p -> p * 9 `div` 10) prices
-- ANCHOR_END: shelf

-- ANCHOR: generate
-- 🔢 A shelf made from slot numbers: slot i costs (i + 1) * 100.
-- 段の番号から作る棚：i 番の段は (i + 1) * 100
priceList :: Int -> Vector Int
priceList n = V.generate n (\i -> (i + 1) * 100)
-- ANCHOR_END: generate
-- ANCHOR_END: all
