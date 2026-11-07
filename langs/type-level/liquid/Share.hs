-- Sharing gold, checked by Liquid Haskell, for the chapter
-- "Type-level programming".
-- ゴールドの山分けを、Liquid Haskell で確かめる。「型レベルプログラミング入門」の章
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
module Share where

-- ANCHOR: share
-- Liquid Haskell
{-@ type Pos = {v:Int | v > 0} @-}

{-@ share :: gold:Nat -> members:Pos -> Nat @-}
share :: Int -> Int -> Int
share gold members = gold `div` members
-- ANCHOR_END: share

-- Liquid Haskell accepts this call: 3 > 0.
-- この呼び出しは通る（3 > 0）
threeWays :: Int
threeWays = share 100 3
