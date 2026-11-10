-- ANCHOR: all
-- ANCHOR: imports
-- | Dice with a seed passed around by hand.
-- 種を手で渡していくサイコロ
module Dice
  ( Seed
  , rollDie
  , threeRolls
  ) where
-- ANCHOR_END: imports

-- ANCHOR: roll
-- 🌱 The seed: a number that decides the next roll.
-- 種：次の目を決める数
type Seed = Int

-- 🎲 Roll a die: the number on it, and the next seed.
--    (A tiny "linear congruential" generator; the same seed always
--    gives the same roll.)
-- サイコロを振る：出た目と、次の種。
-- （小さな「線形合同法」。同じ種からは、いつも同じ目が出る）
rollDie :: Seed -> (Int, Seed)
rollDie seed = (1 + (next `div` 65536) `mod` 6, next)
  where
    next = (seed * 1103515245 + 12345) `mod` 2147483648
-- ANCHOR_END: roll

-- ANCHOR: three
-- 🎲🎲🎲 Roll three times, passing the seed along by hand.
-- 3回振る。種を手で次へ渡していく
threeRolls :: Seed -> ([Int], Seed)
threeRolls s0 =
  let (a, s1) = rollDie s0
      (b, s2) = rollDie s1
      (c, s3) = rollDie s2
  in  ([a, b, c], s3)
-- ANCHOR_END: three

-- ANCHOR_END: all
