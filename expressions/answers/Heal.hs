-- ANCHOR: module
module Heal where
-- ANCHOR_END: module

-- ANCHOR: heal
-- 💚 Heal by some amount, but HP never goes above 100.
-- HP を回復する。ただし HP は 100 を超えない
heal :: Int -> Int -> Int
heal amount hp
  | healed > 100 = 100
  | otherwise    = healed
  where
    healed = hp + amount
-- ANCHOR_END: heal

-- ANCHOR: healUpTo
-- 💖 Heal by some amount, but HP never goes above the given maximum HP.
-- HP を回復する。ただし HP は、指定した最大 HP を超えない
healUpTo :: Int -> Int -> Int -> Int
healUpTo maxHp amount hp
  | healed > maxHp = maxHp
  | otherwise      = healed
  where
    healed = hp + amount
-- ANCHOR_END: healUpTo
