module PartyAnswers where

-- ANCHOR: hpPercent
-- ❤️ How much HP is left, in percent, rounded to a whole number.
-- 残りの HP は何パーセントか（整数に丸める）
hpPercent :: Int -> Int -> Int
hpPercent hp maxHp = round percent
  where
    percent = 100 * fromIntegral hp / fromIntegral maxHp :: Double
-- ANCHOR_END: hpPercent

-- ANCHOR: speedBonus
-- 🏃 The speed bonus grows with the square root of the agility.
-- 素早さのボーナスは、素早さの平方根で増える
speedBonus :: Int -> Double
speedBonus agility = sqrt (fromIntegral agility)
-- ANCHOR_END: speedBonus

-- ANCHOR: isqrt
-- 🔢 The square root of n (0 or more), rounded down, exactly.
--    sqrt on a Double may be one off for a big n,
--    so the answer is fixed by one step at a time.
--    The squares are checked in Integer: near maxBound,
--    (r + 1) * (r + 1) would wrap around in Int.
-- n（0 以上）の平方根の切り捨てを、正確に求める。
-- 大きな n では Double の sqrt が 1 ずれることがあるので、1 ずつ直す。
-- 2乗は Integer で確かめる。maxBound の近くでは、
-- (r + 1) * (r + 1) が Int だと一周してしまう
isqrt :: Int -> Int
isqrt n = fix guess
  where
    guess    = floor (sqrt (fromIntegral n :: Double))
    big      = toInteger n
    square r = toInteger r * toInteger r
    fix r
      | square r       >  big = fix (r - 1)
      | square (r + 1) <= big = fix (r + 1)
      | otherwise             = r
-- ANCHOR_END: isqrt
