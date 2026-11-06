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
-- 🔢 The square root of n, rounded down, exactly.
--    sqrt on a Double may be one off for a big n,
--    so the answer is fixed by one step at a time.
-- n の平方根の切り捨てを、正確に求める。
-- 大きな n では Double の sqrt が 1 ずれることがあるので、1 ずつ直す
isqrt :: Int -> Int
isqrt n = fix guess
  where
    guess = floor (sqrt (fromIntegral n :: Double))
    fix r
      | r * r > n              = fix (r - 1)
      | (r + 1) * (r + 1) <= n = fix (r + 1)
      | otherwise              = r
-- ANCHOR_END: isqrt
