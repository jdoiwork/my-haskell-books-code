-- A draft that does not compile (the training of the chapter).
-- コンパイルが通らない下書き（章のトレーニング）
module SpeedBonus where

-- ANCHOR: speedBonus
-- 🏃 The speed bonus grows with the square root of the agility.
-- 素早さのボーナスは、素早さの平方根で増える
speedBonus :: Int -> Double
speedBonus agility = sqrt agility
-- ANCHOR_END: speedBonus
