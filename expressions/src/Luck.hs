-- ANCHOR: all
module Luck where

-- Random numbers (from the random package)
-- 乱数（random パッケージ）
import System.Random (randomRIO)

-- The damage calculation of this chapter
-- この章のダメージ計算
import Battle (critical, damage)

-- 🎲 Attack with a 10% chance of a critical hit.
-- 10% の確率で会心の一撃が出る攻撃
attackWithLuck :: Int -> Int -> IO Int
attackWithLuck attack defense = do
  -- A random number from 0 to 1, like random.random() in Python
  -- 0 から 1 までの乱数（Python の random.random() と同じ）
  luck <- randomRIO (0, 1 :: Double)
  -- The pure functions do the rest.
  -- 残りの計算は、純粋な関数にまかせる
  let isCritical = luck < 0.1
  pure (critical isCritical (damage attack defense))
-- ANCHOR_END: all
