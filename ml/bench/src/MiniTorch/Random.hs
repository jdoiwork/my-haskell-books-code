-- ANCHOR: all
-- | A tiny random number generator that Python can copy exactly.
-- Python でも、まったく同じ数を出せる、小さな乱数。
module MiniTorch.Random
  ( uniforms
  ) where

import Data.Bits (shiftR)
import Data.Word (Word64)

-- 🎲 An endless list of numbers in [0, 1), from a seed.
--    (A linear congruential generator, modulo 2^64.)
-- 種から作る、0 以上 1 未満の数の、終わらないリスト
uniforms :: Word64 -> [Double]
uniforms seed = map toUnit (drop 1 (iterate next seed))
  where
    next x   = x * 6364136223846793005 + 1442695040888963407
    toUnit x = fromIntegral (x `shiftR` 11) / 9007199254740992
-- ANCHOR_END: all
