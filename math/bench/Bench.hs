-- ANCHOR: all
-- | Is the magic number still faster than 1 / sqrt x?
-- 魔法の数は、いまでも 1 / sqrt x より速い？
module Main (main) where

import Test.Tasty.Bench

import Math.Numbers (fastInvSqrt)

-- 🔁 Add up f x for x = 1, 2, ..., n, in a strict loop.
-- x = 1, 2, …, n について f x を足す（正格なループで）
sumWith :: (Float -> Float) -> Int -> Float
sumWith f n = go 1 0
  where
    go :: Int -> Float -> Float
    go i !acc
      | i > n     = acc
      | otherwise = go (i + 1) (acc + f (fromIntegral i))

main :: IO ()
main =
  defaultMain
    [ -- Only the loop, to see how much of the time is the loop itself.
      -- ループだけ（時間のうち、ループそのものがどれだけかを見る）
      bench "loop only"   (whnf (sumWith id) 1000000)
    , bench "1 / sqrt x"  (whnf (sumWith (\x -> 1 / sqrt x)) 1000000)
    , bench "fastInvSqrt" (whnf (sumWith fastInvSqrt) 1000000)
    ]
-- ANCHOR_END: all
