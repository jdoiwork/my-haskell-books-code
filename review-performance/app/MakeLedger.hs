-- ANCHOR: all
-- 🏭 Make a big ledger to measure with: 1,000,000 lines, 1,000 heroes.
--    The same numbers every time (no random package needed).
-- 測るための大きな決算を作る：100万行、勇者1000人。
-- 毎回同じ数になる（乱数のパッケージは使わない）
module Main (main) where

import Data.ByteString.Builder qualified as B
import System.IO               (stdout)

main :: IO ()
main = B.hPutBuilder stdout (foldMap line [1 .. 1000000 :: Int])
  where
    line i =
      B.string7 "hero" <> B.intDec ((i * 7919) `mod` 1000)
        <> B.char7 ' ' <> B.intDec ((i * 37) `mod` 500 + 1) <> B.char7 '\n'
-- ANCHOR_END: all
