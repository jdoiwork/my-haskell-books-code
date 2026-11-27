-- | Time the three ways on n made-up quests.
--     cabal run quests-measure -- spec 20
-- 3つのやり方を、作りものの n 個の依頼で測る
module Main (main) where

import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)

import Review.Quests

-- 🎲 Made-up quests, spread over the day without randomness.
-- 乱数を使わずに、1日に散らばらせた作りものの依頼
madeUp :: Int -> [Quest]
madeUp n =
  [ Quest "q" s (s + 1 + i `mod` 7) (toInteger (1 + (i * 37) `mod` 50))
  | i <- [1 .. n]
  , let s = (i * 7919) `mod` (n * 2 `div` 3 + 1)
  ]

main :: IO ()
main = do
  args <- getArgs
  (run, version, n) <- case args of
    [version, size]
      | [(n, "")] <- reads size, n >= 0
      , Just run <- lookup version ways -> pure (run, version, n)
    -- A typo must not measure another way.
    -- 打ちまちがいで、別のやり方を測らないように
    _ -> die "usage: quests-measure spec|slow|fast <number of quests>"
  let qs = madeUp n
  t0 <- getMonotonicTime
  let answer = run qs
  printf "%s: %d" version answer
  t1 <- getMonotonicTime
  printf " (%.3f s)\n" (t1 - t0)
  where
    ways = [ ("spec", bestRewardSpec)
           , ("slow", bestRewardSlow)
           , ("fast", bestReward) ]
