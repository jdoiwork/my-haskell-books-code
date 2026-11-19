-- ANCHOR: all
-- | Read PCRE2's tests, run them all, and show how many patterns the
--   parser read, how many tests passed, and what MiniRegex is missing
--   most.
-- PCRE2 のテストを読んで全部動かし、パーサーが読めたパターンの数、
-- 通ったテストの数、MiniRegex に足りないもの（多い順）を見せる
module Main (main) where

-- ANCHOR: imports
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import Text.Printf (printf)

import MiniRegex.Compat
-- ANCHOR_END: imports

-- ANCHOR: main
-- 📟 cabal run compat-meter [-- FILE]
main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        f : _ -> f
        []    -> "testdata/testoutput1"
  tests <- readTestFile file
  -- one second at most for each test
  -- 1問に、長くても1秒
  verdicts <- mapM (judgeWithin 1000000) tests
  let total = length tests
      Tally readOk misread _ = tally (map readable tests)
      Tally ok bad later     = tally verdicts
  printf "parsed:  %4d / %d (%.1f%%), misread %d\n"
    readOk total (percent readOk total) misread
  printf "passed:  %4d / %d (%.1f%%)\n" ok total (percent ok total)
  printf "failed:  %4d\n" bad
  printf "not yet: %4d\n" later
  putStrLn "most wanted:"
  mapM_ showWanted (take 5 (mostWanted verdicts))

-- 💯 n of all, as a percent.
-- 全部のうちの n を、百分率で
percent :: Int -> Int -> Double
percent n total = 100 * fromIntegral n / fromIntegral total

-- 🏆 What the "not yet" tests need, the most often needed first.
-- 「まだ」のテストに要るもの。多い順
mostWanted :: [Verdict] -> [(T.Text, Int)]
mostWanted verdicts = sortOn (Down . snd) (Map.toList counts)
  where
    counts = Map.fromListWith (+) [(what, 1) | NotYet what <- verdicts]

showWanted :: (T.Text, Int) -> IO ()
showWanted (what, n) = do
  printf "  %4d  " n
  TIO.putStrLn what
-- ANCHOR_END: main
-- ANCHOR_END: all
