-- ANCHOR: all
-- | Read PCRE2's tests, run them all, and show how many tests have a
--   pattern the parser could read, how many tests passed, and what the
--   parser is missing most.
-- PCRE2 のテストを読んで全部動かし、パーサーがパターンを読めた問題の
-- 数、通った問題の数、パーサーに足りないもの（多い順）を見せる
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
  let total = length tests
      Tally readOk misread _ = tally (map readable tests)
      verdicts            = map judge tests
      Tally ok bad later  = tally verdicts
  printf "parsed:  %4d / %d (%.1f%%), misread %d\n"
    readOk total (percent readOk total) misread
  printf "passed:  %4d / %d (%.1f%%)\n" ok total (percent ok total)
  printf "failed:  %4d\n" bad
  printf "not yet: %4d\n" later
  putStrLn "the parser wants:"
  mapM_ showWanted (take 5 (mostWanted (map readable tests)))

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
