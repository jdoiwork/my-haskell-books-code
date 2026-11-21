-- ANCHOR: all
-- | Read PCRE2's tests, run them all, and show, for each book of tests,
--   how many patterns the parser read and how many tests passed; then
--   what MiniRegex is missing most.
-- PCRE2 のテストを読んで全部動かし、問題集ごとに、パーサーが読めた
-- パターンの数と、通ったテストの数を見せる。最後に、MiniRegex に
-- 足りないもの（多い順）
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
-- 📚 The two books of tests: without UTF (Latin-1), and with UTF.
-- 2冊の問題集：UTF なし（Latin-1）と、UTF あり
books :: [(FilePath, Encoding)]
books =
  [ ("testdata/testoutput1", Latin1)
  , ("testdata/testoutput4", Utf8) ]

-- 📟 cabal run compat-meter [-- FILE]
main :: IO ()
main = do
  args <- getArgs
  let chosen = case args of
        f : _ -> [(f, if "4" `T.isSuffixOf` T.pack f then Utf8 else Latin1)]
        []    -> books
  verdicts <- concat <$> mapM book chosen
  putStrLn "most wanted:"
  mapM_ showWanted (take 5 (mostWanted verdicts))

-- 📖 One book: read it, run every test (a million steps at most for
--    each search), and print its lines.
-- 1冊ぶん：読んで、全部のテストを動かし（1回の探しものに多くても
-- 100万ステップ）、行を書く
book :: (FilePath, Encoding) -> IO [Verdict]
book (file, enc) = do
  tests <- readTestFile enc file
  let verdicts = map judge tests
  let total = length tests
      Tally readOk misread _ = tally (map readable tests)
      Tally ok bad later     = tally verdicts
  putStrLn (file ++ ":")
  printf "  parsed:  %4d / %d (%.1f%%), misread %d\n"
    readOk total (percent readOk total) misread
  printf "  passed:  %4d / %d (%.1f%%)\n" ok total (percent ok total)
  printf "  failed:  %4d\n" bad
  printf "  not yet: %4d\n" later
  pure verdicts

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
