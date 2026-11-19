-- ANCHOR: all
-- | Time the engines, and the race between them, with a clock of our
--   own: each round gets a fresh copy of the text, so no round can
--   reuse what an earlier one worked out. The race runs on one core,
--   and then on two (setNumCapabilities).
-- エンジンと、そのあいだの競走を、自分で時計を見て測る。回ごとに、
-- 文字列の新しい写しを渡すので、前の回が求めたものを使い回せない。
-- 競走は、1つのコアで、それから2つのコアで動かす（setNumCapabilities）
module Main (main) where

-- ANCHOR: imports
import Control.Concurrent (setNumCapabilities)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)

import MiniRegex
import MiniRegex.Engine (Runner, scanSwitching, searchCountingBy)
import MiniRegex.Examples (surely)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.Race (findRacing)
import MiniRegex.STVM (scanVMST)
-- ANCHOR_END: imports

-- ANCHOR: main
-- ⏱️ The same texts as the benchmarks.
-- ベンチマークと同じ文字列
main :: IO ()
main = do
  printf "%-8s %9s %9s %9s %9s %9s\n"
    ("" :: String) ("vm-st" :: String) ("pike-st" :: String)
    ("switch" :: String) ("race 1" :: String) ("race 2" :: String)
  forM_ cases $ \(name, p, t) -> do
    let re = surely p
    setNumCapabilities 1
    vm     <- clock (\s -> run scanVMST re s) t
    pike   <- clock (\s -> run scanPikeST re s) t
    switch <- clock (\s -> run scanSwitching re s) t
    race1  <- clock (findRacing re) t
    setNumCapabilities 2
    race2  <- clock (findRacing re) t
    printf "%-8s %9s %9s %9s %9s %9s\n"
      name (showTime vm) (showTime pike) (showTime switch)
      (showTime race1) (showTime race2)

cases :: [(String, Text, Text)]
cases =
  [ ("dates", "\\b\\d{4}-\\d{2}-\\d{2}\\b"
    , T.replicate 200 "Room 1234-56-789 is closed\n" <> "2026-11-19")
  , ("many-a", "a*b", T.replicate 10000 "a" <> "b")
  , ("a-or-b", "(?:a|b)*c", T.replicate 5000 "ab" <> "c")
  , ("names", "^(\\w+\\s?)+$", "Mira Ruby Lam GHC Perl!")
  , ("gold", "\\d+ gold", T.replicate 2000 "12345 " <> "7 gold")
  , ("words", "\\w+@\\w+", T.replicate 2000 "Mira Ruby " <> "lam@ghc")
  ]
-- ANCHOR_END: main

-- ANCHOR: clock
-- 🕰️ The middle time of 31 rounds, in nanoseconds. Each round copies
--    the text first (not timed).
-- 31 回の、まん中の時間（ナノ秒）。どの回も、先に文字列を写す（そこは
-- 測らない）
clock :: (Text -> IO (Maybe Match)) -> Text -> IO Integer
clock search t = do
  times <- forM [1 .. 31 :: Int] $ \_ -> do
    s <- evaluate (T.copy t)
    start <- getMonotonicTimeNSec
    _ <- search s >>= evaluate . fmap matchEnd
    end <- getMonotonicTimeNSec
    pure (toInteger (end - start))
  pure (sort times !! 15)

-- 🏃 One VM by itself, as an IO action.
-- VM を1つだけ。IO の動きにして
run :: Runner -> Regex -> Text -> IO (Maybe Match)
run vm re s = case fst (searchCountingBy vm defaultLimit re s 0) of
  Right m -> pure m
  Left e  -> error (show e)

-- ⌚ 2.1 ms, 340 μs, 8 μs
showTime :: Integer -> String
showTime ns
  | ns >= 1000000 = printf "%.2f ms" (fromInteger ns / 1e6 :: Double)
  | otherwise     = printf "%d μs" (ns `div` 1000)
-- ANCHOR_END: clock
-- ANCHOR_END: all
