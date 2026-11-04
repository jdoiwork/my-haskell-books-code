-- ANCHOR: all
-- | Time the four versions of bestRun on n rooms.
--     cabal run algo-measure -- thinking 0 2000
-- bestRun の4つの版を、n 部屋で測る
module Main (main) where

import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import Text.Printf (printf)

import Algo.Divide
import Algo.Dp
import Algo.Purely
import Algo.Search
import Algo.Strings
import Algo.Thinking

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["thinking", version, size] -> thinking version size
    ["search", version]         -> search version
    ["divide", version, size]   -> divide version size
    ["dp", what, version]       -> dp what version
    ["strings", version]        -> strings version
    ["purely", version, size]   -> purely version size
    _                           -> putStrLn "unknown"

-- 🎟️ Push n numbers into a queue, then pop them all.
-- キューに n 個並べてから、全部取り出す
purely :: String -> String -> IO ()
purely version size = do
  let n = read size :: Int
  t0 <- getMonotonicTime
  let total = if version == "slow"
        then sum (drainSlow (foldl (flip slowPush) slowEmpty [1 .. n]))
        else sum (drain (queueFromList [1 .. n]))
  printf "%s: sum %d" version total
  t1 <- getMonotonicTime
  printf " (%.3f s)\n" (t1 - t0)
  where
    drainSlow q = maybe [] (\(x, q') -> x : drainSlow q') (slowPop q)

-- 🔎 Search for 1000 a's and a b in 200000 a's (it is not there).
-- a が 20 万個並んだ文で、a 1000 個と b を探す（見つからない）
strings :: String -> IO ()
strings version = do
  let text = replicate 200000 'a'
      pat  = replicate 1000 'a' ++ "b"
      found = if version == "spec" then findAllSpec pat text
                                   else findAll pat text
  t0 <- getMonotonicTime
  t1 <- length found `seq` getMonotonicTime
  printf "%s: %d found (%.3f s)\n" version (length found) (t1 - t0)

-- 📋 Time a DP problem with the spec or the table.
-- 動的計画法の問題を、仕様か表で測る
dp :: String -> String -> IO ()
dp what version = do
  let spec = version == "spec"
      coins = [1, 5, 10, 25, 50]
      answer = case what of
        "ways" -> show ((if spec then waysSpec else ways) coins 1000)
        _      -> show ((if spec then editDistanceSpec else editDistance)
                          "goblinkingdom" "dragonslayers")
  t0 <- getMonotonicTime
  t1 <- length answer `seq` getMonotonicTime
  printf "%s %s: %s (%.3f s)\n" what version answer (t1 - t0)

-- 🔀 Count the inversions of n numbers, with the spec or the fast one.
-- n 個の数の転倒数を、仕様か速い版で数える
divide :: String -> String -> IO ()
divide version size = do
  let n  = read size :: Int
      xs = [ (i * 7919) `mod` 100003 | i <- [1 .. n] ]
      count = if version == "spec" then inversionsSpec xs else inversions xs
  t0 <- getMonotonicTime
  t1 <- count `seq` getMonotonicTime
  printf "%s: %d inversions (%.3f s)\n" version count (t1 - t0)

-- 🔤 Time SEND + MORE = MONEY, slow or fast.
-- SEND + MORE = MONEY を、遅い版か速い版で測る
search :: String -> IO ()
search version = do
  t0 <- getMonotonicTime
  let answers = if version == "slow" then sendMoreMoneySlow else sendMoreMoney
  print answers
  t1 <- getMonotonicTime
  printf "%s: %.3f s\n" version (t1 - t0)

thinking :: String -> String -> IO ()
thinking version size = do
  let n     = read size :: Int
      rooms = [ (i * 7919) `mod` 21 - 10 | i <- [1 .. n] ]
      (name, run) = case version of
        "0" -> ("bestRun0", bestRun0)
        "1" -> ("bestRun1", bestRun1)
        "2" -> ("bestRun2", bestRun2)
        "3" -> ("bestRun3", bestRun3)
        _   -> ("bestRun",  bestRun)
  t0 <- getMonotonicTime
  let answer = run rooms
  t1 <- answer `seq` getMonotonicTime
  printf "%s: %d (%.3f s)\n" (name :: String) answer (t1 - t0)
-- ANCHOR_END: all
