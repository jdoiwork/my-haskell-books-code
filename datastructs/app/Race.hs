-- ANCHOR: all
-- 🏁 A race of data structures. The times depend on your computer;
--    look at how they grow, not at the numbers themselves.
-- データ構造の競走。時間はパソコンによって違うので、
-- 数そのものより、増え方を見る
module Main (main) where

-- ANCHOR: imports
import Control.Exception   (evaluate)
import Data.IntMap.Strict  qualified as IntMap
import Data.Map.Strict     qualified as Map
import Data.Sequence       ((|>))
import Data.Sequence       qualified as Seq
import Data.Vector.Unboxed qualified as VU
import GHC.Clock           (getMonotonicTime)
import Text.Printf         (printf)
-- ANCHOR_END: imports

-- ANCHOR: timeIt
-- ⏱️ Run an action, and say how many milliseconds it took.
-- 仕事を動かして、何ミリ秒かかったかを伝える
timeIt :: String -> IO Int -> IO ()
timeIt name action = do
  start  <- getMonotonicTime
  result <- action >>= evaluate
  end    <- getMonotonicTime
  printf "%-24s %8.1f ms  (%d)\n" name ((end - start) * 1000) result
-- ANCHOR_END: timeIt

-- ANCHOR: lookups
-- 🔍 Look up 10,000 positions in 100,000 items.
-- 10万個の中から、1万回、番号で取り出す
lookups :: IO ()
lookups = do
  let n       = 100000
      wanted  = [ (i * 7919) `mod` n | i <- [1 .. 10000] ]
      list    = [0 .. n - 1] :: [Int]
      vector  = VU.fromList list
      seqLine = Seq.fromList list
      intMap  = IntMap.fromList (zip list list)
  _ <- evaluate (sum list + VU.sum vector + sum seqLine + sum intMap)
  timeIt "list    (!!)"     (pure (sum [ list !! i           | i <- wanted ]))
  timeIt "Seq     (index)"  (pure (sum [ Seq.index seqLine i | i <- wanted ]))
  timeIt "IntMap  (lookup)" (pure (sum [ intMap IntMap.! i   | i <- wanted ]))
  timeIt "Vector  (!)"      (pure (sum [ vector VU.! i       | i <- wanted ]))
-- ANCHOR_END: lookups

-- ANCHOR: appends
-- ➕ Add 20,000 items, one at a time, at the back.
-- 2万個を、1つずつ後ろに足していく
appends :: IO ()
appends = do
  let items      = [1 .. 20000] :: [Int]
      toList     = foldl' (\xs x -> xs ++ [x]) [] items
      toSeq      = foldl' (|>) Seq.empty items
      toMap      = foldl' (\m x -> Map.insert x x m) Map.empty items
  timeIt "list    (xs ++ [x])" (pure (sum toList))
  timeIt "Seq     (|>)"        (pure (sum toSeq))
  timeIt "Map     (insert)"    (pure (sum toMap))
-- ANCHOR_END: appends

-- ANCHOR: main
main :: IO ()
main = do
  putStrLn "-- lookups / 番号で取り出す"
  lookups
  putStrLn "-- appends / 後ろに足す"
  appends
-- ANCHOR_END: main
-- ANCHOR_END: all
