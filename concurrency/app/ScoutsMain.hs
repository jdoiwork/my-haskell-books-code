-- ANCHOR: all
-- 🔭 Send out three scouts, one by one and all at once.
-- 3人の偵察隊を、1人ずつと、いっせいに送る
module Main (main) where

import GHC.Clock   (getMonotonicTime)
import Text.Printf (printf)

import Scouts

places :: [(String, Int)]
places = [("forest", 300), ("cave", 200), ("tower", 100)]

-- ⏱️ Run an action, and print how long it took.
-- 仕事を動かして、かかった時間を出す
timed :: String -> IO a -> IO a
timed name action = do
  start  <- getMonotonicTime
  result <- action
  end    <- getMonotonicTime
  printf "%s: %.1f s\n" name (end - start)
  pure result

main :: IO ()
main = do
  one <- timed "one by one" (scoutsOneByOne places)
  together <- timed "all at once" (scoutsAtOnce places)
  mapM_ putStrLn together
  print (one == together)
  scoutWithin 150 ("forest", 300) >>= print
  scoutWithin 150 ("tower", 100) >>= print
-- ANCHOR_END: all
