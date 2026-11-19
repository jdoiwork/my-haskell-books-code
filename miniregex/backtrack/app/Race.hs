-- ANCHOR: all
-- | Race the two matchers: a*b on texts of a's (no b, so every place is
--   tried, and every candidate of a* at each of them).
-- 2つのマッチャーを競走させる：a だけの文字列に a*b を当てる（b がないので、
-- どの場所も試し、その場所ごとに a* の候補を全部試す）
module Main (main) where

-- ANCHOR: imports
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Text qualified as T
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import MiniRegex
import MiniRegex.Engine (searchFromList)
import MiniRegex.Examples (surely)
-- ANCHOR_END: imports

-- ANCHOR: main
-- 🏁 cabal run -v0 race
main :: IO ()
main = forM_ [250, 500, 1000] $ \n -> do
  let text = T.replicate n "a"
      re   = surely "a*b"
  -- make the text first, outside the time
  -- 文字列は先に作っておく（時間に入れない）
  _ <- evaluate (T.length text)
  list <- seconds (searchFromList re text 0)
  cont <- seconds (find re text)
  printf "n = %4d   list %7.3f s   continuation %6.3f s\n" n list cont

-- ⏱️ The CPU time to find out whether there is a match.
-- 一致があるかどうかを確かめるまでの CPU の時間
seconds :: Maybe Match -> IO Double
seconds answer = do
  t0 <- getCPUTime
  _ <- evaluate (fmap matchEnd answer)
  t1 <- getCPUTime
  pure (fromIntegral (t1 - t0) / 1e12)
-- ANCHOR_END: main
-- ANCHOR_END: all
