-- ANCHOR: all
module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO

import Watch

-- 🌙 night-watch stubborn | careful: appraise ten treasures in 0.35 s.
-- night-watch stubborn か careful：0.35 秒で、10個の宝物を鑑定する
main :: IO ()
main = do
  -- Show each line at once (so Ctrl-C shows where we were).
  -- 1行ずつすぐ表示する（Ctrl-C のとき、どこまで進んだかわかるように）
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ["stubborn"] -> nightAppraisal appraiseStubborn >>= print
    ["careful"]  -> nightAppraisal appraiseCareful  >>= print
    _            -> die "usage: night-watch stubborn|careful"
-- ANCHOR_END: all
