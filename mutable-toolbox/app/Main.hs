-- ANCHOR: all
-- | Run the demos of the chapter that need threads.
-- この章の、スレッドの要るデモを動かす
module Main (main) where

import Data.List (sort)
import qualified Data.Text as T
import System.Environment (getArgs)

import Appraise (appraiseRush)
import Board    (guildDay)
import Debounce (debounceDemo)
import Gather   (gatherDemo)
import Guild    (newEnv, runApp, welcomeAll)
import Handoff  (countMVarLazy, countMVarStrict)
import Stumble  (stumbleDemo)
import Tickets  (ticketRush)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["rush"]         -> ticketRush >>= print
    ["appraise", h]  -> appraiseRush h
    ["mvar-lazy"]    -> countMVarLazy 10000000 >>= print
    ["mvar-strict"]  -> countMVarStrict 10000000 >>= print
    ["debounce"]     -> debounceDemo >>= print
    ["gather"]       -> do
      gatherDemo ["Lambda", "Python", "Rust"] 1000000 >>= print
      gatherDemo ["Lambda", "Python"] 500000 >>= print
    ["day"]          -> do
      let quests = ["quest-" <> T.pack (show i) | i <- [1 .. 9 :: Int]]
      results <- guildDay 3 quests
      print (length results)
      print (sort (map snd results) == sort quests)
    ["stumble"]      -> stumbleDemo >>= print
    ["welcome"]      -> do
      env <- newEnv
      tickets <- runApp env (welcomeAll ["Lambda", "Python", "Rust"])
      print (sort tickets)
    -- 「使い方：toolbox rush|appraise ...|mvar-lazy|...」
    _ -> putStrLn usage

usage :: String
usage = concat
  [ "usage: toolbox rush|appraise ioref|appraise mvar|appraise once"
  , "|mvar-lazy|mvar-strict|debounce|gather|day|stumble|welcome"
  ]
-- ANCHOR_END: all
