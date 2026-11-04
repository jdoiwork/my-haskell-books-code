-- ANCHOR: all
-- 📊 Print the top three of the battle report.
-- バトルの報告の、上位3人を出す
module Main (main) where

import System.Environment (getArgs)

import Report

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["slow"] -> do
      print (length (heroesSlow battleLog))
      print (topThree (totalsSlow battleLog))
    ["fast"] -> do
      print (length (heroesFast battleLog))
      print (topThree (totalsFast battleLog))
    _ -> putStrLn "usage: report (slow|fast)"
-- ANCHOR_END: all
