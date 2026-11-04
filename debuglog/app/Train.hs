{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T

import Leveling (levelFor)
import Logger

-- ⚔️ The exp the hero gains in each battle.
-- 勇者が、戦闘ごとに手に入れる経験値
battles :: [Int]
battles = [40, 80, 0, 120, 60, 200]

-- 🏋️ Train through all the battles, then show the result.
--    The result goes to standard output; the logs go to standard error.
-- すべての戦闘で修行して、結果を表示する
-- 結果は標準出力へ、ログは標準エラーへ
main :: IO ()
main = do
  minLevel <- levelFromEnv
  let logger = stderrLogger minLevel
  logInfo logger "training started"
  total <- train logger 0 battles
  logInfo logger "training finished"
  putStrLn ("Final level: " ++ show (levelFor total)
            ++ " (" ++ show total ++ " exp)")

-- 🔁 One battle at a time, logging what happens.
-- 1戦ずつ進めて、起きたことをログに残す
train :: Logger -> Int -> [Int] -> IO Int
train _ total [] = pure total
train logger total (gain : rest) = do
  let newTotal = total + gain
  logDebug logger
    ("gained " <> tshow gain <> " exp, total " <> tshow newTotal)
  if gain == 0
    then logWarn logger "a battle with no exp"
    else pure ()
  if levelFor newTotal > levelFor total
    then logInfo logger ("level up! now level " <> tshow (levelFor newTotal))
    else pure ()
  train logger newTotal rest

-- 🔢 show, as Text.
-- show の Text 版
tshow :: Int -> Text
tshow n = T.pack (show n)
-- ANCHOR_END: all
