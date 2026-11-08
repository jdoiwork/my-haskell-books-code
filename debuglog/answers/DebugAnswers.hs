-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- Answers for the training of the debugging and logging chapter.
-- デバッグとログの章のトレーニングの解答
module DebugAnswers
  ( totalDamage
  , safeGain
  , teeLogger
  ) where

import Data.Text qualified as T
import Debug.Trace (traceShowId)

import Logger
-- ANCHOR_END: imports

-- ANCHOR: totalDamage
-- 🔍 traceShowId shows each value as it passes through.
-- traceShowId は、通り過ぎる値をそのまま表示する
totalDamage :: [Int] -> Int
totalDamage hits = sum (map traceShowId (filter (> 0) hits))
-- ANCHOR_END: totalDamage

-- ANCHOR: safeGain
-- 🚨 Negative exp is a mistake in the data: log it as an Error,
--    and count it as 0.
-- マイナスの経験値はデータのまちがい：Error で記録して、0 として扱う
safeGain :: Logger -> Int -> IO Int
safeGain logger gain
  | gain < 0 = do
      logError logger ("negative exp: " <> T.pack (show gain))
      pure 0
  | otherwise = pure gain
-- ANCHOR_END: safeGain

-- ANCHOR: teeLogger
-- 🔀 One logger that writes to two loggers.
-- 2つのロガーに書く、1つのロガー
teeLogger :: Logger -> Logger -> Logger
teeLogger a b = Logger write
  where
    write level msg = do
      logAt a level msg
      logAt b level msg
-- ANCHOR_END: teeLogger
-- ANCHOR_END: all
