-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Files, commands and clocks: the
--   tools of scripts".
-- 「ファイルとコマンドと時計：スクリプトの道具」のトレーニングの解答。
module Lam.Answers
  ( withTimeout
  ) where

import qualified Data.Text as T
import System.Timeout (timeout)

import Lam.Eval
import Lam.Files (outcomeValue)
import Lam.Io (Handler)

-- ANCHOR: withTimeout
-- ⏳ Step 4: a handler around a handler, that gives each command some
--    milliseconds. When
--    the time is up, the command is stopped (readCreateProcess...
--    stops it when its thread is stopped), and the answer is
--    Failed 124, the exit code of the timeout command of GNU.
-- ステップ4：ハンドラーをくるむハンドラー。コマンドに、何ミリ秒かの
-- 持ち時間を与える。時間が
-- 来たら、コマンドを止める（スレッドが止まると、readCreateProcess… が
-- 止める）。答えは Failed 124（GNU の timeout コマンドの終了コード）
withTimeout :: Int -> Handler IO -> Handler IO
withTimeout ms handle request = case request of
  RunProcess command _ _ -> do
    answer <- timeout (ms * 1000) (handle request)
    pure $ case answer of
      Just v  -> v
      Nothing -> outcomeValue (Right (124, "",
        command <> ": no answer in " <> T.pack (show ms) <> " ms\n"))
  _ -> handle request
-- ANCHOR_END: withTimeout
-- ANCHOR_END: all
