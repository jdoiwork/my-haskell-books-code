-- ANCHOR: all
-- | Race the two engines: run the backtracking VM and the Pike VM at
--   the same time, and take the answer of the one that finishes first.
--   They give the same answers (the tests say so), so it does not
--   matter which one wins; only how soon. With the threaded runtime
--   and two cores or more (+RTS -N), the two really run side by side.
-- 2つのエンジンを競走させる：バックトラックの VM と Pike VM を同時に
-- 動かし、先に終わったほうの答えをもらう。2つは同じ答えを出す（テストが
-- そう言っている）ので、どちらが勝っても同じ。ちがうのは早さだけ。
-- スレッドの使える実行時システムで、コアが2つ以上（+RTS -N）なら、
-- 2つは本当に並んで動く
module MiniRegex.Race
  ( findRacing
  ) where

-- ANCHOR: imports
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (evaluate, throwIO)
import Control.Monad (forever)
import Data.Text (Text)

import MiniRegex.Engine (Runner, searchCountingBy)
import MiniRegex.Pike (blocker)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.STVM (scanVMST)
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: race
-- 🏁 The first match, from whichever engine is done first. If the
--    backtracking VM runs out of steps, it steps aside and waits: the
--    Pike VM goes on. A pattern the Pike VM cannot run has only the
--    backtracking VM.
-- 最初の一致を、先に終わったエンジンから。バックトラックの VM の手数が
-- 尽きたら、脇によけて待つ。Pike VM は続ける。Pike VM で動かせない
-- パターンは、バックトラックの VM だけ
findRacing :: Regex -> Text -> IO (Maybe Match)
findRacing re t = case blocker (program re) of
  Just _  -> attempt scanVMST >>= either throwIO pure
  Nothing -> either id id <$> race backtrack pike
  where
    backtrack = attempt scanVMST >>= either (const stepAside) pure
    pike      = attempt scanPikeST >>= either throwIO pure
    stepAside = forever (threadDelay maxBound)

    -- run one engine to the end (evaluate forces the whole search)
    -- エンジンを1つ、最後まで動かす（evaluate が、探しもの全体を動かす）
    attempt :: Runner -> IO (Either GaveUp (Maybe Match))
    attempt vm = evaluate (fst (searchCountingBy vm defaultLimit re t 0))
-- ANCHOR_END: race
-- ANCHOR_END: all
