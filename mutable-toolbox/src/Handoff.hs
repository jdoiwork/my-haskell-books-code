-- ANCHOR: all
-- | MVar: a lock for the log, a box for one answer, and a thunk trap.
-- MVar：ログの錠、答えを1回だけ渡す箱、サンクの落とし穴
module Handoff
  ( newLogger
  , startWork
  , waitWork
  , countMVarLazy
  , countMVarStrict
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- ANCHOR: lock
-- 📢 A logger that writes one line at a time, even from many threads.
-- いくつものスレッドから呼んでも、1行ずつ書くロガー
newLogger :: IO (Text -> IO ())
newLogger = do
  lock <- newMVar ()
  pure (\line -> withMVar lock (\_ -> TIO.putStrLn line))
-- ANCHOR_END: lock

-- ANCHOR: once
-- 📮 Start some work, and get the box its answer will be put in.
-- 仕事を始めて、答えが入る予定の箱を受け取る
startWork :: IO a -> IO (MVar a)
startWork work = do
  box <- newEmptyMVar
  _ <- forkIO (work >>= putMVar box)
  pure box

-- ⏳ Wait until the answer is in the box. The answer stays in it.
-- 箱に答えが入るまで待つ。答えは箱に残る
waitWork :: MVar a -> IO a
waitWork = readMVar
-- ANCHOR_END: once

-- ANCHOR: thunk
-- 🐌 Count with modifyMVar_. The box fills up with promises.
-- modifyMVar_ で数える。箱に約束がたまっていく
countMVarLazy :: Int -> IO Int
countMVarLazy n = do
  box <- newMVar 0
  forM_ [1 .. n] $ \_ ->
    modifyMVar_ box (\k -> pure (k + 1))
  readMVar box

-- ⚡ The same, but adding before putting it back ($!).
-- 同じことを、箱に戻す前に足し算をすませて（$!）
countMVarStrict :: Int -> IO Int
countMVarStrict n = do
  box <- newMVar 0
  forM_ [1 .. n] $ \_ ->
    modifyMVar_ box (\k -> pure $! k + 1)
  readMVar box
-- ANCHOR_END: thunk
-- ANCHOR_END: all
