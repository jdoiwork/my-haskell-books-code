-- ANCHOR: all
-- | Moving gold between two purses, and tripping halfway.
--   What is left in the boxes?
-- 2つの財布のあいだで金貨を動かし、途中で転ぶ。箱には何が残る？
module Stumble
  ( Stumbled (..)
  , moveIORef
  , moveMVar
  , moveTVar
  , stumbleDemo
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO, try)
import Data.IORef

-- ANCHOR: move
-- 💥 Tripped halfway.
-- 途中で転んだ
data Stumbled = Stumbled
  deriving (Show)

instance Exception Stumbled

-- 🤕 Trip over (in IO, and in STM).
-- 転ぶ（IO で、STM で）
stumble :: IO ()
stumble = throwIO Stumbled

stumbleSTM :: STM ()
stumbleSTM = throwSTM Stumbled

-- 📦 Two IORefs: take 30 out, trip, (never) put 30 in.
-- IORef が2つ：30 を出して、転んで、（入れられない）
moveIORef :: IORef Int -> IORef Int -> IO ()
moveIORef from to = do
  modifyIORef' from (subtract 30)
  stumble
  modifyIORef' to (+ 30)

-- 🔒 Both purses in one MVar.
-- 両方の財布を、1つの MVar に
moveMVar :: MVar (Int, Int) -> IO ()
moveMVar purses =
  modifyMVar_ purses $ \(from, to) -> do
    let from' = from - 30
    stumble
    pure (from', to + 30)

-- ⚛️ Two TVars, in one atomically.
-- TVar が2つ。1つの atomically で
moveTVar :: TVar Int -> TVar Int -> IO ()
moveTVar from to = atomically $ do
  modifyTVar' from (subtract 30)
  stumbleSTM
  modifyTVar' to (+ 30)
-- ANCHOR_END: move

-- ANCHOR: demo
-- 🧪 Start with 100 and 0, trip, and look in the purses.
-- 100 と 0 から始めて、転んで、財布の中を見る
stumbleDemo :: IO [(Int, Int)]
stumbleDemo = do
  a <- newIORef 100
  b <- newIORef 0
  _ <- try @Stumbled (moveIORef a b)
  viaIORef <- (,) <$> readIORef a <*> readIORef b

  purses <- newMVar (100, 0)
  _ <- try @Stumbled (moveMVar purses)
  viaMVar <- readMVar purses

  c <- newTVarIO 100
  d <- newTVarIO 0
  _ <- try @Stumbled (moveTVar c d)
  viaTVar <- (,) <$> readTVarIO c <*> readTVarIO d

  pure [viaIORef, viaMVar, viaTVar]
-- ANCHOR_END: demo
-- ANCHOR_END: all
