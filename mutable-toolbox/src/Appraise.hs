-- ANCHOR: all
-- | Remember appraisals: three caches, from simple to careful.
-- 鑑定の結果を覚える：素朴なものから、ていねいなものまで、3つのキャッシュ
module Appraise
  ( slowAppraise
  , cachedIORef
  , cachedMVar
  , Cache
  , newCache
  , cachedOnce
  , appraiseRush
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, try)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

-- ANCHOR: slow
-- 💎 Appraise an item. It takes 0.1 seconds, and we count the calls.
-- 品物を鑑定する。0.1秒かかる。何回鑑定したかを数えておく
slowAppraise :: IORef Int -> Text -> IO Int
slowAppraise calls item = do
  atomicModifyIORef' calls (\n -> (n + 1, ()))
  threadDelay 100000
  pure (T.length item * 100)
-- ANCHOR_END: slow

-- ANCHOR: ioref
-- 📒 Remember the prices in a Map, in an IORef.
-- 値段を Map に覚える。Map は IORef に入れておく
cachedIORef :: IORef (Map Text Int) -> (Text -> IO Int) -> Text -> IO Int
cachedIORef cache appraise item = do
  known <- readIORef cache
  case Map.lookup item known of
    Just price -> pure price
    Nothing    -> do
      price <- appraise item
      atomicModifyIORef' cache (\m -> (Map.insert item price m, ()))
      pure price
-- ANCHOR_END: ioref

-- ANCHOR: mvar
-- 🔒 The same, but holding the MVar while appraising.
-- 同じことを、MVar を握ったまま鑑定する
cachedMVar :: MVar (Map Text Int) -> (Text -> IO Int) -> Text -> IO Int
cachedMVar cache appraise item =
  modifyMVar cache $ \known ->
    case Map.lookup item known of
      Just price -> pure (known, price)
      Nothing    -> do
        price <- appraise item
        pure (Map.insert item price known, price)
-- ANCHOR_END: mvar

-- ANCHOR: once
-- 🗂️ For each item, a slot that will get its result (or its failure).
-- 品物ごとに、結果（か失敗）が入る予定の枠
type Cache = TVar (Map Text (TMVar (Either SomeException Int)))

newCache :: IO Cache
newCache = newTVarIO Map.empty

-- 🎯 Appraise each item once. Others asking for it wait for the slot.
-- 品物ごとに1回だけ鑑定する。ほかの人は、枠が埋まるのを待つ
cachedOnce :: Cache -> (Text -> IO Int) -> Text -> IO Int
cachedOnce cache appraise item = do
  -- Find the slot, or make one and say "I will appraise it".
  -- 枠を探す。なければ作って「わたしが鑑定します」と名乗る
  (slot, mine) <- atomically $ do
    known <- readTVar cache
    case Map.lookup item known of
      Just slot -> pure (slot, False)
      Nothing   -> do
        slot <- newEmptyTMVar
        writeTVar cache (Map.insert item slot known)
        pure (slot, True)
  if mine
    then do
      result <- try (appraise item)
      atomically $ do
        putTMVar slot result
        -- A failure is not remembered: the next one tries again.
        -- 失敗は覚えない。次の人が、もう一度鑑定する
        case result of
          Left _  -> modifyTVar' cache (Map.delete item)
          Right _ -> pure ()
    else pure ()
  result <- atomically (readTMVar slot)
  either throwIO pure result
-- ANCHOR_END: once

-- ANCHOR: rush
-- 🏃 Five ask for a sword and five for a shield, all at once.
--    How many appraisals, and how long?
-- 剣を5人、盾を5人が、同時に頼む。鑑定は何回で、何秒かかった？
appraiseRush :: String -> IO ()
appraiseRush how = do
  calls <- newIORef 0
  ask <- case how of
    "ioref" -> cachedIORef <$> newIORef Map.empty
    "mvar"  -> cachedMVar <$> newMVar Map.empty
    _       -> cachedOnce <$> newCache
  let items = replicate 5 "sword" ++ replicate 5 "shield"
  start <- getMonotonicTime
  forConcurrently_ items (ask (slowAppraise calls))
  end <- getMonotonicTime
  n <- readIORef calls
  -- 「ioref：鑑定 10 回、0.1 秒」
  printf "%s: %d appraisals, %.1f s\n" how n (end - start)
-- ANCHOR_END: rush
-- ANCHOR_END: all
