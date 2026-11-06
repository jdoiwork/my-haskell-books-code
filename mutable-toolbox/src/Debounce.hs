-- ANCHOR: all
-- | Save once things calm down: changes that come close together go
--   into one save (debounce). Like the review board of this book.
-- 落ち着いてから保存する：続けて来た変更を、1回の保存にまとめる
-- （デバウンス）。本書のレビューボードと同じ考え方
module Debounce
  ( Debouncer
  , newDebouncer
  , noteChange
  , debounceDemo
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (unless)
import Data.IORef
import Data.Text (Text)

-- ANCHOR: debouncer
-- 💾 What the debouncer keeps.
-- デバウンサーが持っているもの
data Debouncer = Debouncer
  { quietTime :: Int                -- ^ wait this long (μs) / 待つ時間
  , pending   :: MVar (Int, [Text]) -- ^ generation and changes / 世代と変更
  , saveLock  :: MVar ()            -- ^ one save at a time / 保存は1人ずつ
  , save      :: [Text] -> IO ()    -- ^ how to save / 保存のしかた
  }

newDebouncer :: Int -> ([Text] -> IO ()) -> IO Debouncer
newDebouncer quiet saveAll = do
  st   <- newMVar (0, [])
  lock <- newMVar ()
  pure (Debouncer quiet st lock saveAll)
-- ANCHOR_END: debouncer

-- ANCHOR: note
-- 📝 Note a change. Save when no other change comes for a while.
-- 変更を書き留める。しばらくほかの変更が来なければ保存する
noteChange :: Debouncer -> Text -> IO ()
noteChange d change = do
  -- A new generation: "the latest change is mine".
  -- 世代を1つ進める：「最新の変更は、わたしのもの」
  mine <- modifyMVar (pending d) $ \(g, changes) ->
    pure ((g + 1, changes ++ [change]), g + 1)
  _ <- forkIO $ do
    threadDelay (quietTime d)
    -- Still the latest? Then take all the changes and save them.
    -- まだ自分が最新？ なら、変更を全部もらって保存する
    ready <- modifyMVar (pending d) $ \(g, changes) ->
      if g == mine
        then pure ((g, []), changes)
        else pure ((g, changes), [])
    unless (null ready) $
      withMVar (saveLock d) (\_ -> save d ready)
  pure ()
-- ANCHOR_END: note

-- ANCHOR: demo
-- 🧪 Three quick changes, a pause, then one more.
-- すばやく3つ変えて、ひと休みして、もう1つ
debounceDemo :: IO [[Text]]
debounceDemo = do
  saved <- newIORef []
  let saveAll changes = modifyIORef' saved (++ [changes])
  d <- newDebouncer 200000 saveAll
  mapM_ (\c -> noteChange d c >> threadDelay 50000) ["a", "b", "c"]
  threadDelay 500000
  noteChange d "d"
  threadDelay 500000
  readIORef saved
-- ANCHOR_END: demo
-- ANCHOR_END: all
