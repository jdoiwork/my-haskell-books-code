-- ANCHOR: head
-- ANCHOR: all
-- | A live index: searches keep going while a new index is built
--   in the background, then the new one is swapped in at once.
-- 生きているインデックス：裏で新しいインデックスを作っているあいだも
-- 検索は続き、できたら一瞬で差しかえる。
module MiniSearch.Live
  ( Live
  , newLive
  , liveTokenizer
  , currentIndex
  , startReindex
  , waitIdle
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.DeepSeq (force)
import Control.Exception (evaluate, finally, mask)
import Control.Monad (void, when)

import MiniSearch.Index (buildIndexParallelWith)
import MiniSearch.Tokenizer (Tokenizer)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: types
-- 🌱 The index now, whether a rebuild is running,
--    and where the documents come from.
-- いまのインデックス、作り直しの最中かどうか、文書の出どころ
data Live = Live
  { liveTok    :: Tokenizer
  , liveIndex  :: TVar Index
  , liveBusy   :: TVar Bool
  , liveSource :: IO [Document]
  }

-- 🔤 The Tokenizer the index was built with. A function, not a field:
--    with an exported field, `live { liveTokenizer = japanese }` would
--    search an English index with Japanese tokens.
-- インデックスを作ったときの Tokenizer。フィールドではなく関数にする：
-- フィールドを export すると、live { liveTokenizer = japanese } で、
-- 英語のインデックスを日本語のトークンで引けてしまう
liveTokenizer :: Live -> Tokenizer
liveTokenizer = liveTok

-- 🏗️ Read the documents and build the first index.
-- 文書を読んで、最初のインデックスを作る
newLive :: Tokenizer -> IO [Document] -> IO Live
newLive tok source = do
  docs  <- source
  index <- evaluate (force (buildIndexParallelWith tok 1000 docs))
  Live tok <$> newTVarIO index <*> newTVarIO False <*> pure source

-- 👀 The index to search now. Never waits.
-- いま検索に使うインデックス。待たされることはない
currentIndex :: Live -> IO Index
currentIndex live = readTVarIO (liveIndex live)
-- ANCHOR_END: types

-- ANCHOR: reindex
-- 🔄 Start a rebuild in the background.
--    False if one is already running (we do not start two).
-- 裏で作り直しを始める。
-- もう走っていれば False（2つ同時には始めない）
startReindex :: Live -> IO Bool
startReindex live = mask $ \restore -> do
  -- From "busy" to the finally, nothing can stop us (mask).
  -- 「使用中」にしてから finally を付けるまで、止められない（mask）
  started <- atomically $ do
    busy <- readTVar (liveBusy live)
    if busy
      then pure False
      else do
        writeTVar (liveBusy live) True
        pure True
  -- Only the rebuild itself can be stopped (restore).
  -- 止めてよいのは、作り直しそのものだけ（restore）
  when started $
    void (forkIO (restore rebuild `finally` done))
  pure started
  where
    -- Whether it worked or failed, the rebuild is over.
    -- うまくいっても失敗しても、作り直しは終わり
    done = atomically (writeTVar (liveBusy live) False)

    rebuild = do
      docs  <- liveSource live
      -- Build it completely BEFORE swapping, so that searches
      -- never wait for a half-built index.
      -- 差しかえる「前」に、全部作りきる。
      -- 検索が、作りかけのインデックスを待つことはない
      index <- evaluate (force (buildIndexParallelWith (liveTokenizer live)
                                                       1000 docs))
      atomically (writeTVar (liveIndex live) index)

-- ⏳ Wait until no rebuild is running (for tests).
-- 作り直しが終わるまで待つ（テスト用）
waitIdle :: Live -> IO ()
waitIdle live = atomically $ do
  busy <- readTVar (liveBusy live)
  check (not busy)
-- ANCHOR_END: reindex
-- ANCHOR_END: all
