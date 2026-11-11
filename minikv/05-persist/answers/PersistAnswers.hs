-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of "Persistence".
-- 「永続化」のトレーニングの解答
module PersistAnswers
  ( compactLog
  ) where

import Control.Concurrent (modifyMVarMasked_)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (onException)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import System.Directory (renameFile)
import System.IO (IOMode (..), hClose, hFlush, openFile)

import MiniKv.Persist (rewriteLog)
import MiniKv.Server (Env (..))
-- ANCHOR_END: head

-- ANCHOR: compact
-- 🗜️ Step 2: make the log short while the server keeps running.
--    Holding the log's lock, no command can change the store, so the
--    store we read is exactly what the log says. Write it to a new
--    file, put the new file in the old one's place (a rename is all
--    or nothing), and keep writing to the new file.
-- ステップ2：サーバーを動かしたまま、ログを短くする。
-- ログの錠をとっているあいだ、ストアを変えるコマンドは動けないので、
-- 読んだストアは、ログの中身とぴったり同じ。それを新しいファイルに書き、
-- 古いファイルと置き換え（名前の付け替えは、全部か無しか）、
-- そのあとは新しいファイルに書き足していく
compactLog :: Env -> FilePath -> IO ()
compactLog env path = forM_ (journal env) $ \lock ->
  modifyMVarMasked_ lock $ \old -> do
    store <- readTVarIO (storeVar env)
    let fresh = path <> ".new"
    new <- openFile fresh WriteMode
    ( do BS.hPut new (rewriteLog store)
         hFlush new
         renameFile fresh path ) `onException` hClose new
    -- The new handle still points at the file, now under its new name.
    -- 新しい Handle は、名前が変わったあとも、同じファイルを指している
    hClose old
    pure new
-- ANCHOR_END: compact
-- ANCHOR_END: all
