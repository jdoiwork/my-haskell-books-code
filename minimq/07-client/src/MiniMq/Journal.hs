-- ANCHOR: all
-- | The journal: one JSON line per fact, written by a single writer,
--   read back (and made short again) when the broker starts.
-- 日誌：事実1つにつき JSON を1行。書き係は1人だけ。
-- ブローカーが始まるときに読み戻し、短く書き直す
module MiniMq.Journal
  ( Journal
  , newJournal
  , handIn
  , handedIn
  , onDisk
  , writer
  , encodeEntries
  , decodeEntries
  , loadJournal
  , rewriteJournal
  ) where

import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist, renameFile)
import System.IO

import MiniMq.Types

-- ANCHOR: journal
-- 📒 The journal as the server sees it: lines waiting for the writer,
--    how many lines were handed in, and how many are safe on disk.
-- サーバーから見た日誌：書き係を待っている行、手渡した行の数、
-- ディスクに無事に書けた行の数
data Journal = Journal
  { waitingLines :: TQueue Entry
  , handedIn     :: TVar Int
  , onDisk       :: TVar Int
  }

newJournal :: IO Journal
newJournal = Journal <$> newTQueueIO <*> newTVarIO 0 <*> newTVarIO 0

-- ✋ Hand lines to the writer, inside the caller's transaction, so they
--    go in exactly the order the changes happened.
-- 行を書き係に手渡す。呼んだ側の取引の中で。だから、変えた順の
-- とおりに並ぶ
handIn :: Journal -> [Entry] -> STM ()
handIn journal entries = do
  mapM_ (writeTQueue (waitingLines journal)) entries
  modifyTVar' (handedIn journal) (+ length entries)
-- ANCHOR_END: journal

-- ANCHOR: writer
-- ✍️ The only writer. It takes everything waiting, writes it with one
--    flush, then says how far the disk has got. Ten publishers at once
--    cost one flush, not ten.
-- ただ1人の書き係。待っている行を全部取り、1回の flush で書いて、
-- ディスクがどこまで進んだかを知らせる。10人が同時に送っても、
-- flush は10回ではなく1回
writer :: Maybe Handle -> Journal -> IO ()
writer target journal = forever $ do
  batch <- atomically $ do
    entries <- flushTQueue (waitingLines journal)
    check (not (null entries))
    pure entries
  case target of
    Just handle -> BS.hPut handle (encodeEntries batch) >> hFlush handle
    Nothing     -> pure ()                          -- no file: forget them
  atomically (modifyTVar' (onDisk journal) (+ length batch))
-- ANCHOR_END: writer

-- ANCHOR: file
-- 📄 One JSON value per line. A line cut short by a crash does not
--    decode, and is simply skipped.
-- 1行に JSON を1つ。落ちて途中で切れた行は読めないので、読み飛ばす
encodeEntries :: [Entry] -> ByteString
encodeEntries = foldMap (\e -> LBS.toStrict (encode e) <> "\n")

decodeEntries :: ByteString -> [Entry]
decodeEntries = mapMaybe decodeStrict . BC.lines

loadJournal :: FilePath -> IO [Entry]
loadJournal path = do
  exists <- doesFileExist path
  if exists then decodeEntries <$> BS.readFile path else pure []

-- 🧹 Write a whole new journal next to the old one, then swap them.
--    A crash in the middle leaves the old journal as it was.
-- 新しい日誌を、古いものの隣に丸ごと書いてから、入れ替える。
-- 途中で落ちても、古い日誌はそのまま残る
rewriteJournal :: FilePath -> [Entry] -> IO ()
rewriteJournal path entries = do
  let fresh = path <> ".new"
  BS.writeFile fresh (encodeEntries entries)
  renameFile fresh path
-- ANCHOR_END: file

-- ANCHOR_END: all
