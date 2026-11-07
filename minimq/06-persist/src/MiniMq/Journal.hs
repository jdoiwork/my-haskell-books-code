-- ANCHOR: all
-- ANCHOR: header
-- | The journal: one JSON line per transaction, written by a single
--   writer, read back (and made short again) when the broker starts.
-- 日誌：取引1つにつき JSON を1行。書き係は1人だけ。
-- ブローカーが始まるときに読み戻し、短く書き直す
module MiniMq.Journal
  ( Journal
  , newJournal
  , handIn
  , handedIn
  , onDisk
  , writer
  , encodeLines
  , decodeLines
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
-- ANCHOR_END: header

-- ANCHOR: journal
-- 📒 The journal as the server sees it: lines waiting for the writer,
--    how many lines were handed in, and how many are safe on disk.
--    One line holds everything one transaction changed.
-- サーバーから見た日誌：書き係を待っている行、手渡した行の数、
-- ディスクに無事に書けた行の数。1行には、1つの取引で変えたことを全部
data Journal = Journal
  { waitingLines :: TQueue [Entry]
  , handedIn     :: TVar Int
  , onDisk       :: TVar Int
  }

newJournal :: IO Journal
newJournal = Journal <$> newTQueueIO <*> newTVarIO 0 <*> newTVarIO 0

-- ✋ Hand one transaction's entries to the writer, as one line, inside
--    that transaction, so lines go in exactly the order the changes
--    happened. A step that changed nothing hands in nothing.
-- 1つの取引で書いたことを、1行にまとめて書き係に手渡す。その取引の
-- 中で。だから、変えた順のとおりに並ぶ。何も変えなければ、何も渡さない
handIn :: Journal -> [Entry] -> STM ()
handIn _       []      = pure ()
handIn journal entries = do
  writeTQueue (waitingLines journal) entries
  modifyTVar' (handedIn journal) (+ 1)
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
    waiting <- flushTQueue (waitingLines journal)
    check (not (null waiting))
    pure waiting
  case target of
    Just handle -> BS.hPut handle (encodeLines batch) >> hFlush handle
    Nothing     -> pure ()                          -- no file: forget them
  atomically (modifyTVar' (onDisk journal) (+ length batch))
-- ANCHOR_END: writer

-- ANCHOR: file
-- 📄 One line per transaction: its entries as one JSON array. A line
--    cut short by a crash does not decode, and is skipped as a whole,
--    so a transaction is either all there or not there at all.
-- 取引1つにつき1行：その取引の行を、JSON の配列1つで。落ちて途中で
-- 切れた行は読めないので、丸ごと読み飛ばす。取引は、全部あるか、
-- まったくないかのどちらか
encodeLines :: [[Entry]] -> ByteString
encodeLines = foldMap (\entries -> LBS.toStrict (encode entries) <> "\n")

decodeLines :: ByteString -> [[Entry]]
decodeLines = mapMaybe decodeStrict . BC.lines

loadJournal :: FilePath -> IO [Entry]
loadJournal path = do
  exists <- doesFileExist path
  if exists then concat . decodeLines <$> BS.readFile path else pure []

-- 🧹 Write a whole new journal next to the old one, then swap them.
--    A crash in the middle leaves the old journal as it was. The new
--    journal goes in whole, so each entry may have a line of its own.
-- 新しい日誌を、古いものの隣に丸ごと書いてから、入れ替える。
-- 途中で落ちても、古い日誌はそのまま残る。新しい日誌は丸ごと
-- 入れ替わるので、1行に1つずつ書いてよい
rewriteJournal :: FilePath -> [Entry] -> IO ()
rewriteJournal path entries = do
  let fresh = path <> ".new"
  BS.writeFile fresh (encodeLines (map (: []) entries))
  renameFile fresh path
-- ANCHOR_END: file

-- ANCHOR_END: all
