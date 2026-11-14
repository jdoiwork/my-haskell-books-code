-- ANCHOR: all
-- ANCHOR: module
{-# LANGUAGE OverloadedStrings #-}
-- | Read the documents from a folder.
-- フォルダーから文書を読む。
module MiniSearch.Load
  ( readDocuments
  , readDocument
  , toDocument
  , fileDocId
  ) where
-- ANCHOR_END: module

-- ANCHOR: imports
import Control.Exception (throwIO)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)

import MiniSearch.Types
-- ANCHOR_END: imports

-- ANCHOR: toDocument
-- 📄 The first line is the title, the rest is the body.
-- 1行目が題、残りが本文
toDocument :: DocId -> Text -> Document
toDocument i content =
  case T.lines content of
    []             -> Document i "" ""
    (title : rest) -> Document i (T.strip title) (T.unlines rest)
-- ANCHOR_END: toDocument

-- ANCHOR: fileDocId
-- 🔑 The id of a file: SHA-256 of its name, a \0, and its bytes,
--    cut to the first 12 hex digits. The same name and contents
--    always give the same id, on any machine, in any version.
-- ファイルの番号：名前、区切りの \0、中身のバイト列の SHA-256 を、
-- 16進の頭の12文字に切る。名前と中身が同じなら、どの機械でも、
-- どの版でも、いつも同じ番号になる
fileDocId :: FilePath -> ByteString -> DocId
fileDocId name bytes =
  let digest = SHA256.hash (TE.encodeUtf8 (T.pack name) <> "\0" <> bytes)
      hex    = concatMap (printf "%02x") (BS.unpack digest) :: String
  in DocId (T.pack (take 12 hex))
-- ANCHOR_END: fileDocId

-- ANCHOR: read
-- 📂 Read every .txt file in the folder, in name order.
-- フォルダーの .txt を全部、名前の順に読む
readDocuments :: FilePath -> IO [Document]
readDocuments dir = do
  names <- listDirectory dir
  let txtFiles = sort (filter (\n -> takeExtension n == ".txt") names)
  mapM (readDocument dir) txtFiles

-- 📄 Read one file. Its id comes from its name and contents,
--    not from where it is in the folder.
-- ファイルを1つ読む。番号は、名前と中身から決まる。
-- フォルダーの中の何番目かには、よらない
--    UTF-8 is checked here, while reading (decodeUtf8' gives an
--    Either), so a bad file fails now, with its name.
-- UTF-8 かどうかは、読むときにここで確かめる（decodeUtf8' は Either を
-- 返す）。だめなファイルは、あとでではなく、いま、名前つきで失敗する
readDocument :: FilePath -> FilePath -> IO Document
readDocument dir name = do
  bytes <- BS.readFile (dir </> name)
  case TE.decodeUtf8' bytes of
    -- 「cafe.txt：UTF-8 ではありません」
    Left _     -> throwIO (userError (name <> ": not UTF-8"))
    Right text -> pure (toDocument (fileDocId name bytes) text)
-- ANCHOR_END: read
-- ANCHOR_END: all
