-- ANCHOR: all
-- | Save the index to a file, and read it back.
--   index.json -> JSON (easy to read), anything else -> binary (fast).
-- インデックスをファイルに保存し、読み戻す。
-- index.json なら JSON（読みやすい）、それ以外ならバイナリー（速い）。
module MiniSearch.Store
  ( saveIndex
  , loadIndex
  , replaceFile
  ) where

import Control.Exception (IOException, bracketOnError, try)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName)
import System.IO (hClose, openBinaryTempFileWithDefaultPermissions)

import MiniSearch.Types (Index)

-- ANCHOR: format
-- 🗂️ Which format to use, from the file name.
-- ファイルの名前から、形式を選ぶ
data Format = Json | Bin

formatOf :: FilePath -> Format
formatOf path
  | takeExtension path == ".json" = Json
  | otherwise                     = Bin
-- ANCHOR_END: format

-- ANCHOR: save
-- 💾 Write the index.
-- インデックスを書く
saveIndex :: FilePath -> Index -> IO ()
saveIndex path index = replaceFile path bytes
  where
    bytes = case formatOf path of
      Json -> encode index
      Bin  -> Binary.encode index

-- 🔁 Write to a temporary file in the same folder, then rename it
--    to the real name. The rename happens all at once, so if we stop
--    midway, the old file is still there, whole.
-- 同じフォルダーの一時ファイルに書いてから、本当の名前に付け替える。
-- 付け替えは一度に起きるので、途中で止まっても、古いファイルが丸ごと残る
replaceFile :: FilePath -> BL.ByteString -> IO ()
replaceFile path bytes =
  bracketOnError open throwAway $ \(tmp, h) -> do
    BL.hPut h bytes
    hClose h
    renameFile tmp path
  where
    open = openBinaryTempFileWithDefaultPermissions
             (takeDirectory path) (takeFileName path)
    -- If something goes wrong, remove the half-written file.
    -- 失敗したら、書きかけのファイルを消す
    throwAway (tmp, h) = hClose h >> removeFile tmp

-- 📥 Read the index back. Problems become a Left with a message.
-- インデックスを読み戻す。困ったことは、メッセージ付きの Left になる
loadIndex :: FilePath -> IO (Either String Index)
loadIndex path = case formatOf path of
  Json -> do
    result <- try (BS.readFile path)
    case result of
      Left err    -> pure (Left (show (err :: IOException)))
      Right bytes -> pure (eitherDecodeStrict bytes)
  Bin -> do
    result <- try (Binary.decodeFileOrFail path)
    case result of
      Left err               -> pure (Left (show (err :: IOException)))
      Right (Left (_, msg))  -> pure (Left msg)
      Right (Right index)    -> pure (Right index)
-- ANCHOR_END: save
-- ANCHOR_END: all
