-- ANCHOR: all
-- | Save the index to a file, and read it back.
-- インデックスをファイルに保存し、読み戻す。
module MiniSearch.Store
  ( saveIndex
  , loadIndex
  , replaceFile
  ) where

import Control.Exception (IOException, bracketOnError, try)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, openBinaryTempFileWithDefaultPermissions)

import MiniSearch.Types (Index)

-- 💾 Write the index as JSON.
-- インデックスを JSON で書く
saveIndex :: FilePath -> Index -> IO ()
saveIndex path index = replaceFile path (encode index)

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

-- 📥 Read the index back. A missing file or broken JSON
--    becomes a Left with a message, not a crash.
-- インデックスを読み戻す。ファイルがない・JSON が壊れている、は
-- 落ちるのではなく、メッセージ付きの Left になる
loadIndex :: FilePath -> IO (Either String Index)
loadIndex path = do
  result <- try (BS.readFile path)
  case result of
    Left err    -> pure (Left (show (err :: IOException)))
    Right bytes -> pure (eitherDecodeStrict bytes)
-- ANCHOR_END: all
