-- ANCHOR: head
-- ANCHOR: all
-- | Save the index to a file, and read it back.
--   index.json -> JSON (easy to read), anything else -> binary (fast).
-- インデックスをファイルに保存し、読み戻す。
-- index.json なら JSON（読みやすい）、それ以外ならバイナリー（速い）。
module MiniSearch.Store
  ( saveIndex
  , loadIndex
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeExtension)

import MiniSearch.Types (Index)
-- ANCHOR_END: head

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
saveIndex path index = case formatOf path of
  Json -> BL.writeFile path (encode index)
  Bin  -> Binary.encodeFile path index

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
