-- ANCHOR: all
-- | Save the index to a file, and read it back.
-- インデックスをファイルに保存し、読み戻す。
module MiniSearch.Store
  ( saveIndex
  , loadIndex
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import MiniSearch.Types (Index)

-- 💾 Write the index as JSON.
-- インデックスを JSON で書く
saveIndex :: FilePath -> Index -> IO ()
saveIndex path index = BL.writeFile path (encode index)

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
