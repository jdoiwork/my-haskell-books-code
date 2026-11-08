-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE DeriveAnyClass #-}
-- | Save the index to a file, and read it back.
--   index.json -> JSON (easy to read), anything else -> binary (fast).
-- インデックスをファイルに保存し、読み戻す。
-- index.json なら JSON（読みやすい）、それ以外ならバイナリー（速い）。
module MiniSearch.Store
  ( saveIndex
  , loadIndex
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary (Binary)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath (takeExtension)

import MiniSearch.Tokenizer (Tokenizer (..), tokenizerByName)
import MiniSearch.Types (Index)
-- ANCHOR_END: head

-- ANCHOR: saved
-- 🏷️ What goes into the file: the index, and the name of the
--    tokenizer it was made with (searching needs the same one).
-- ファイルに入れるもの：インデックスと、それを作ったトークナイザーの
-- 名前（検索にも、同じものが要る）
data Saved = Saved
  { savedTokenizer :: Text
  , savedIndex     :: Index
  }
  deriving stock    (Generic)
  deriving anyclass (ToJSON, FromJSON, Binary)
-- ANCHOR_END: saved

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
saveIndex :: FilePath -> Tokenizer -> Index -> IO ()
saveIndex path tok index =
  let saved = Saved (tokenizerName tok) index
  in case formatOf path of
       Json -> BL.writeFile path (encode saved)
       Bin  -> Binary.encodeFile path saved

-- 📥 Read the index back. Problems become a Left with a message.
-- インデックスを読み戻す。困ったことは、メッセージ付きの Left になる
loadIndex :: FilePath -> IO (Either String (Tokenizer, Index))
loadIndex path = (>>= withTokenizer) <$> loadSaved path
  where
    withTokenizer (Saved name index) = case tokenizerByName name of
      -- 「知らないトークナイザーです」
      Nothing  -> Left ("unknown tokenizer: " <> T.unpack name)
      Just tok -> Right (tok, index)

loadSaved :: FilePath -> IO (Either String Saved)
loadSaved path = case formatOf path of
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
      Right (Right saved)    -> pure (Right saved)
-- ANCHOR_END: save
-- ANCHOR_END: all
