-- ANCHOR: all
-- ANCHOR: header
-- | The way in for things from outside: pick the reader by the kind of
--   file, and find the web novels on the shelf.
-- 外から来るものの入り口：ファイルの種類で読み方を選ぶ。本棚の Web 小説を
-- 見つける
module Tsundoku.Import
  ( charsPerPage
  , importFile
  , ncodeDef
  , narouSerials
  , updatesFor
  ) where

import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

import Tsundoku.Card (Card (..), episodesIn)
import Tsundoku.Import.Epub (readEpub)
import Tsundoku.Import.Narou (Novel (..))
import Tsundoku.Import.Pdf (readPdf)
import Tsundoku.Import.Types
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: importFile
-- 📏 How many characters we take as one page (about one page of a
--    bunko, 文庫本).
-- 1ページを何字とみなすか（文庫本の1ページくらい）
charsPerPage :: Int
charsPerPage = 600

-- 📥 Take in a file: EPUB or PDF, by its extension.
-- ファイルを取り込む。EPUB か PDF かは、拡張子で決める
importFile :: FilePath -> IO (Either ImportProblem (Imported Pages))
importFile path = do
  there <- doesFileExist path
  if not there
    then pure (Left (NoSuchFile path))
    else case map toLower (takeExtension path) of
      ".epub" -> do
        bytes <- try (LBS.readFile path)
        pure $ case bytes of
          Left (_ :: IOException) -> Left (NoSuchFile path)
          Right b -> fmap (perPage charsPerPage) <$> readEpub b
      ".pdf"  -> readPdf path
      _       -> pure (Left (NotSupported path))
-- ANCHOR_END: importFile

-- ANCHOR: updates
-- 🏷️ The attribute that says where a web novel is on 小説家になろう.
-- 小説家になろうのどの作品かを書いておく属性
ncodeDef :: AttrDef
ncodeDef = AttrDef { attrName = "Nコード", attrKind = TextKind }

-- 📱 The serials on the shelf that have an Nコード.
-- 本棚の連載のうち、Nコードのあるもの
narouSerials :: [(WorkId, Card)] -> [(WorkId, Text, Toc Episodes)]
narouSerials cards =
  [ (wid, code, toc)
  | (wid, card)       <- cards
  , Serial toc        <- [contents (work card)]
  , Just (TextV code) <- [Map.lookup (attrName ncodeDef) (attrs card)] ]

-- 🔔 For each of those, what the API said: the novel and how many
--    episodes came out that the card does not have, or that the API did
--    not know the novel.
-- それぞれについて、API の答え：作品と、カードにまだない話が何話出たか。
-- または、API がその作品を知らなかったこと
updatesFor :: [Novel] -> [(WorkId, Text, Toc Episodes)]
           -> [(WorkId, Either ImportProblem (Novel, Int))]
updatesFor found serials =
  [ (wid, answer code toc) | (wid, code, toc) <- serials ]
  where
    byCode = [ (T.toUpper (ncode n), n) | n <- found ]
    answer code toc = case lookup (T.toUpper code) byCode of
      Nothing -> Left (UnknownNovel code)
      Just n  -> Right (n, max 0 (episodes n - episodesIn toc))
-- ANCHOR_END: updates
-- ANCHOR_END: all
