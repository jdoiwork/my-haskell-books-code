-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE OverloadedRecordDot #-}
-- | The way in for things from outside: pick the reader by the kind of
--   file, and find the web novels on the shelf.
-- 外から来るものの入り口：ファイルの種類で読み方を選ぶ。本棚の Web 小説を
-- 見つける
module Tsundoku.Import
  ( charsPerPage
  , importFile
  , readEpubBook
  , textsFor
  , shelveBook
  , ncodeDef
  , publisherDef
  , narouSerials
  , updatesFor
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (flatten)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

import Tsundoku.Card (Card (..), episodesIn)
import Tsundoku.Import.Epub (bodyChars, bodyText, readEpubWith)
import Tsundoku.Import.Narou (Novel (..))
import Tsundoku.Import.Pdf (readPdf)
import Tsundoku.Import.Types
import Tsundoku.Shelf (Shelf (..), WorkWindow (..), InsideWindow (..))
import Tsundoku.Types
-- ANCHOR_END: header

-- ANCHOR: importFile
-- 📏 How many characters we take as one page (about one page of a
--    bunko, 文庫本).
-- 1ページを何字とみなすか（文庫本の1ページくらい）
charsPerPage :: Int
charsPerPage = 600

-- 📥 Take in a file: EPUB or PDF, by its extension. Each heading comes
--    with its pages and its text (a PDF's text is not taken in yet, so
--    it is empty).
-- ファイルを取り込む。EPUB か PDF かは、拡張子で決める。見出しごとに、
-- ページ数と本文が付く（PDF の本文はまだ取り込まないので、空）
importFile :: FilePath -> IO (Either ImportProblem (Imported (Pages, Text)))
importFile path = do
  there <- doesFileExist path
  if not there
    then pure (Left (NoSuchFile path))
    else case map toLower (takeExtension path) of
      ".epub" -> do
        bytes <- try (LBS.readFile path)
        pure $ case bytes of
          Left (_ :: IOException) -> Left (NoSuchFile path)
          Right b -> readEpubBook b
      ".pdf"  -> fmap (fmap noText) <$> readPdf path
      _       -> pure (Left (NotSupported path))
  where
    noText pages = (pages, T.empty)

-- 📖 An EPUB, measured two ways at once: the characters (made into
--    pages) and the text. A pair of monoids is a monoid, so readEpubWith
--    gives both out to the headings in one go.
-- EPUB を、2通りに一度にはかる：字数（ページにする）と本文。Monoid の組は
-- Monoid なので、readEpubWith が、両方を一度に見出しへ配ってくれる
readEpubBook :: LBS.ByteString
             -> Either ImportProblem (Imported (Pages, Text))
readEpubBook bytes =
  fmap (first (perPage charsPerPage)) <$> readEpubWith both bytes
  where
    both page = (bodyChars page, bodyText page)
-- ANCHOR_END: importFile

-- ANCHOR: shelveBook
-- 📝 The texts of a draft, by the numbers the shelf gave its table.
--    Both go from top to bottom, so they line up. Blank texts are left
--    out.
-- 下書きの本文を、本棚が目次に振った番号で。どちらも上から順なので、
-- そのまま並ぶ。空白だけの本文は入れない
textsFor :: Contents -> Draft Text -> [(EntryId, Text)]
textsFor what d =
  [ (i, text)
  | (i, text) <- zip (entryIds what) texts
  , not (T.null (T.strip text)) ]
  where
    texts = map snd (concatMap flatten (draftToc d))

-- 📚 Put a book we took in on the shelf, through the window only: the
--    work first, then the text of each entry.
-- 取り込んだ本を、窓口だけを通して本棚に置く：まず作品、それから
-- 項目ごとの本文
shelveBook :: Shelf -> Text -> Draft (Pages, Text) -> IO WorkId
shelveBook shelf name d = do
  wid   <- shelf.works.addWork (bookWork name (fmap fst d))
  found <- shelf.works.getCard wid
  forM_ found $ \card ->
    forM_ (textsFor (contents (work card)) (fmap snd d)) $ \(i, text) ->
      shelf.inside.setText wid i text
  pure wid
-- ANCHOR_END: shelveBook

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

-- ANCHOR: publisher
-- 🏢 The attribute that keeps a book's publisher (openBD tells it; an AI
--    can send it too). Some readers care a lot about the publisher.
-- 本の出版社を書いておく属性（openBD が教えてくれる。AI が送ってくる
-- こともある）。出版社に、こだわる読み手もいる
publisherDef :: AttrDef
publisherDef = AttrDef { attrName = "出版社", attrKind = TextKind }
-- ANCHOR_END: publisher
-- ANCHOR_END: all
