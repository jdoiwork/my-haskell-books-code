-- ANCHOR: all
-- ANCHOR: header
-- | Look a book up by its ISBN, with openBD (https://openbd.jp/): the
--   title, the author, the publisher, how many pages, and sometimes the
--   table of contents. Reading the answer is pure; asking needs the
--   network.
-- openBD（https://openbd.jp/）で、ISBN から本を調べる：題・著者・出版社・
-- ページ数、ときには目次も。答えを読むのは純粋。聞くにはネットが要る
module Tsundoku.Import.OpenBd
  ( BookInfo (..)
  , openBd
  , parseOpenBd
  , tocFromText
  , bookDraft
  , fetchBook
  ) where

-- bytes, text, and reading numbers
-- バイト列、文字列、数を読む
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import Text.Read (readMaybe)

-- JSON (aeson)
-- JSON（aeson）
import Data.Aeson (Value (..), eitherDecode, withObject, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)

import Network.HTTP.Client (Manager)

import Tsundoku.Import.Http (getUrl)
import Tsundoku.Import.Types
import Tsundoku.Isbn (Isbn, isbnText)
import Tsundoku.Types (Pages (..))
-- ANCHOR_END: header

-- ANCHOR: book
-- 📘 What openBD tells us about a book. `bookToc` is the table of contents
--    as text, when the publisher sent one ("はじめに……002" a line).
-- openBD が教えてくれる、本のこと。bookToc は文字の目次で、出版社が送って
-- いれば入っている（1行に「はじめに……002」）
data BookInfo = BookInfo
  { bookIsbn   :: Text
  , bookTitle  :: Text
  , bookAuthor :: Text
  , publisher  :: Text
  , pubdate    :: Text
  , bookPages  :: Maybe Pages
  , bookToc    :: Maybe Text
  }
  deriving (Show, Eq)

-- 🌐 Where openBD answers. / openBD の窓口
openBd :: String
openBd = "https://api.openbd.jp/v1"
-- ANCHOR_END: book

-- ANCHOR: parse
-- 📨 openBD answers with a list, one for each ISBN we asked about:
--    the book, or null if it does not know the book.
-- openBD の答えはリストで、聞いた ISBN ごとに1つ。本か、知らなければ null
parseOpenBd :: Isbn -> LBS.ByteString -> Either ImportProblem BookInfo
parseOpenBd asked bytes =
  case eitherDecode bytes of
    Left why         -> Left (Unexpected (T.pack why))
    Right [Null]     -> Left (UnknownIsbn asked)
    Right [answer]   -> either (Left . Unexpected . T.pack) Right
                               (parseEither bookFrom answer)
    Right _          -> Left (Unexpected "答えが1冊ぶんではない")

-- 🔎 The summary has the plain values. Pages and the table are deeper,
--    in the ONIX part (the book trade's format), and may be missing.
-- summary には、そのままの値がある。ページ数と目次は ONIX（出版業界の
-- 形式）の部分の奥にあって、ないこともある
bookFrom :: Value -> Parser BookInfo
bookFrom = withObject "openBD" $ \o -> do
  summary <- o .: "summary"
  let onix = Object o
  BookInfo <$> summary .: "isbn"
       <*> summary .: "title"
       <*> summary .: "author"
       <*> summary .: "publisher"
       <*> summary .: "pubdate"
       <*> pure (pagesIn onix)
       <*> pure (tocIn onix)
  where
    -- DescriptiveDetail/Extent: unit 03 is pages / 単位 03 がページ
    pagesIn onix = listToMaybe
      [ Pages n
      | extent       <- list ["onix", "DescriptiveDetail", "Extent"] onix
      , at ["ExtentUnit"] extent == Just (String "03")
      , String value <- maybeToList (at ["ExtentValue"] extent)
      , Just n       <- [readMaybe (T.unpack value)] ]
    -- CollateralDetail/TextContent: type 04 is the table of contents
    -- 種類 04 が目次
    tocIn onix = listToMaybe
      [ text
      | content     <- list ["onix", "CollateralDetail", "TextContent"] onix
      , at ["TextType"] content == Just (String "04")
      , String text <- maybeToList (at ["Text"] content) ]

-- 🧭 Walk into JSON by keys; a list there, or nothing.
-- キーをたどって JSON の奥へ。そこにあるリスト（なければ空）
at :: [Key] -> Value -> Maybe Value
at []         v          = Just v
at (k : keys) (Object o) = KeyMap.lookup k o >>= at keys
at _          _          = Nothing

list :: [Key] -> Value -> [Value]
list keys v = case at keys v of
  Just (Array xs) -> toList xs
  _               -> []
-- ANCHOR_END: parse

-- ANCHOR: toc
-- 📑 A table written as text: each line is a heading, dots, and the
--    page it starts on ("第1章　見分け方……003"). Lines with no page at
--    the end are left out. The table is flat: text has no depth.
-- 文字で書いた目次：1行が、見出し・点線・始まるページ
-- （「第1章　見分け方……003」）。終わりにページがない行は外す。
-- 文字には深さがないので、目次は平ら
tocFromText :: Text -> [(Text, Int)]
tocFromText text =
  [ (heading, page)
  | line <- T.lines text
  , let trimmed = T.strip line
        digits  = T.takeWhileEnd isDigit trimmed
        front   = T.dropWhileEnd isDigit trimmed
        heading = T.strip (T.dropWhileEnd isLeader front)
  , Just page <- [readMaybe (T.unpack digits)]
  , not (T.null heading) ]
  where
    isLeader c = c `elem` ("…・.．‥ 　" :: String)

-- 📗 A draft from what openBD said. With a table and the page count,
--    each heading gets its pages. Many real tables have no pages at all
--    (just the headings): keep the headings, share the pages out
--    evenly, and say so. With no table, one heading for the whole book.
-- openBD の答えから下書きを作る。目次とページ数があれば、見出しごとに
-- ページ数を。本物の目次は、ページのない（見出しだけの）ものが多い。
-- そのときは見出しを残し、ページ数を等分して、そう知らせる。目次が
-- なければ、本全体で1つの見出し
bookDraft :: BookInfo -> Imported Pages
bookDraft b = case (bookPages b, bookToc b) of
  (Just (Pages n), Just text)
    | lines'@(_ : _) <- tocFromText text ->
        let headings = [ Node line [] | line <- lines' ]
        in  Imported (draftOf (fromStarts n headings)) (outOfOrder headings)
    | headings@(_ : _) <- headingsOf text ->
        Imported (draftOf (shareOut n headings))
                 [NoPagesInToc (length headings)]
  (Just n,  _) -> Imported (draftOf [Node ("本文", n) []])       []
  (Nothing, _) -> Imported (draftOf [Node ("本文", Pages 0) []]) []
  where
    draftOf = Draft (Just (bookTitle b)) (Just (bookAuthor b))
    -- n pages over k headings: n `div` k each, one more for the first
    -- n `mod` k. They add up to n.
    -- n ページを k 個の見出しに：n `div` k ずつ、最初の n `mod` k 個には
    -- 1つ多く。足すと n
    shareOut n hs =
      [ Node (h, Pages (n `div` k + (if i < n `mod` k then 1 else 0))) []
      | (i, h) <- zip [0 ..] hs ]
      where
        k = length hs

-- ✂️ The headings of a table written as text, one to a line, with no
--    pages: every line that is not blank.
-- 文字で書いた目次の見出し（ページなし）：空でない行の全部
headingsOf :: Text -> [Text]
headingsOf text = filter (not . T.null) (map T.strip (T.lines text))
-- ANCHOR_END: toc

-- ANCHOR: fetch
-- 🌐 Ask openBD about one book. / openBD に1冊を聞く
fetchBook :: Manager -> String -> Isbn -> IO (Either ImportProblem BookInfo)
fetchBook manager base asked = do
  answer <- getUrl manager (base <> "/get?isbn=" <> T.unpack (isbnText asked))
  pure (parseOpenBd asked =<< answer)
-- ANCHOR_END: fetch
-- ANCHOR_END: all
