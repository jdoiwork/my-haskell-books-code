-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE MultilineStrings #-}
-- | Small books made by code, to try the importers on: an EPUB 3 and an
--   EPUB 2 of 『はじめてのドラゴン学』, and a PDF of the first chapters
--   of "Alice's Adventures in Wonderland" (in the public domain).
-- 取り込みを試すための、コードで作る小さな本：『はじめてのドラゴン学』の
-- EPUB 3 と EPUB 2、『不思議の国のアリス』のはじめの章の PDF（著作権の
-- 切れた作品）
module Tsundoku.Import.Sample
  ( -- * EPUB
    EpubVersion (..)
  , epubFiles
  , zipFiles
  , sampleEpub
  , sampleEpub2
    -- * PDF
  , pdfBook
  , samplePdf
  ) where

import Codec.Archive.Zip
  (CompressionMethod (..), Entry (..), addEntryToArchive, emptyArchive,
   fromArchive, toEntry)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
-- ANCHOR_END: header

-- ANCHOR: epub
-- 📚 EPUB 3 has nav.xhtml; EPUB 2 has only toc.ncx.
-- EPUB 3 には nav.xhtml、EPUB 2 には toc.ncx だけがある
data EpubVersion = Epub3 | Epub2 deriving (Show, Eq)

sampleEpub, sampleEpub2 :: LBS.ByteString
sampleEpub  = zipFiles (epubFiles Epub3)
sampleEpub2 = zipFiles (epubFiles Epub2)

-- 🗂️ Every file of the book, in the order they go into the zip. The
--    first must be `mimetype`, stored without compression: that is how
--    a reader knows the zip is an EPUB.
-- 本のファイルの全部を、zip に入れる順に。最初は mimetype で、圧縮しない
-- で入れる。それを見て、読む側は「この zip は EPUB だ」とわかる
epubFiles :: EpubVersion -> [(FilePath, LBS.ByteString)]
epubFiles version =
  [ ("mimetype",               "application/epub+zip")
  , ("META-INF/container.xml", utf8 container)
  , ("OEBPS/content.opf",      utf8 (package version))
  ]
  ++ tocFiles
  ++ [ ("OEBPS/" <> file, utf8 (page heading body))
     | (file, heading, body) <- chapters ]
  where
    tocFiles = case version of
      Epub3 -> [("OEBPS/nav.xhtml", utf8 nav)]
      Epub2 -> [("OEBPS/toc.ncx",   utf8 ncx)]

-- 🤐 Zip the files, in order. A fixed time, so the bytes are the same
--    every time.
-- ファイルを順に zip にする。時刻は決めておく（毎回同じバイト列になる）
zipFiles :: [(FilePath, LBS.ByteString)] -> LBS.ByteString
zipFiles files = fromArchive (foldr add emptyArchive files)
  where
    add (path, bytes) archive = addEntryToArchive (entry path bytes) archive
    entry "mimetype" bytes = (toEntry "mimetype" 0 bytes)
      { eCompressionMethod = NoCompression
      , eCompressedData    = bytes
      , eCompressedSize    = fromIntegral (LBS.length bytes) }
    entry path bytes = toEntry path 0 bytes

utf8 :: Text -> LBS.ByteString
utf8 = LBS.fromStrict . encodeUtf8
-- ANCHOR_END: epub

-- ANCHOR: chapters
-- 📄 The pages, in reading order: (file, heading, body). The cover
--    comes first and is in no table. Chapter 4 is split into two files,
--    and only the first is in the table. 1.1 uses ruby (ふりがな).
-- 読む順のページ：（ファイル、見出し、本文）。表紙が先頭で、どの目次にも
-- ない。第4章は2つのファイルに分かれていて、目次にあるのは1つ目だけ。
-- 1.1 はルビ（ふりがな）を使う
chapters :: [(FilePath, Text, Text)]
chapters =
  [ ("cover.xhtml",  "はじめてのドラゴン学", "<p>ギルド図書室</p>")
  , ("part1.xhtml",  "第1部 ドラゴンを知る", filler  1100)
  , ("ch1.xhtml",    "第1章 見分け方",       filler  2300)
  , ("ch1-1.xhtml",  "1.1 うろこの色",       ruby     400)
  , ("ch1-2.xhtml",  "1.2 火を吐くもの",     filler  5900)
  , ("ch2.xhtml",    "第2章 すみかと季節",   filler 14300)
  , ("part2.xhtml",  "第2部 ドラゴンと話す", filler  1100)
  , ("ch3.xhtml",    "第3章 あいさつの作法", filler  9500)
  , ("ch4.xhtml",    "第4章 怒らせたときは", filler  3500)
  , ("ch4-2.xhtml",  "第4章（つづき）",      filler  3500)
  ]
  where
    -- About n characters of text: one sentence, again and again.
    -- 約 n 字の本文：同じ文を、くり返す
    filler n = "<p>" <> T.take n (T.replicate n sentence) <> "</p>"
    sentence = "ドラゴンは、ていねいにあいさつをすれば怒りません。"
    -- A sentence with ruby (竜鱗 read りゅうりん), n times: 16 characters
    -- each with the ruby, 11 without.
    -- ルビ（竜鱗＝りゅうりん）つきの文を n 回。ルビも数えると1回 16 字、
    -- 数えなければ 11 字
    ruby n = "<p>" <> T.replicate n
      "<ruby>竜鱗<rt>りゅうりん</rt></ruby>は光で色を変える。<br/>" <> "</p>"
-- ANCHOR_END: chapters

-- ANCHOR: xml
-- 📝 The XML files. / XML のファイル
page :: Text -> Text -> Text
page heading body = T.concat
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  , "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"ja\">"
  , "<head><title>", heading, "</title></head>"
  , "<body><h1>", heading, "</h1>", body, "</body></html>\n"
  ]

container :: Text
container = """
  <?xml version="1.0" encoding="UTF-8"?>
  <container version="1.0"
             xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
    <rootfiles>
      <rootfile full-path="OEBPS/content.opf"
                media-type="application/oebps-package+xml"/>
    </rootfiles>
  </container>
  """

package :: EpubVersion -> Text
package version = T.unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<package xmlns=\"http://www.idpf.org/2007/opf\""
  , "         version=\"" <> number <> "\" unique-identifier=\"id\">"
  , "<metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\">"
  , "  <dc:identifier id=\"id\">urn:uuid:" <> uuid <> "</dc:identifier>"
  , "  <dc:title>はじめてのドラゴン学</dc:title>"
  , "  <dc:creator>ギルド図書室</dc:creator>"
  , "  <dc:language>ja</dc:language>"
  , "  <meta property=\"dcterms:modified\">2026-11-09T00:00:00Z</meta>"
  , "</metadata>"
  , "<manifest>"
  , "  " <> tocItem
  ]
  ++ [ "  " <> item (idOf file) (T.pack file) "application/xhtml+xml" ""
     | (file, _, _) <- chapters ]
  ++ [ "</manifest>"
     , "<spine" <> spineToc <> ">" ]
  ++ [ "  <itemref idref=\"" <> idOf file <> "\"/>"
     | (file, _, _) <- chapters ]
  ++ [ "</spine>"
     , "</package>" ]
  where
    uuid = "3f0e3c8e-0000-4000-8000-d7a90b00c0de"
    (number, tocItem, spineToc) = case version of
      Epub3 -> ("3.0", item "nav" "nav.xhtml" "application/xhtml+xml"
                            " properties=\"nav\"", "")
      Epub2 -> ("2.0", item "ncx" "toc.ncx" "application/x-dtbncx+xml" "",
                " toc=\"ncx\"")
    item i href kind more =
      "<item id=\"" <> i <> "\" href=\"" <> href
        <> "\" media-type=\"" <> kind <> "\"" <> more <> "/>"
    idOf file = T.replace "." "-" (T.pack file)

-- 🌳 The table, for EPUB 3: lists in lists.
-- EPUB 3 の目次：リストの中のリスト
nav :: Text
nav = """
  <?xml version="1.0" encoding="UTF-8"?>
  <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:epub="http://www.idpf.org/2007/ops" lang="ja">
  <head><title>目次</title></head>
  <body>
    <nav epub:type="toc"><h1>目次</h1>
      <ol>
        <li><a href="part1.xhtml">第1部 ドラゴンを知る</a>
          <ol>
            <li><a href="ch1.xhtml">第1章 見分け方</a>
              <ol>
                <li><a href="ch1-1.xhtml">1.1 うろこの色</a></li>
                <li><a href="ch1-2.xhtml">1.2 火を吐くもの</a></li>
              </ol>
            </li>
            <li><a href="ch2.xhtml">第2章 すみかと季節</a></li>
          </ol>
        </li>
        <li><a href="part2.xhtml">第2部 ドラゴンと話す</a>
          <ol>
            <li><a href="ch3.xhtml">第3章 あいさつの作法</a></li>
            <li><a href="ch4.xhtml">第4章 怒らせたときは</a></li>
          </ol>
        </li>
      </ol>
    </nav>
  </body>
  </html>
  """

-- 🌳 The same table, for EPUB 2: navPoints in navPoints.
-- 同じ目次の EPUB 2 版：navPoint の中の navPoint
ncx :: Text
ncx = T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\">"
  , "<head/><docTitle><text>はじめてのドラゴン学</text></docTitle>"
  , "<navMap>"
  , point "part1.xhtml" "第1部 ドラゴンを知る"
      [ point "ch1.xhtml" "第1章 見分け方"
          [ point "ch1-1.xhtml" "1.1 うろこの色"   []
          , point "ch1-2.xhtml" "1.2 火を吐くもの" [] ]
      , point "ch2.xhtml" "第2章 すみかと季節" [] ]
  , point "part2.xhtml" "第2部 ドラゴンと話す"
      [ point "ch3.xhtml" "第3章 あいさつの作法" []
      , point "ch4.xhtml" "第4章 怒らせたときは" [] ]
  , "</navMap>"
  , "</ncx>"
  ]
  where
    point src label kids = T.concat
      [ "<navPoint id=\"", T.replace "." "-" src, "\">"
      , "<navLabel><text>", label, "</text></navLabel>"
      , "<content src=\"", src, "\"/>"
      , T.concat kids
      , "</navPoint>" ]
-- ANCHOR_END: xml

-- ANCHOR: pdf
-- 📕 A small PDF, written by hand: six pages of plain text, and (if
--    asked) bookmarks. A PDF is numbered objects, and a table at the end
--    that says where each object starts, counted in bytes.
-- 手で書いた小さな PDF：ただの文字の6ページと、（頼まれれば）しおり。
-- PDF は番号つきのオブジェクトの並びと、最後の表（各オブジェクトが何
-- バイト目から始まるか）でできている
samplePdf :: LBS.ByteString
samplePdf = pdfBook True

pdfBook :: Bool -> LBS.ByteString
pdfBook withMarks = LBS8.pack (pdfFile objects)
  where
    -- The objects, numbered from 1 in this order: 1 the catalog, 2 the
    -- pages, 3 the font, 4 the bookmarks, then 2 for each page (what
    -- is on it, and the page), the bookmarks, and the title last.
    -- オブジェクト。この順に 1 から番号が付く：1 目録、2 ページの束、
    -- 3 書体、4 しおりの根、ページごとに2つ（中身とページ）、しおり、
    -- 最後に題と作者
    objects = [catalog, pageTree, font, outlines]
              ++ concat [ [content lines', pageObject k]
                        | (k, lines') <- zip [0 ..] texts ]
              ++ (if withMarks then marks else [])
              ++ [info]

    catalog  = dict ("/Type /Catalog /Pages 2 0 R"
                     ++ (if withMarks then " /Outlines 4 0 R" else ""))
    pageTree = dict ("/Type /Pages /Count " ++ show (length texts)
                     ++ " /Kids [" ++ unwords (map (ref . pageId) pageNos)
                     ++ "]")
    font     = dict "/Type /Font /Subtype /Type1 /BaseFont /Helvetica"
    info     = dict ("/Title (" ++ bookTitle ++ ") /Author (Lewis Carroll)")

    -- a page and what is on it: lines of text, from the top
    -- ページと、その中身：上から文字の行
    pageNos      = [0 .. length texts - 1]
    pageId k     = 6 + 2 * k
    pageObject k = dict ("/Type /Page /Parent 2 0 R /MediaBox [0 0 420 595]"
                         ++ " /Resources << /Font << /F1 3 0 R >> >>"
                         ++ " /Contents " ++ ref (pageId k - 1))
    content lines' = dict ("/Length " ++ show (length stream))
                     ++ "\nstream\n" ++ stream ++ "\nendstream"
      where
        stream = "BT /F1 12 Tf 48 540 Td 16 TL "
                 ++ unwords [ "(" ++ escape l ++ ") '" | l <- lines' ]
                 ++ " ET"

    -- the bookmarks: the title (page 1), and the chapters under it
    -- しおり：題（1ページ目）と、その下の章
    top      = 5 + 2 * length texts
    outlines
      | withMarks = dict ("/Type /Outlines /First " ++ ref top
                          ++ " /Last " ++ ref top
                          ++ " /Count " ++ show (1 + length marked))
      | otherwise = dict "/Type /Outlines /Count 0"
    marks = dict ("/Title (" ++ bookTitle ++ ") /Parent 4 0 R"
                  ++ " /First " ++ ref (top + 1)
                  ++ " /Last "  ++ ref (top + length marked)
                  ++ " /Count " ++ show (length marked) ++ dest 0)
          : [ dict ("/Title (" ++ name ++ ") /Parent " ++ ref top
                    ++ (if i > 1 then " /Prev " ++ ref (top + i - 1) else "")
                    ++ (if i < length marked
                          then " /Next " ++ ref (top + i + 1) else "")
                    ++ dest p)
            | (i, (name, p)) <- zip [1 ..] marked ]
    dest p = " /Dest [" ++ ref (pageId p) ++ " /Fit]"

    bookTitle = "Alice's Adventures in Wonderland"
    -- (bookmark, the page it points to, from 0)
    -- （しおり、指すページ。0 から数える）
    marked  =
      [ ("I. Down the Rabbit-Hole", 1)
      , ("II. The Pool of Tears",   3)
      , ("III. A Caucus-Race",      5) ]
    -- The pages: from the start of the book (public domain).
    -- ページ：本のはじめから（著作権の切れた作品）
    texts =
      [ [bookTitle, "by Lewis Carroll"]
      , [ "CHAPTER I. Down the Rabbit-Hole"
        , "Alice was beginning to get very tired"
        , "of sitting by her sister on the bank," ]
      , ["and of having nothing to do."]
      , [ "CHAPTER II. The Pool of Tears"
        , "Curiouser and curiouser! cried Alice." ]
      , [ "(she was so much surprised, that for the moment"
        , "she quite forgot how to speak good English)" ]
      , ["CHAPTER III. A Caucus-Race and a Long Tale"]
      ]

-- 🧱 The objects into a file: the header, each object, the table of
--    where each one starts, and the trailer (the catalog is object 1,
--    the title is the last one). Every character here is ASCII, one
--    byte, so `length` counts bytes.
-- オブジェクトを、ファイルにする：頭、オブジェクト、それぞれが始まる位置
-- の表、おしまい（目録は 1 番、題は最後の番号）。ここの文字は全部 ASCII
-- の1バイトなので、length がそのままバイト数になる
pdfFile :: [String] -> String
pdfFile objects = header ++ concat bodies ++ xref ++ trailer
  where
    header  = "%PDF-1.4\n"
    bodies  = [ show n ++ " 0 obj\n" ++ o ++ "\nendobj\n"
              | (n, o) <- zip [1 :: Int ..] objects ]
    starts  = scanl (+) (length header) (map length bodies)
    size    = show (length objects + 1)
    xref    = "xref\n0 " ++ size ++ "\n0000000000 65535 f \n"
              ++ concat [ pad (show o) ++ " 00000 n \n" | o <- init starts ]
    trailer = "trailer\n" ++ dict ("/Size " ++ size ++ " /Root 1 0 R /Info "
                                   ++ ref (length objects))
              ++ "\nstartxref\n" ++ show (last starts) ++ "\n%%EOF\n"
    pad s   = replicate (10 - length s) '0' ++ s

-- A dictionary, and a reference to object n.
-- 辞書と、n 番のオブジェクトへの参照
dict :: String -> String
dict body = "<< " ++ body ++ " >>"

ref :: Int -> String
ref n = show n ++ " 0 R"

-- ( and ) end a string in a PDF, so they need a \ before them.
-- PDF の文字列の中では、( と ) の前に \ が要る
escape :: String -> String
escape = concatMap (\c -> if c `elem` special then ['\\', c] else [c])
  where
    special = "()\\" :: String
-- ANCHOR_END: pdf
-- ANCHOR_END: all
