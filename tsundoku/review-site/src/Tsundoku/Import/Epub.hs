-- ANCHOR: all
-- ANCHOR: header
-- | Read an EPUB: a zip of XHTML files, with a table of contents.
--   Everything here is pure: it takes the bytes of the file, and does
--   not touch the disk.
-- EPUB を読む。中身は、XHTML のファイルと目次を入れた zip。ここは全部
-- 純粋：ファイルのバイト列を受け取るだけで、ディスクにはさわらない
module Tsundoku.Import.Epub
  ( readEpub
  , readEpubWith
  , bodyChars
  , bodyText
  , resolve
  ) where

-- zip files, bytes, and text
-- zip、バイト列、文字列
import Codec.Archive.Zip
  (Archive, findEntryByPath, fromEntry, toArchiveOrFail)
import Control.Exception (displayException)
import Control.Monad ((>=>))
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isSpace)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (mapAccumL)
import Data.Tree (Forest, Tree (..), flatten)

-- XML that must be right (xml-conduit), and HTML as it comes (tagsoup)
-- きちんとした XML（xml-conduit）と、ありのままの HTML（tagsoup）
import Text.HTML.TagSoup (Tag (..), parseTags)
import Text.XML (ParseSettings (..), def, parseLBS)
import Text.XML.Cursor
  (Cursor, content, fromDocument, laxAttribute, laxElement, ($/), ($//))
import Text.XML.Stream.Parse (decodeHtmlEntities)

import Tsundoku.Import.Types
-- ANCHOR_END: header

-- ANCHOR: readEpub
-- 📖 Read an EPUB: its title, its creator, and its table with the
--    characters of each part. A missing chapter file does not stop us;
--    it comes back in `missed`.
-- EPUB を読む：題・作者・目次と、項目ごとの字数。章のファイルが1つ
-- 欠けていても止まらず、missed に入れて返す
readEpub :: LBS.ByteString -> Either ImportProblem (Imported Chars)
readEpub = readEpubWith bodyChars

-- 🔧 The same, with your own way to measure a page. Anything that adds
--    up (a Monoid) will do: the characters, the text itself, or both.
-- 同じことを、ページのはかり方を選んで。足し合わせられるもの（Monoid）
-- なら何でもよい：字数でも、本文そのものでも、その両方でも
readEpubWith :: Monoid m => (Text -> m) -> LBS.ByteString
             -> Either ImportProblem (Imported m)
readEpubWith measure bytes = do
  archive   <- either (Left . NotAZip . T.pack) Right (toArchiveOrFail bytes)
  container <- xmlIn archive "META-INF/container.xml"
  opfPath   <- case container $// fullPath of
    path : _ -> Right (T.unpack path)
    []       -> Left (BrokenXml "META-INF/container.xml" "rootfile がない")
  package   <- readPackage opfPath <$> xmlIn archive opfPath
  links     <- tocOf archive package
  let pages   = [ (path, fileText archive path) | path <- spineFiles package ]
      counted = [ (path, measure text) | (path, Just text) <- pages ]
      linked  = [ path | (_, Just path) <- concatMap flatten links ]
      missing = nub [ path | path <- linked ++ spineFiles package
                           , fileText archive path == Nothing ]
  pure Imported
    { draft  = Draft (packTitle package) (packCreator package)
                     (sizesOf links counted)
    , missed = map MissingFile missing
    }
  where
    fullPath = laxElement "rootfile" >=> laxAttribute "full-path"
-- ANCHOR_END: readEpub

-- ANCHOR: package
-- 📦 What the package file (the OPF) says: the title and creator, where
--    the table is (nav for EPUB 3, toc.ncx for EPUB 2), and the files
--    in reading order (the spine). Paths are inside the zip.
-- パッケージのファイル（OPF）に書いてあること：題と作者、目次の場所
-- （EPUB 3 は nav、EPUB 2 は toc.ncx）、読む順のファイル（spine）。
-- パスは zip の中のもの
data Package = Package
  { packTitle   :: Maybe Text
  , packCreator :: Maybe Text
  , navFile     :: Maybe FilePath
  , ncxFile     :: Maybe FilePath
  , spineFiles  :: [FilePath]
  }

readPackage :: FilePath -> Cursor -> Package
readPackage opfPath opf = Package
  { packTitle   = firstText "title"
  , packCreator = firstText "creator"
  , navFile     = listToMaybe [ href | (_, href, props, _) <- items
                                     , "nav" `elem` T.words props ]
  , ncxFile     = listToMaybe [ href | (_, href, _, kind) <- items
                                     , kind == "application/x-dtbncx+xml" ]
  , spineFiles  = mapMaybe (`Map.lookup` byId) idrefs
  }
  where
    firstText name =
      listToMaybe (filter (not . T.null) (map tidy (opf $// laxElement name)))
    items =
      [ (one "id" c, resolve opfPath (one "href" c), one "properties" c,
         one "media-type" c)
      | c <- opf $// laxElement "item" ]
    byId   = Map.fromList [ (i, href) | (i, href, _, _) <- items ]
    idrefs = opf $// (laxElement "itemref" >=> laxAttribute "idref")
    one name c = T.concat (laxAttribute name c)
-- ANCHOR_END: package

-- ANCHOR: toc
-- 🌳 The table: from nav (EPUB 3) if there is one, else from toc.ncx
--    (EPUB 2). Each heading comes with the file it points to.
-- 目次：nav（EPUB 3）があればそこから、なければ toc.ncx（EPUB 2）から。
-- 見出しごとに、指しているファイルも持つ
tocOf :: Archive -> Package
      -> Either ImportProblem (Forest (Text, Maybe FilePath))
tocOf archive package =
  case (navFile package, ncxFile package) of
    (Just nav, _) -> navToc nav <$> xmlIn archive nav
    (_, Just ncx) -> ncxToc ncx <$> xmlIn archive ncx
    _             -> Left NoToc

-- EPUB 3: <nav epub:type="toc"><ol><li><a href="…">…</a><ol>…</ol></li>
navToc :: FilePath -> Cursor -> Forest (Text, Maybe FilePath)
navToc nav doc = concatMap list (take 1 (concatMap ($/ laxElement "ol") tocs))
  where
    -- the <nav> whose epub:type is toc / epub:type が toc の <nav>
    tocs    = filter (elem "toc" . laxAttribute "type")
                     (doc $// laxElement "nav")
    list ol = map item (ol $/ laxElement "li")
    item li = Node (tidy label, resolve nav <$> link)
                   (concatMap list (li $/ laxElement "ol"))
      where
        heads = li $/ (\c -> laxElement "a" c ++ laxElement "span" c)
        label = maybe li id (listToMaybe heads)
        link  = listToMaybe (concatMap (laxAttribute "href") (take 1 heads))

-- EPUB 2: <navMap><navPoint><navLabel><text>…</text></navLabel>
--         <content src="…"/><navPoint>…</navPoint></navPoint></navMap>
ncxToc :: FilePath -> Cursor -> Forest (Text, Maybe FilePath)
ncxToc ncx doc = map point (concatMap ($/ laxElement "navPoint") maps)
  where
    maps    = doc $// laxElement "navMap"
    point p = Node (tidy label, resolve ncx <$> src)
                   (map point (p $/ laxElement "navPoint"))
      where
        label = maybe p id (listToMaybe (p $/ laxElement "navLabel"))
        src   = listToMaybe
                  (p $/ (laxElement "content" >=> laxAttribute "src"))
-- ANCHOR_END: toc

-- ANCHOR: sizes
-- 📏 Give each heading what its files measure (characters, or text).
--    A file belongs to the first heading that points to it, and so do
--    the files after it, up to the next file a heading points to (a
--    chapter split into two files). Files before the first heading (the
--    cover) count for nobody.
-- 見出しごとに、ファイルをはかったもの（字数や本文）を配る。ファイルは、
-- それを指す最初の見出しのもの。その後ろの、どの見出しも指していない
-- ファイルも同じ見出しのもの（2つのファイルに分かれた章）。最初の見出し
-- より前のファイル（表紙）は、誰のものでもない
sizesOf :: Monoid m => Forest (Text, Maybe FilePath) -> [(FilePath, m)]
        -> Forest (Text, m)
sizesOf links counted = map (fmap sized) numbered
  where
    -- number the headings from top to bottom: 0, 1, 2, ...
    -- 見出しに、上から番号を振る：0, 1, 2, …
    (_, numbered) = mapAccumL (mapAccumL label) (0 :: Int) links
    label n x     = (n + 1, (n, x))
    -- each file: the first heading that points to it
    -- ファイルごとに、それを指す最初の見出しの番号
    firstOf = Map.fromListWith min
      [ (path, n) | (n, (_, Just path)) <- concatMap flatten numbered ]
    -- each page in reading order: whose it is (nobody's before the first)
    -- 読む順のページごとに、だれのものか（最初の見出しより前は、なし）
    owners = drop 1 (scanl owner Nothing counted)
    owner before (path, _) = maybe before Just (Map.lookup path firstOf)
    -- add up each heading's pages, in reading order (fromListWith
    -- puts the new one first, so flip it back)
    -- 見出しごとに、ページの分を読む順に足す（fromListWith は新しいほうを
    -- 前に置くので、flip で戻す）
    totals = Map.fromListWith (flip (<>))
      [ (n, part) | (Just n, (_, part)) <- zip owners counted ]
    sized (n, (heading, _)) = (heading, Map.findWithDefault mempty n totals)
-- ANCHOR_END: sizes

-- ANCHOR: helpers
-- 🔤 The characters of a page: the text in <body>, without spaces.
-- ページの字数：<body> の中の文字。空白は数えない
bodyChars :: Text -> Chars
bodyChars page = Chars (sum [ T.length (T.filter (not . isSpace) t)
                            | TagText t <- inBody (parseTags page) ])
  where
    inBody = drop 1 . dropWhile (not . isBody)
    isBody (TagOpen name _) = T.toLower name == "body"
    isBody _                = False

-- ANCHOR: bodyText
-- 📝 The text of a page: what is in <body>, one paragraph to a line,
--    without the ruby (<rt> and <rp>), so 竜鱗は is found as it is
--    written. Every page ends with a new line.
-- ページの本文：<body> の中を、1段落を1行に。ルビ（<rt> と <rp>）は外す
-- ので、「竜鱗は」は書いてあるとおりに見つかる。どのページも改行で終わる
bodyText :: Text -> Text
bodyText page = T.unlines (filter (not . T.null) (map T.strip lines'))
  where
    lines' = T.lines (T.concat (map piece (noRuby (inBody (parseTags page)))))
    inBody = drop 1 . dropWhile (not . isOpen "body")

    -- text as it is; a new line where a block (a paragraph...) ends
    -- 文字はそのまま。段落などのかたまりの切れ目で改行
    piece (TagText t)              = t
    piece (TagClose n) | isBlock n = "\n"
    piece (TagOpen "br" _)         = "\n"
    piece _                        = ""
    isBlock n = T.toLower n `elem` ["p", "div", "li", "h1", "h2", "h3"]

    noRuby (tag : rest)
      | isOpen "rt" tag = noRuby (after "rt" rest)
      | isOpen "rp" tag = noRuby (after "rp" rest)
      | otherwise       = tag : noRuby rest
    noRuby []           = []
    after name = drop 1 . dropWhile (not . isClose name)

    isOpen  name (TagOpen n _) = T.toLower n == name
    isOpen  _    _             = False
    isClose name (TagClose n)  = T.toLower n == name
    isClose _    _             = False
-- ANCHOR_END: bodyText

-- 🧭 A link inside the zip: relative to the file it is written in, and
--    without the part after # (a place inside the file).
-- zip の中のリンク。書いてあるファイルからの相対パスで、# の後ろ（ファイル
-- の中の場所）は外す
resolve :: FilePath -> Text -> FilePath
resolve from href = T.unpack (T.intercalate "/" (walk [] (folder ++ parts)))
  where
    -- the folder of `from`: every part but the file's name
    -- from のフォルダー：ファイル名のほかの部分
    folder = reverse (drop 1 (reverse (T.splitOn "/" (T.pack from))))
    parts  = T.splitOn "/" (T.takeWhile (/= '#') href)
    walk done (x : rest)
      | x == ".."           = walk (drop 1 done) rest
      | x == "." || T.null x = walk done rest
      | otherwise           = walk (x : done) rest
    walk done []            = reverse done

-- 📄 A file inside the zip, as XML. / zip の中のファイルを、XML として
xmlIn :: Archive -> FilePath -> Either ImportProblem Cursor
xmlIn archive path =
  case findEntryByPath path archive of
    Nothing    -> Left (MissingFile path)
    Just entry -> case parseLBS settings (fromEntry entry) of
      Left err  -> Left (BrokenXml path (T.pack (displayException err)))
      Right doc -> Right (fromDocument doc)
  where
    -- XHTML may use &nbsp; and friends / XHTML は &nbsp; なども使う
    settings = def { psDecodeEntities = decodeHtmlEntities }

-- 📄 A file inside the zip, as text (if it is there).
-- zip の中のファイルを、文字として（あれば）
fileText :: Archive -> FilePath -> Maybe Text
fileText archive path =
  decodeUtf8With lenientDecode . LBS.toStrict . fromEntry
    <$> findEntryByPath path archive

-- ✂️ The text of an element, with its spaces tidied.
-- 要素の中の文字。空白を整える
tidy :: Cursor -> Text
tidy c = T.unwords (T.words (T.concat (c $// content)))
-- ANCHOR_END: helpers
-- ANCHOR_END: all
