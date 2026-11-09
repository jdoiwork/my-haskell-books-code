-- ANCHOR: all
-- ANCHOR: imports
-- 🧪 Taking things in: ISBNs, EPUB, PDF, openBD, and the なろう小説API.
--    No test needs the network: the answers of the APIs are recorded
--    in samples/.
-- 取り込みのテスト：ISBN・EPUB・PDF・openBD・なろう小説API。ネットの要る
-- テストはない。API の答えは samples/ に記録してある
module ImportSpec (importSpec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Tree (Tree (..), flatten)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Directory
  (findExecutable, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Tsundoku.Card (Card (..))
import Tsundoku.Examples
import Tsundoku.Import
import Tsundoku.Import.Epub
import Tsundoku.Import.Narou
import Tsundoku.Import.OpenBd
import Tsundoku.Import.Pdf
import Tsundoku.Import.Sample
import Tsundoku.Import.Types
import Tsundoku.Isbn
import Tsundoku.Types

import ShelfAnswers.Depth
import ShelfAnswers.Isbn13
import ShelfAnswers.Ruby
-- ANCHOR_END: imports

-- ANCHOR: spec
importSpec :: Spec
importSpec = do
  isbnSpec
  epubSpec
  pdfSpec
  openBdSpec
  narouSpec
  answersSpec

-- 📏 The headings and amounts of a draft, from top to bottom.
-- 下書きの見出しと量を、上から順に
rowsOf :: Draft u -> [(Text, u)]
rowsOf = concatMap flatten . draftToc

-- 📏 The amounts of the dragon book typed by hand, from top to bottom.
-- 手で打ったドラゴン学の量を、上から順に
dragonPages :: [Pages]
dragonPages = map size (concatMap flatten dragonToc)
-- ANCHOR_END: spec

-- ANCHOR: isbn
isbnSpec :: Spec
isbnSpec = describe "ISBN" $ do
  -- 「人が書くとおりに読む」
  it "reads ISBNs as people write them" $ do
    text "978-4-10-101001-4" `shouldBe` Right "9784101010014"
    text "4 10 101001 3"     `shouldBe` Right "4101010013"
    -- an X at the end means 10 / 最後の X は 10
    text "0-8044-2957-x"     `shouldBe` Right "080442957X"
    isIsbn13 <$> parseIsbn "4101010013" `shouldBe` Right False
  -- 「まちがいを、理由と一緒に断る」
  it "says why it is not an ISBN" $ do
    parseIsbn "978-4-10-101001-5" `shouldBe` Left (WrongCheck '4' '5')
    parseIsbn "978-4-10-10100"    `shouldBe` Left (WrongLength 11)
    parseIsbn "97841010100A4"     `shouldBe` Left (NotADigit 'A')
  -- 「どの1けたを打ちまちがえても、見つける」
  prop "finds any one wrong digit" $
    forAll (vectorOf 12 digit) $ \body ->
    forAll (choose (0, 12)) $ \i ->
    forAll digit $ \d ->
      let good = isbn13Of (T.pack body)
          bad  = T.take i good <> T.singleton d <> T.drop (i + 1) good
      in  (T.index good i /= d) ==> isLeft (parseIsbn bad)
  -- 「ISBN-10 は、隣どうしの入れかえも見つける」
  prop "ISBN-10 finds two digits next to each other swapped" $
    forAll (vectorOf 9 digit) $ \body ->
    forAll (choose (0, 7)) $ \i ->
      let good = T.pack body <> T.singleton (checkDigit10 (T.pack body))
      in  (T.index good i /= T.index good (i + 1))
            ==> isLeft (parseIsbn (swapAt i good))
  -- 「ISBN-13 は、差が 5 の入れかえを見逃す」
  it "ISBN-13 misses a swap of two digits 5 apart" $ do
    let good = isbn13Of "978051234567"
    text (swapAt 3 good) `shouldBe` Right (swapAt 3 good)
  where
    digit = elements ['0' .. '9']
    text  = fmap isbnText . parseIsbn

-- 🔢 Twelve digits and their check digit.
-- 12けたと、そのチェックディジット
isbn13Of :: Text -> Text
isbn13Of body = body <> T.singleton (checkDigit13 body)

-- 🔀 Swap the characters at i and i + 1.
-- i 番目と i+1 番目を入れかえる
swapAt :: Int -> Text -> Text
swapAt i t =
  T.take i t <> T.pack [T.index t (i + 1), T.index t i] <> T.drop (i + 2) t
-- ANCHOR_END: isbn

-- ANCHOR: epub
epubSpec :: Spec
epubSpec = describe "EPUB" $ do
  -- 「EPUB 3 の nav から、手で打った目次と同じ見出しを読む」
  it "reads the same headings as the table typed by hand" $
    map fst . rowsOf . draft <$> readEpub sampleEpub
      `shouldBe` Right (map heading (concatMap flatten dragonToc))
  -- 「EPUB 2 の toc.ncx からも、同じ下書きを読む」
  it "reads the same draft from toc.ncx (EPUB 2)" $
    readEpub sampleEpub2 `shouldBe` readEpub sampleEpub
  -- 「ルビを数えるので、1.1 だけ多い。ほかは手で打ったとおり」
  it "counts ruby too, so only 1.1 has more pages" $
    map snd . rowsOf . draft . fmap (perPage charsPerPage)
        <$> readEpub sampleEpub
      `shouldBe` Right (map Pages [2, 4, 11, 10, 24, 2, 16, 12])
  -- 「題と作者も読む。欠けたものはない」
  it "reads the title and creator, and misses nothing" $ do
    let got = readEpub sampleEpub
    draftTitle   . draft <$> got
      `shouldBe` Right (Just "はじめてのドラゴン学")
    draftCreator . draft <$> got `shouldBe` Right (Just "ギルド図書室")
    missed <$> got `shouldBe` Right []
  -- 「章のファイルが欠けていても、止まらずに、欠けたことを返す」
  it "goes on without a missing chapter, and says so" $ do
    let got = readEpub (zipFiles (without "OEBPS/ch2.xhtml"))
    missed <$> got `shouldBe` Right [MissingFile "OEBPS/ch2.xhtml"]
    lookup "第2章 すみかと季節" . rowsOf . draft <$> got
      `shouldBe` Right (Just (Chars 0))
  -- 「止まるしかないときは、その理由」
  it "says why it cannot go on" $ do
    readEpub "hello" `shouldSatisfy` isNotAZip
    readEpub (zipFiles (without "META-INF/container.xml"))
      `shouldBe` Left (MissingFile "META-INF/container.xml")
    readEpub (zipFiles (map noNav (epubFiles Epub3)))
      `shouldBe` Left NoToc
  -- 「リンクは、書いてあるファイルからの相対パス」
  it "follows links from the file they are written in" $ do
    resolve "OEBPS/text/ch1.xhtml" "../images/a.png#top"
      `shouldBe` "OEBPS/images/a.png"
    resolve "OEBPS/nav.xhtml" "ch1.xhtml#s2" `shouldBe` "OEBPS/ch1.xhtml"
    resolve "content.opf"     "./ch1.xhtml"  `shouldBe` "ch1.xhtml"
  where
    without path = filter ((/= path) . fst) (epubFiles Epub3)
    isNotAZip (Left (NotAZip _)) = True
    isNotAZip _                  = False
    -- the package file with no nav item / nav の item のないパッケージ
    noNav (path, bytes)
      | path == "OEBPS/content.opf" = (path, dropNav bytes)
      | otherwise                   = (path, bytes)
    dropNav = LBS.fromStrict . encodeUtf8
            . T.replace "properties=\"nav\"" ""
            . decodeUtf8 . LBS.toStrict
-- ANCHOR_END: epub

-- ANCHOR: pdf
pdfSpec :: Spec
pdfSpec = describe "PDF" $ do
  -- 「しおりの始まるページから、項目ごとのページ数を出す」
  it "gives each bookmark its own pages" $
    fromStarts 6 [Node ("題", 1) [leaf "I" 2, leaf "II" 4, leaf "III" 6]]
      `shouldBe`
        [Node ("題", Pages 1)
              [leaf "I" (Pages 2), leaf "II" (Pages 2), leaf "III" (Pages 1)]]
  -- 「順に並んだしおりなら、足すと最初のページから本の終わりまで」
  prop "adds up to the whole book, from the first bookmark" $
    forAll (listOf1 (choose (1, 20))) $ \steps ->
      let starts = scanl (+) 1 (init steps)
          whole  = sum steps
          marks  = [ leaf (T.pack (show s)) s | s <- starts ]
          pages  = [ n | (_, Pages n) <- concatMap flatten
                                                   (fromStarts whole marks) ]
      in  sum pages === whole .&&. all (> 0) pages
  -- 「前より前のページから始まる見出しは、知らせる」
  it "says which bookmark starts before the one above" $
    outOfOrder [leaf "a" 1, leaf "b" 9, leaf "c" 5]
      `shouldBe` [OutOfOrder "c"]
  -- 「pdftohtml の XML の、入れ子のしおりを読む」
  it "reads nested bookmarks from pdftohtml's XML" $
    outlineOf (LBS.fromStrict (encodeUtf8 outlineXml))
      `shouldBe` Right [Node ("題", 1) [leaf "I" 2, leaf "II" 4]]
  -- 「pdfinfo の行を読む」
  it "reads the lines of pdfinfo" $
    Map.lookup "Pages" (infoFields "Title:  Alice\nPages:  6\nTagged:  no\n")
      `shouldBe` Just "6"
  -- 「道具がなければ、ないと言う」
  it "says when the tool is not there" $ do
    got <- readPdfWith poppler { infoTool = "no-such-pdfinfo" } "alice.pdf"
    got `shouldBe` Left (NoTool "no-such-pdfinfo")
  -- 「poppler があれば、本物の PDF を読む」
  it "reads a real PDF with poppler" $ do
    found <- findExecutable "pdftohtml"
    case found of
      Nothing -> pendingWith "poppler（pdfinfo・pdftohtml）がありません"
      Just _  -> do
        dir <- getTemporaryDirectory
        let marked = dir </> "tsundoku-alice.pdf"
            plain  = dir </> "tsundoku-plain.pdf"
        LBS.writeFile marked samplePdf
        LBS.writeFile plain (pdfBook False)
        got  <- readPdf marked
        flat <- readPdf plain
        mapM_ removeFile [marked, plain]
        map snd . rowsOf . draft <$> got
          `shouldBe` Right (map Pages [1, 2, 2, 1])
        draftCreator . draft <$> got `shouldBe` Right (Just "Lewis Carroll")
        missed <$> flat `shouldBe` Right [NoOutline]
  where
    outlineXml = T.unlines
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<pdf2xml><page number=\"1\"><text>x</text></page>"
      , "<outline><item page=\"1\">題</item>"
      , "<outline><item page=\"2\">I</item><item page=\"4\">II</item>"
      , "</outline></outline></pdf2xml>" ]

-- 🍃 A heading with no children. / 子のない見出し
leaf :: Text -> a -> Tree (Text, a)
leaf heading' amount = Node (heading', amount) []
-- ANCHOR_END: pdf

-- ANCHOR: openBd
openBdSpec :: Spec
openBdSpec = describe "openBD" $ do
  -- 「記録した答えを読む。ページ数と目次も」
  it "reads a recorded answer, with pages and the table" $ do
    recorded <- LBS.readFile "samples/openbd-dragon.json"
    let book = parseOpenBd dragonIsbn recorded
    bookTitle <$> book `shouldBe` Right "はじめてのドラゴン学"
    bookPages <$> book `shouldBe` Right (Just (Pages 78))
    -- the table is flat, with the pages typed by hand
    -- 目次は平らで、ページ数は手で打ったとおり
    map snd . rowsOf . draft . bookDraft <$> book
      `shouldBe` Right dragonPages
  -- 「知らない本は null」
  it "knows when openBD does not know the book" $ do
    recorded <- LBS.readFile "samples/openbd-unknown.json"
    parseOpenBd dragonIsbn recorded `shouldBe` Left (UnknownIsbn dragonIsbn)
  -- 「文字の目次を、見出しとページに分ける」
  it "splits a table written as text" $
    tocFromText (T.unlines [ "はじめに……002", "ご飯の炊き方 ……008", ""
                           , "1. 中に入れる FILL……019", "奥付" ])
      `shouldBe`
        [("はじめに", 2), ("ご飯の炊き方", 8), ("1. 中に入れる FILL", 19)]
  -- 「つながらなければ、そう言う（閉じたポートに聞く）」
  it "says when it cannot connect" $ do
    manager <- newManager defaultManagerSettings
    got     <- fetchBook manager "http://127.0.0.1:9" dragonIsbn
    got `shouldSatisfy` isOffline
  where
    isOffline (Left (Offline _)) = True
    isOffline _                  = False

-- 📗 The (made-up) ISBN of the dragon book. / ドラゴン学の（架空の）ISBN
dragonIsbn :: Isbn
dragonIsbn = either (error . show) id (parseIsbn "978-4-9999-0001-5")
-- ANCHOR_END: openBd

-- ANCHOR: narou
narouSpec :: Spec
narouSpec = describe "なろう小説API" $ do
  -- 「最初の1つだけ形のちがう答えを読む」
  it "reads an answer whose first item is different" $ do
    novels <- parseNovels <$> LBS.readFile "samples/narou.json"
    map ncode    <$> novels `shouldBe` Right ["N0000XX", "N0000YY"]
    map episodes <$> novels `shouldBe` Right [5, 30]
    map run      <$> novels `shouldBe` Right [Ongoing, Completed]
  -- 「聞くのは、使う項目だけを、まとめて1回で」
  it "asks for every novel at once, with only the fields we use" $
    novelsUrl narou ["N0000XX", "n0000yy"] `shouldBe` concat
      [ "https://api.syosetu.com/novelapi/api/?out=json&gzip=5"
      , "&of=n-t-w-ga-nt-e-gl&lim=2&ncode=n0000xx-n0000yy" ]
  -- 「カードにない話の数。知らない作品は、知らないと言う」
  it "counts the episodes the card does not have yet" $ do
    Right novels <- parseNovels <$> LBS.readFile "samples/narou.json"
    let cards = [ (WorkId 2, withCode "n0000xx" librarianCard)
                , (WorkId 3, withCode "n0000yy" copyistCard)
                , (WorkId 4, withCode "n0000zz" librarianCard)
                , (WorkId 5, dragonCard) ]
        found = updatesFor novels (narouSerials cards)
    [ (wid, snd <$> answer) | (wid, answer) <- found ] `shouldBe`
      [ (WorkId 2, Right 2), (WorkId 3, Right 0)
      , (WorkId 4, Left (UnknownNovel "n0000zz")) ]
  where
    withCode code card =
      card { attrs = Map.fromList [("Nコード", TextV code)] }
-- ANCHOR_END: narou

-- ANCHOR: answers
answersSpec :: Spec
answersSpec = describe "トレーニングの解答" $ do
  -- 「ルビを数えなければ、手で打った目次と同じページ数」
  it "counts no ruby, and the pages are as typed by hand" $
    map snd . rowsOf . draft . fmap (perPage charsPerPage)
        <$> readEpubWith charsWithoutRuby sampleEpub
      `shouldBe` Right dragonPages
  -- 「3段目より深い見出しのページは、2段目に寄せる」
  it "folds the third level into the second" $
    fmap (fmap pagesOf) (twoLevels (map (fmap entryRow) dragonToc))
      `shouldBe`
        [ Node ("第1部 ドラゴンを知る", 2)
            [leaf "第1章 見分け方" 22, leaf "第2章 すみかと季節" 24]
        , Node ("第2部 ドラゴンと話す", 2)
            [leaf "第3章 あいさつの作法" 16, leaf "第4章 怒らせたときは" 12] ]
  -- 「ISBN-10 を 13 にしても、同じ本。13 はそのまま」
  it "turns an ISBN-10 into 13" $ do
    to13 "4-10-101001-3" `shouldBe` Right "9784101010014"
    to13 "0-8044-2957-X" `shouldBe` Right "9780804429573"
    to13 "9784101010014" `shouldBe` Right "9784101010014"
  -- 「どの ISBN-10 からも、正しい ISBN-13 ができる」
  prop "makes a right ISBN-13 from any ISBN-10" $
    forAll (vectorOf 9 (elements ['0' .. '9'])) $ \body ->
      let ten = T.pack body <> T.singleton (checkDigit10 (T.pack body))
      in  case toIsbn13 <$> parseIsbn ten of
            Right new -> isIsbn13 new && parseIsbn (isbnText new) == Right new
            Left _    -> False
  where
    entryRow e           = (heading e, size e)
    pagesOf (h, Pages n) = (h, n)
    to13                 = fmap (isbnText . toIsbn13) . parseIsbn
-- ANCHOR_END: answers
-- ANCHOR_END: all
