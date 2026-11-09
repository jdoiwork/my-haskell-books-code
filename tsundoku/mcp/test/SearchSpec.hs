-- ANCHOR: all
-- ANCHOR: imports
-- 🧪 Searching what is inside: the gate, the snippets, the trails, and
--    the texts of an EPUB. All pure: no shelf is opened here.
-- 中身を探すテスト：門番、抜き出し、道順、EPUB の本文。全部純粋で、
-- ここでは本棚を開かない
module SearchSpec (searchSpec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (flatten)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Tsundoku.Card (newCard)
import Tsundoku.Examples
import Tsundoku.Import (readEpubBook)
import Tsundoku.Import.Sample (sampleEpub)
import Tsundoku.Import.Types
import Tsundoku.Search
import Tsundoku.Types

import ShelfAnswers.TitlesFirst
-- ANCHOR_END: imports

-- ANCHOR: spec
searchSpec :: Spec
searchSpec = do
  -- 「門番：打ったものを、探す言葉にする」
  describe "the gate" $ do
    -- 「空白で切って、同じ語は1つに。何もなければ断る」
    it "cuts at the spaces, and refuses nothing at all" $ do
      searchWords <$> parseSearch "  宝  ドラゴン 宝 "
        `shouldBe` Right ["宝", "ドラゴン"]
      parseSearch " \n " `shouldBe` Left EmptySearch
    -- 「3文字からが FTS5 へ。中の " は2つ重ねる」
    it "gives FTS5 the long words, each one quoted" $ do
      ftsQuery <$> parseSearch "宝 怠けもの a\"b"
        `shouldBe` Right (Just "\"怠けもの\" \"a\"\"b\"")
      shortWords <$> parseSearch "宝 怠けもの a\"b" `shouldBe` Right ["宝"]
      ftsQuery <$> parseSearch "宝 冬" `shouldBe` Right Nothing
    -- 「PGroonga には全部の語を。" と \ の前に \」
    it "gives PGroonga every word, with \\ before \" and \\" $
      groongaQuery <$> parseSearch "宝 a\"b c\\d"
        `shouldBe` Right "\"宝\" \"a\\\"b\" \"c\\\\d\""

  -- 「しるしと抜き出し」
  describe "snippets" $ do
    -- 「しるしを付けても、文字は1つも変わらない」
    prop "keeps every character when it marks" $
      forAll smallText $ \text -> forAll smallWords $ \wanted ->
        T.concat (map pieceText (marked (searchOf wanted) text)) == text
    -- 「しるしの中身は、どれかの語（大文字・小文字は問わない）」
    prop "marks only the words, in any case" $
      forAll smallText $ \text -> forAll smallWords $ \wanted ->
        and [ T.toLower t `elem` map T.toLower wanted
            | Match t <- marked (searchOf wanted) text ]
    -- 「語があれば、どこかにしるしが付く」
    prop "marks a word wherever it is" $
      forAll smallText $ \front -> forAll smallText $ \back ->
        any isMatch (marked (searchOf ["ab"]) (front <> "AB" <> back))
    -- 「抜き出しは、最初に当たった所と、その前後だけ」
    it "keeps the first match, and a little around it" $ do
      let text = "冬のドラゴンは山の洞くつで宝を数えて過ごします"
      cutAround 3 (marked (searchOf ["宝"]) text)
        `shouldBe` [Plain "…くつで", Match "宝", Plain "を数え…"]
    -- 「抜き出しは長くなりすぎない」
    prop "never makes a long snippet" $
      forAll smallText $ \text ->
        let pieces = cutAround 5 (marked (searchOf ["a"]) text)
        in  T.length (T.concat (map pieceText pieces)) <= 2 * 5 + 1 + 2

  -- 「道順と、手で探す」
  describe "trails and searching by hand" $ do
    -- 「目次の上から、その項目まで」
    it "walks from the top of the table down to an entry" $ do
      trailTo (EntryId 4) (contents dragonBook)
        `shouldBe`
          ["第1部 ドラゴンを知る", "第1章 見分け方", "1.2 火を吐くもの"]
      trailTo (EntryId 99) (contents dragonBook) `shouldBe` []
      trailTo (EntryId 1) (contents lastDragonFilm) `shouldBe` []
    -- 「たくさん当たったものが先」
    it "puts the documents with more matches first" $ do
      let texts = Map.fromList [(EntryId 2, "竜。"), (EntryId 3, "竜と竜。")]
          card  = newCard dragonBook
          found = findIn (searchOf ["竜"]) [(WorkId 1, card, texts)]
      map place found `shouldBe` [InText (EntryId 3), InText (EntryId 2)]

  -- 「EPUB の本文」
  describe "the texts of an EPUB" $ do
    let got      = either (error . show) id (readEpubBook sampleEpub)
        rows     = concatMap flatten (draftToc (draft got))
        textOf h = maybe "" snd (lookup h rows)
    -- 「ページ数は、前の章と同じ」
    it "measures the same pages as before" $
      map (fst . snd) rows `shouldBe`
        map Pages [2, 4, 11, 10, 24, 2, 16, 12]
    -- 「ルビを外すので、竜鱗は で見つかる」
    it "leaves out the ruby" $ do
      textOf "1.1 うろこの色" `shouldSatisfy` T.isInfixOf "竜鱗は光で"
      textOf "1.1 うろこの色" `shouldNotSatisfy` T.isInfixOf "りゅうりん"
    -- 「2つのファイルに分かれた章は、読む順につなぐ」
    it "joins a chapter in two files, in reading order" $ do
      let text = textOf "第4章 怒らせたときは"
          (first, rest) = T.breakOn "怠けもの" text
      first `shouldSatisfy` T.isInfixOf "宝石を1つ置いて"
      rest  `shouldNotSatisfy` T.isInfixOf "宝石を1つ置いて"

  -- 「トレーニングの解答」
  describe "answers (search)" $
    -- 「題に当たったものを上に。ほかは順番のまま」
    it "puts the hits in a title first" $
      map place (titlesFirst (map hitAt
          [InText (EntryId 1), InTitle, InHeading (EntryId 2), InCreator]))
        `shouldBe`
          [InTitle, InCreator, InHeading (EntryId 2), InText (EntryId 1)]
  where
    hitAt at = Hit (WorkId 1) "" at [] []
-- ANCHOR_END: spec

-- ANCHOR: helpers
-- 🧰 A search made of words we know are fine.
-- 確かに大丈夫な語で作った Search
searchOf :: [Text] -> Search
searchOf wanted = either (error . show) id (parseSearch (T.unwords wanted))

pieceText :: Piece -> Text
pieceText (Plain t) = t
pieceText (Match t) = t

isMatch :: Piece -> Bool
isMatch (Match _) = True
isMatch (Plain _) = False

-- 🎲 Small texts and words from a few letters, so that the words are
--    found often (and in both cases).
-- 少しの文字でできた、短い文と語。語がよく見つかるように（大文字も小文字も）
smallText :: Gen Text
smallText = T.pack <$> listOf (elements "abAB 宝。")

smallWords :: Gen [Text]
smallWords = listOf1 (T.pack <$> listOf1 (elements "abAB宝"))
-- ANCHOR_END: helpers
-- ANCHOR_END: all
