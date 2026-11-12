-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 The review site: tags (the core's gate), spoilers by how far I have
--    read, the statistics, works alike, writing out, and the pages.
-- レビューサイトのテスト：タグ（核の門）、どこまで読んだかでネタバレ、
-- 統計、似ている作品、書き出し、ページ
module ReviewSpec (reviewSpec) where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime (..), fromGregorian)
import Network.HTTP.Types (hContentType, methodPost)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai hiding (pending)
import Test.QuickCheck

import Tsundoku.Card
import Tsundoku.Examples
import Tsundoku.Html.Server (siteApp)
import Tsundoku.Rating (RatingStyle (..))
import Tsundoku.Review
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Types

import ShelfAnswers.CsvIn
import ShelfAnswers.Reread
import ShelfAnswers.StrictSpoiler
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- 🙈 The dragon card with a spoiler on entry 8 (not read yet).
-- 項目 8（まだ読んでいない）にネタバレを書いた、ドラゴン学のカード
spoiled :: Card
spoiled = surely (addNote note dragonCard)
  where
    note = Note (OnEntry (EntryId 8)) "最後に竜は眠る" (onDay 2026 11 9) True

-- 📝 The bodies of the notes I may see, and the hints of the others.
-- 見てよい感想の本文と、ほかの感想のヒント
shown :: Card -> [Either Text Text]
shown card = [ see s | s <- seenNotes card ]
  where
    see (Shown n)  = Right n.body
    see (Hidden t) = Left (hiddenHint card.work.contents t)

-- 🆕 The whole site on a shelf in memory with three tagged works.
-- タグを付けた3作品を置いた、メモリの本棚の上のサイト全体
taggedSite :: IO Application
taggedSite = do
  shelf <- newMemoryShelf
  mapM_ (\(_, c) -> shelf.works.addWork c.work) (take 3 taggedShelf)
  mapM_ (\(wid, c) -> shelf.tagging.setTags wid c.tags) (take 3 taggedShelf)
  pure (siteApp FiveStars (pure (UTCTime (fromGregorian 2026 11 9) 0)) shelf)

-- 🔍 The answer has this status, and these pieces in its body.
-- 答えの状態がこれで、本体にこのかけらが入っている
showing :: Int -> [Text] -> ResponseMatcher
showing status pieces = ResponseMatcher status [] $ MatchBody $ \_ body ->
  case [ p | p <- pieces, not (p `T.isInfixOf` TE.decodeUtf8 (LBS.toStrict body)) ] of
    []      -> Nothing
    missing -> Just ("missing: " <> show missing)
-- ANCHOR_END: helpers

-- ANCHOR: reviewSpec
reviewSpec :: Spec
reviewSpec = do
  tagSpec
  spoilerSpec
  statsSpec
  exportSpec
  pageSpec
  answersSpec
-- ANCHOR_END: reviewSpec

-- ANCHOR: tagSpec
tagSpec :: Spec
tagSpec = describe "tags" $ do
  -- 「カンマで分けて、前後の空白を落とす」
  it "reads tags typed in one box" $ do
    readTags "推理, 魔法、図書館 , " `shouldBe` Set.fromList ["推理", "魔法", "図書館"]
    -- the full-width comma splits too / 全角のカンマでも分ける
    readTags "推理，魔法" `shouldBe` Set.fromList ["推理", "魔法"]
  -- 「空・カンマ入り・長すぎ・多すぎは断る」
  it "refuses blank, comma, too long, and too many" $ do
    (.tags) <$> setTags (Set.fromList ["竜"]) dragonCard
      `shouldBe` Right (Set.fromList ["竜"])
    fmap (.tags) (setTags (Set.fromList [" 竜"]) dragonCard) `shouldBe` Left (BadTag " 竜")
    fmap (.tags) (setTags (Set.fromList ["a,b"]) dragonCard) `shouldBe` Left (BadTag "a,b")
    -- 、 and ，, and control characters (a line break, NUL)
    -- 、と ，、制御文字（改行・NUL）
    forM_ ["推理、魔法", "推理，魔法", "a\nb", "a\0b"] $ \t ->
      fmap (.tags) (setTags (Set.fromList [t]) dragonCard) `shouldBe` Left (BadTag t)
    fmap (.tags) (setTags (Set.fromList [T.replicate 31 "x"]) dragonCard)
      `shouldBe` Left (BadTag (T.replicate 31 "x"))
    let many = Set.fromList [ T.pack (show n) | n <- [1 .. 21 :: Int] ]
    fmap (.tags) (setTags many dragonCard) `shouldBe` Left (TooManyTags 21)
  -- 「打ったタグは、いつも核の門を通る」
  prop "lets readTags' answer through the gate, when few enough" $
    \(Small n) -> forAll (vectorOf (n `mod` 10) word) $ \ws ->
      let typed = T.intercalate ", " ws
          tags  = readTags typed
      in  Set.size tags <= tagLimit ==>
            fmap (.tags) (setTags tags dragonCard) === Right tags
  where
    -- words of at most 10 letters (a longer tag is refused: see above)
    -- 10 文字までの言葉（もっと長いタグは断られる。上のテスト）
    word = T.pack <$> resize 10 (listOf1 (elements "竜魔法図書館 推理"))
-- ANCHOR_END: tagSpec

-- ANCHOR: spoilerSpec
spoilerSpec :: Spec
spoilerSpec = describe "spoilers" $ do
  -- 「読んだ項目のネタバレは見せ、まだの項目は、いつ見えるかを言う」
  it "shows spoilers on read entries, and hints at the rest" $
    shown spoiled `shouldBe`
      [ Right "絵が多くて読みやすい", Right "火を吐かない種もいるとは！"
      , Left "ネタバレ（「第4章 怒らせたときは」を読んだら見えます）" ]
  -- 「読み終えたら、全部見える」
  it "shows everything once the work is finished" $
    map (either (const False) (const True)) (shown (surely (applyEvent Finish spoiled)))
      `shouldBe` [True, True, True]
  -- 「隠した感想は、本文を持たない」
  prop "never carries the body of a hidden note" $ \(Small k) ->
    let logged = take (k `mod` 9) dragonLog
        card   = spoiled { readLog = logged }
    in  [ () | Hidden _ <- seenNotes card ] /= [] ==>
          all (\s -> case s of Hidden _ -> True; Shown n -> n.body /= "") (seenNotes card)
-- ANCHOR_END: spoilerSpec

-- ANCHOR: statsSpec
statsSpec :: Spec
statsSpec = describe "statistics" $ do
  let cards = map snd taggedShelf
  -- 「読み終えた日は、ログの最後の日」
  it "finds the day a work was finished" $ do
    finishedOn finishedDragon `shouldBe` Just (nov 12)
    finishedOn dragonCard     `shouldBe` Nothing
  -- 「月ごとに、読み終えた数と読んだページ」
  it "counts works finished and pages read, month by month" $ do
    monthly cards `shouldBe`
      Map.fromList [(Month 2026 11, MonthStat 1 (Pages 78))]
    -- two works finished in the same month add up
    -- 同じ月に読み終えた2冊は、足し合わさる
    monthly [finishedDragon, finishedDragon] `shouldBe`
      Map.fromList [(Month 2026 11, MonthStat 2 (Pages 156))]
  -- 「タグの雲は、多い順、同じなら名前の順」
  it "makes a tag cloud, most first" $
    take 3 (tagCloud cards) `shouldBe` [("魔法", 3), ("冒険", 2), ("図書館", 2)]
  -- 「その年のベスト」
  it "picks the best of a year" $
    map fst (yearTop 2026 10 taggedShelf) `shouldBe` [WorkId 1]
  -- 「Jaccard：共有するもの ÷ どちらかにあるもの」
  it "measures how alike two sets of tags are" $ do
    let a = Set.fromList ["竜", "魔法"] :: Set.Set Text
    jaccard a (Set.fromList ["魔法", "図書館"]) `shouldBe` 1 / 3
    jaccard (Set.empty :: Set.Set Text) Set.empty `shouldBe` 0
    [ w | (_, w, _) <- similarTo 3 (WorkId 1) taggedShelf ]
      `shouldBe` [WorkId 5, WorkId 2, WorkId 3]
    -- a work with no tag in common is never "alike"
    -- 共有するタグのない作品は、似ていない
    length (similarTo 10 (WorkId 1) taggedShelf) `shouldBe` 3
  -- 「似ている度合いは 0〜1、向きを入れかえても同じ」
  prop "is between 0 and 1, and the same both ways" $ \a b ->
    let x = Set.fromList (a :: [Int]); y = Set.fromList b
    in  (jaccard x y, 0 <= jaccard x y && jaccard x y <= 1) === (jaccard y x, True)
-- ANCHOR_END: statsSpec

-- ANCHOR: exportSpec
exportSpec :: Spec
exportSpec = describe "writing out" $ do
  -- 「CSV は RFC 4180：カンマや引用符を含むフィールドはくるむ」
  it "quotes CSV fields with commas and quotes" $ do
    csvField "司書" `shouldBe` "司書"
    csvField "竜,魔法" `shouldBe` "\"竜,魔法\""
    csvField "1行目\n2行目" `shouldBe` "\"1行目\n2行目\""
    csvField "司書は「しーっ」と\"言った\"" `shouldBe` "\"司書は「しーっ」と\"\"言った\"\"\""
  -- 「書き出しの見出しの行と、作品の行」
  it "writes a header and a row for each work" $
    take 2 (T.lines (toCsv (take 1 taggedShelf))) `shouldBe`
      [ "id,title,creator,kind,reading,rating,tags,finished_on\r"
      , "1,はじめてのドラゴン学,ギルド図書室,book,Finished,90,図鑑 竜 魔法,2026-11-12\r" ]
  -- 「Markdown でも、隠したネタバレは隠したまま」
  it "keeps hidden spoilers hidden in Markdown too" $ do
    let md = toMarkdown [(WorkId 1, spoiled)]
    md `shouldSatisfy` T.isInfixOf "（ネタバレ（「第4章 怒らせたときは」を読んだら見えます））"
    md `shouldNotSatisfy` T.isInfixOf "最後に竜は眠る"
    -- each note its own quote, with a blank line after it
    -- 感想ごとに別の引用。あとに空行
    md `shouldSatisfy` T.isInfixOf "> 絵が多くて読みやすい\n\n> "
-- ANCHOR_END: exportSpec

-- ANCHOR: pageSpec
pageSpec :: Spec
pageSpec = describe "the review pages" $ with taggedSite $ do
  -- 「タグで本棚を絞る」
  it "filters the shelf by a tag" $ do
    get "/?tag=%E8%BF%B7%E5%AE%AE"
      `shouldRespondWith` showing 200 ["迷宮図書館の写本係"]
    -- and the works without the tag are gone
    -- タグのない作品は、出ない
    get "/?tag=%E8%BF%B7%E5%AE%AE" `shouldRespondWith` 200
      { matchBody = MatchBody $ \_ body ->
          if "はじめてのドラゴン学" `T.isInfixOf` TE.decodeUtf8 (LBS.toStrict body)
            then Just "a work without the tag is shown"
            else Nothing }
  -- 「カードのページに、タグと似ている作品」
  it "shows tags and works alike on a card's page" $
    get "/works/1" `shouldRespondWith`
      showing 200 ["#魔法", "この作品が好きなら", "/works/2"]
  -- 「タグを保存すると、タグの部品だけが返る」
  it "saves tags and answers the tags part" $ do
    request methodPost "/works/1/tags"
      [ (hContentType, "application/x-www-form-urlencoded")
      , ("HX-Request", "true") ]
      "tags=%E6%8E%A8%E7%90%86%2C+%E7%AB%9C"
      `shouldRespondWith` showing 200 ["id=\"tags\"", "#推理", "#竜"]
    -- a tag with # goes into the link URL-encoded (C# → C%23)
    -- # 入りのタグは、URL の形にしてリンクに入る（C# → C%23）
    request methodPost "/works/1/tags"
      [ (hContentType, "application/x-www-form-urlencoded")
      , ("HX-Request", "true") ]
      "tags=C%23"
      `shouldRespondWith` showing 200 ["href=\"/?tag=C%23\""]
  -- 「統計のページ」
  it "shows the statistics" $
    get "/stats" `shouldRespondWith` showing 200 ["読書の統計", "#魔法", "2026年のベスト"]
-- ANCHOR_END: pageSpec

-- ANCHOR: answersSpec
answersSpec :: Spec
answersSpec = describe "the answers" $ do
  -- 「★4 以上で、1年より前に読み終えた作品」
  it "finds works to read again" $ do
    map fst (wantReread (fromGregorian 2027 12 1) taggedShelf) `shouldBe` [WorkId 1]
    map fst (wantReread (fromGregorian 2027 1 1) taggedShelf) `shouldBe` []
  -- 「CSV の1行は、書いたとおりに読み戻せる」
  prop "reads a CSV row back as it was written" $ \ws ->
    let fields = map T.pack ws
    in  not (null fields) ==>
          csvRow (T.intercalate "," (map csvField fields)) === fields
  -- 「章の題も隠すヒント」
  it "hides the entry's title too" $
    strictHint dragonCard.work.contents (OnEntry (EntryId 8))
      `shouldBe` "ネタバレ（目次の 8 番目を読んだら見えます）"
-- ANCHOR_END: answersSpec
-- ANCHOR_END: all
