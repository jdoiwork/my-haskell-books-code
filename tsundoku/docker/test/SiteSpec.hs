-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}
-- 🧪 The review site on the wire: the shapes (pure), and the routes
--    (hspec-wai, on a shelf in memory with tagged works).
-- 線の上のレビューサイトのテスト：形（純粋に）と、道（hspec-wai。タグを
-- 付けた作品を置いた、メモリの本棚で）
module SiteSpec (siteSpec) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (methodPut)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.Wai.JSON (json)

import Tsundoku.Api.Json (WorkSummary (..))
import Tsundoku.Api.Server (shelfApp)
import Tsundoku.Api.Site
import Tsundoku.Card (Card (..), addNote)
import Tsundoku.Examples
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Types

import ApiSpec (refusedWith, send, stopped)
import ShelfAnswers.Alike (alikeApp)
import Tsundoku.Review (similarTo)
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- 🙈 The finished dragon book, with a spoiler on entry 8; and the same
--    book before it was finished (read up to entry 5).
-- 読み終えたドラゴン学に、項目 8 へのネタバレ。そして、読み終える前の
-- 同じ本（項目 5 まで読んだもの）
spoiled, halfway :: Card
spoiled = surely (addNote note finishedDragon)
  where
    note = Note (OnEntry (EntryId 8)) "最後に竜は眠る" (onDay 2026 11 9) True
halfway = spoiled { reading = Reading, readLog = dragonLog }

-- 🆕 The API on a shelf in memory with the six tagged works.
-- タグを付けた6作品を置いた、メモリの本棚の上の API
taggedApi :: IO Application
taggedApi = shelfApp stopped <$> taggedMemory

-- 📚 A shelf in memory with the six tagged works.
-- タグを付けた6作品を置いた、メモリの本棚
taggedMemory :: IO Shelf
taggedMemory = do
  shelf <- newMemoryShelf
  mapM_ (\(_, c) -> shelf.works.addWork c.work) taggedShelf
  mapM_ (\(wid, c) -> shelf.tagging.setTags wid c.tags) taggedShelf
  pure shelf

-- 🔍 The body read as this type, and this test about it.
-- 本体をこの型として読んで、それについてこのテスト
reading' :: Show a
         => (LBS.ByteString -> Maybe a) -> (a -> Bool) -> ResponseMatcher
reading' readIt ok = ResponseMatcher 200 [] $ MatchBody $ \_ body ->
  case readIt body of
    Just a | ok a -> Nothing
    other         -> Just ("unexpected: " <> show other)
-- ANCHOR_END: helpers

-- ANCHOR: siteSpec
siteSpec :: Spec
siteSpec = describe "the review site on the wire" $ do
  -- ANCHOR: shapeSpec
  -- 「隠した感想は、ヒントだけを線に乗せる」
  it "puts only the hint of a hidden note on the wire" $ do
    let item = extrasItem [(WorkId 1, halfway)] (WorkId 1) halfway
        wire = TE.decodeUtf8 (LBS.toStrict (encode item))
    [ () | ShownNote _ <- item.seen ] `shouldBe` [(), ()]
    [ (e, h) | HiddenNote e h <- item.seen ] `shouldBe`
      [(Just 8, "ネタバレ（「第4章 怒らせたときは」を読んだら見えます）")]
    "最後に竜は眠る" `T.isInfixOf` wire `shouldBe` False
    "\"tag\":\"HiddenNote\"" `T.isInfixOf` wire `shouldBe` True
  -- 「読み終えたら、隠すものはない」
  it "hides nothing once the work is finished" $ do
    let item = extrasItem [] (WorkId 1) spoiled
    [ () | HiddenNote _ _ <- item.seen ] `shouldBe` []
  -- 「似ている作品は、百分率で」
  it "says how alike, in percent" $ do
    let item = extrasItem taggedShelf (WorkId 1) finishedDragon
    [ (a.work, a.percent) | a <- item.alike ]
      `shouldBe` [(5, 25), (2, 20), (3, 20)]
  -- 「統計は、その年の月だけ」
  it "gives the statistics of one year" $ do
    let this = statsItem (nov 13) 2026 taggedShelf
    this.months `shouldBe` [MonthItem "2026-11" 1 78]
    map (.id) this.best `shouldBe` [1]
    (statsItem (nov 13) 2025 taggedShelf).months `shouldBe` []
    -- the streak and the paces are counted up to the day given
    -- 連続とペースは、渡した日まで数える
    this.streak `shouldBe` 3
    map (.work) this.reading `shouldBe` [2, 3, 4]
    (statsItem (nov 20) 2026 taggedShelf).streak `shouldBe` 0
  -- ANCHOR_END: shapeSpec

  -- ANCHOR: routeSpec
  describe "the routes" $ with taggedApi $ do
    -- 「タグの雲と、タグで絞った作品」
    it "answers the tag cloud, and the works with a tag" $ do
      get "/tags" `shouldRespondWith` reading' (decode @[TagCount])
        (\cloud -> take 1 cloud == [TagCount "魔法" 3])
      get "/tags/%E8%BF%B7%E5%AE%AE" `shouldRespondWith`
        reading' (decode @[WorkSummary]) (\ws -> map (.id) ws == [3, 6])
    -- 「タグを付け直すと、付いたタグが返る。門で断られたら 422」
    it "puts tags on a work, through the core's gate" $ do
      send methodPut "/works/1/tags" [json|["竜", "図鑑"]|]
        `shouldRespondWith` [json|["図鑑", "竜"]|]
      send methodPut "/works/1/tags" [json|[" 竜"]|]
        `shouldRespondWith` refusedWith 422 "bad-tag"
      send methodPut "/works/9/tags" [json|["竜"]|]
        `shouldRespondWith` refusedWith 404 "no-such-work"
    -- 「おまけ。ない作品は 404」
    it "answers the extras of a work, or 404" $ do
      get "/works/1/extras" `shouldRespondWith` reading' (decode @ExtrasItem)
        (\x -> x.tags == ["図鑑", "竜", "魔法"]
             && map (.work) x.alike == [5, 2, 3])
      get "/works/9/extras" `shouldRespondWith` refusedWith 404 "no-such-work"
    -- 「年を言わなければ、今日の年の統計」
    it "answers the statistics of this year, or of the year asked" $ do
      get "/stats" `shouldRespondWith`
        reading' (decode @StatsItem) ((== 2026) . (.year))
      get "/stats?year=2025" `shouldRespondWith`
        reading' (decode @StatsItem) (\s -> s.year == 2025 && null s.best)
      -- the reader's today decides this year, when the screen sends it
      -- 画面が読む人の今日を送れば、それが今年を決める
      get "/stats?today=2025-12-31" `shouldRespondWith`
        reading' (decode @StatsItem) ((== 2025) . (.year))
  -- ANCHOR_END: routeSpec
  -- ANCHOR: alikeSpec
  -- 「トレーニングのステップ2：似ている作品を n 個。n は 1〜20」
  describe "the answers" $ with (alikeApp <$> taggedMemory) $
    it "gives n works alike, as the core finds them" $ do
      let alike n = [ m | (_, WorkId m, _) <- similarTo n (WorkId 1)
                                                taggedShelf ]
          ids     = map (.work)
      get "/works/1/alike?n=2" `shouldRespondWith`
        reading' (decode @[AlikeItem]) (\xs -> ids xs == alike 2)
      get "/works/1/alike" `shouldRespondWith`
        reading' (decode @[AlikeItem]) (\xs -> ids xs == alike 3)
      get "/works/1/alike?n=21" `shouldRespondWith` 400
      get "/works/9/alike" `shouldRespondWith` refusedWith 404 "no-such-work"
  -- ANCHOR_END: alikeSpec
-- ANCHOR_END: siteSpec
-- ANCHOR_END: all
