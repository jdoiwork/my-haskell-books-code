-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
-- 🧪 The web pages: the parts as HTML text (pure), and what each route
--    answers, with hspec-wai (no network, no browser).
-- Web の画面のテスト：部品を HTML の文字として（純粋）、道ごとの答えを
-- hspec-wai で（ネットワークもブラウザーも使わない）
module HtmlSpec (htmlSpec) where

-- bytes, maps, text, time, and HTTP
-- バイト列、辞書、文字列、日時、HTTP
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime (..), fromGregorian)
import Network.HTTP.Types (Method, hContentType, methodPost, methodPut)
import Network.Wai (Application)

-- the tests: hspec, QuickCheck, and hspec-wai
-- テスト：hspec、QuickCheck、hspec-wai
import Test.Hspec
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Lucid (Html, renderText)

import Tsundoku.Api.Json (NoteItem (..))
import Tsundoku.Api.Server (Clock)
import Tsundoku.Examples
import Tsundoku.Html.Parts
import Tsundoku.Html.Routes (Fields (..))
import Tsundoku.Html.Server
import Tsundoku.Rating
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Types

import ShelfAnswers.LiveSearch
import ShelfAnswers.FinishBoth
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- ⏰ A clock that never moves.
-- 止まった時計
stopped :: Clock
stopped = pure (UTCTime (fromGregorian 2026 11 9) 0)

-- 🆕 The whole site on a new shelf in memory, with the dragon book and
--    the library's attributes. ★ is the style unless told otherwise.
-- 新しいメモリの本棚の上の、サイトの全体。ドラゴン学の本と、図書室の
-- 属性を置いておく。見せ方は、言われなければ ★
siteWith :: RatingStyle -> IO Application
siteWith style = do
  shelf <- newMemoryShelf
  _     <- shelf.works.addWork dragonBook
  mapM_ shelf.custom.defineAttr libraryAttrs
  pure (siteApp style stopped shelf)

-- 📮 What htmx sends: a form, with the header HX-Request.
-- htmx が送るもの：フォームと、ヘッダー HX-Request
htmx :: Method -> ByteString -> LBS.ByteString -> WaiSession () SResponse
htmx method path = request method path
  [ (hContentType, "application/x-www-form-urlencoded")
  , ("HX-Request", "true") ]

-- 🔍 The answer has this status, and these pieces in its body.
-- 答えの状態がこれで、本体にこのかけらが入っている
showing :: Int -> [Text] -> ResponseMatcher
showing status pieces = ResponseMatcher
  { matchStatus  = status
  , matchHeaders = []
  , matchBody    = MatchBody $ \_ body ->
      case [ p | p <- pieces, not (p `T.isInfixOf` asText body) ] of
        []      -> Nothing
        missing -> Just ("missing: " <> show missing <> "\nin: "
                         <> T.unpack (asText body))
  }
  where
    asText = TE.decodeUtf8 . LBS.toStrict

-- 📄 A part as the text of its HTML.
-- 部品を、HTML の文字に
html :: Html () -> Text
html = TL.toStrict . renderText
-- ANCHOR_END: helpers

-- ANCHOR: htmlSpec
-- 🧪 The parts, the routes, and the answers of the training.
-- 部品、道、トレーニングの解答
htmlSpec :: Spec
htmlSpec = do
  partsSpec
  routesSpec
  answersSpec
-- ANCHOR_END: htmlSpec

-- ANCHOR: partsSpec
partsSpec :: Spec
partsSpec = describe "the parts" $ do
  -- 「いまの評価の目盛りに印を付ける」
  it "marks the button of the rating now" $
    html (ratingPart FiveStars 1 (Just 73))
      `shouldSatisfy` T.isInfixOf "value=\"70\" class=\"now\">★3.5<"
  -- 「感想の中の HTML は、文字としてエスケープする」
  it "escapes HTML in a note" $ do
    let note = NoteItem Nothing "<script>alert(1)</script>" "2026-11-09" False
    html (notePart [] note) `shouldSatisfy`
      T.isInfixOf "&lt;script&gt;alert(1)&lt;/script&gt;"
    html (notePart [] note) `shouldNotSatisfy` T.isInfixOf "<script>"
  -- 「ネタバレの感想は <details> に畳む」
  it "folds a spoiler in <details>" $
    html (notePart [] (NoteItem Nothing "犯人は司書" "2026-11-09" True))
      `shouldSatisfy` T.isInfixOf "<details><summary>ネタバレ</summary>"

  describe "ratingFrom" $ do
    -- 「目盛りのボタンは、その点を送る」
    it "takes the points of a mark" $
      ratingFrom FiveStars (form [("points", "70")])
        `shouldBe` Just (score 70)
    -- 「打ちこんだ評価は、見せ方で読み戻す」
    it "reads a typed rating back with the style" $
      ratingFrom Hundred (form [("typed", "88点"), ("shown", "73点")])
        `shouldBe` Just (score 88)
    -- 「読めないものは BadRating」
    it "says BadRating for what it cannot read" $
      ratingFrom Hundred (form [("typed", "とても"), ("shown", "")])
        `shouldBe` Just (Left (BadRating "とても"))
    -- 「さわらずに保存したら、どの見せ方でも、点は変わらない」
    prop "keeps the points when saved untouched, in any style" $
      \(Small n) -> forAll (elements allStyles) $ \style ->
        case score (n `mod` 101) of
          Left _  -> property False
          Right s -> let shown = render style s
                     in  ratingFrom style
                           (form [("typed", shown), ("shown", shown)])
                           === Nothing
  where
    form = Fields . Map.fromList
-- ANCHOR_END: partsSpec

-- ANCHOR: routesSpec
routesSpec :: Spec
routesSpec = describe "the pages" $ do
  with (siteWith FiveStars) $ do
    -- 「本棚と作品のページを見せる。ない作品は 404」
    it "shows the shelf and a work, and 404 for no such work" $ do
      get "/" `shouldRespondWith` showing 200 ["はじめてのドラゴン学", "積読"]
      get "/works/1"
        `shouldRespondWith` showing 200 ["目次", "第1章 見分け方"]
      get "/works/9" `shouldRespondWith` showing 404 ["本棚にない作品です"]
    -- 「htmx と自分の見た目を出す」
    it "serves htmx and the style from inside the program" $ do
      get "/static/htmx.min.js" `shouldRespondWith` 200
      get "/static/shelf.css"   `shouldRespondWith` 200
      get "/static/theme.css"   `shouldRespondWith` 200
    -- 「JSON の API は /api の下に、そのまま」
    it "keeps the JSON API as it was, under /api" $
      get "/api/works/1" `shouldRespondWith` showing 200 ["\"rating\":null"]
    -- 「HX-Request のない変更は 403」
    it "refuses a change without HX-Request, with 403" $
      request methodPut "/works/1/rating"
              [(hContentType, "application/x-www-form-urlencoded")]
              "points=90"
        `shouldRespondWith` 403
    -- 「★を押すと、★の部品だけが返り、API の点も変わる」
    it "answers only the stars, and the API sees the points" $ do
      htmx methodPut "/works/1/rating" "points=70"
        `shouldRespondWith`
          showing 200 ["class=\"now\">★3.5<", "id=\"rating\""]
      get "/api/works/1" `shouldRespondWith` showing 200 ["\"rating\":70"]
    -- 「状態のボタンは、新しい状態と次のボタンを返す」
    it "moves the reading state, and draws the next buttons" $ do
      htmx methodPost "/works/1/reading" "event=Start"
        `shouldRespondWith` showing 200 ["読書中", "読み終えた", "中断する"]
      htmx methodPost "/works/1/reading" "event=Reread"
        `shouldRespondWith`
          showing 409 ["hx-swap-oob", "いまの読書の状態では"]
      htmx methodPost "/works/1/reading" "event=Jump"
        `shouldRespondWith` showing 400 ["「Jump」は event として読めません"]
    -- 「感想は <li> が1つ。空なら 422 で、箱にだけ理由」
    it "answers one <li> for a note, and 422 for an empty one" $ do
      -- body=<b>竜</b>, URL-encoded / URL エンコードした body=<b>竜</b>
      htmx methodPost "/works/1/notes"
                      "body=%3Cb%3E%E7%AB%9C%3C%2Fb%3E&entry=3"
        `shouldRespondWith`
          showing 200 ["<li>", "&lt;b&gt;竜&lt;/b&gt;", "1.1 うろこの色"]
      htmx methodPost "/works/1/notes" "body=+&entry="
        `shouldRespondWith` showing 422 ["感想が空っぽです"]
    -- 「属性のフォームは、定義で確かめる」
    it "checks the attributes with their definitions" $ do
      htmx methodPost "/works/1/attrs" "%E6%8E%A8%E3%81%97%E5%BA%A6=many"
        `shouldRespondWith` showing 422 ["推し度 に「many」は入りません"]
      htmx methodPost "/works/1/attrs" "%E6%8E%A8%E3%81%97%E5%BA%A6=4"
        `shouldRespondWith` showing 200 ["value=\"4\""]
    -- 「見つかった語に <mark> を付ける」
    it "marks the words it found" $ do
      get "/search?q=%E3%83%89%E3%83%A9%E3%82%B4%E3%83%B3"
        `shouldRespondWith` showing 200 ["<mark>ドラゴン</mark>"]
      get "/search?q=+"
        `shouldRespondWith` showing 422 ["探す言葉を入れてください"]

  with (siteWith Hundred) $
    -- 「1点刻みなら、打ちこんだ評価を読み戻す」
    it "reads a typed rating back, with a scale of 1 point" $ do
      htmx methodPut "/works/1/rating" "typed=88%E7%82%B9&shown="
        `shouldRespondWith` showing 200 ["value=\"88点\""]
      htmx methodPut "/works/1/rating" "typed=88%E7%82%B9&shown=88%E7%82%B9"
        `shouldRespondWith` showing 200 ["value=\"88点\""]
      get "/api/works/1" `shouldRespondWith` showing 200 ["\"rating\":88"]
-- ANCHOR_END: routesSpec

-- ANCHOR: answersSpec
answersSpec :: Spec
answersSpec = describe "the answers" $ do
  -- 「打つたびに探す箱は、keyup と delay を持つ」
  it "searches as you type" $
    html liveSearchBox `shouldSatisfy`
      T.isInfixOf "hx-trigger=\"input changed delay:300ms, search\""
  -- 「読み終えたら、状態とバーを一度に差し替える」
  with finishSite $
    it "swaps the state and the bar at once" $
      htmx methodPost "/works/1/finish" ""
        `shouldRespondWith` showing 200
          [ "id=\"reading\"", "読了"
          , "id=\"bar\" hx-swap-oob=\"true\"", "100%" ]
  where
    finishSite = do
      shelf <- newMemoryShelf
      _     <- shelf.works.addWork dragonBook
      _     <- shelf.reading.applyEvent (WorkId 1) Start
      pure (finishApp (pure (UTCTime (fromGregorian 2026 11 9) 0)) shelf)
-- ANCHOR_END: answersSpec
-- ANCHOR_END: all
