-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}
-- 🧪 The API: what each route answers (hspec-wai, with no network),
--    and the client made from the same type, against a real server on
--    this machine (servant-client and warp).
-- API のテスト：道ごとの答え（hspec-wai。ネットワークを使わない）と、
-- 同じ型から作ったクライアントを、このマシンで動かした本物のサーバーに
-- つないで（servant-client と warp）
module ApiSpec (apiSpec) where

-- JSON, bytes, time, and HTTP
-- JSON、バイト列、日時、HTTP
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Network.HTTP.Types (Method, hContentType, methodDelete, methodPost,
                           methodPut)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)

-- the tests: hspec, QuickCheck, and hspec-wai
-- テスト：hspec、QuickCheck、hspec-wai
import Test.Hspec
import Test.Hspec.Wai hiding (pending)
import Test.Hspec.Wai.JSON (json)
import Test.QuickCheck

import Servant.API (NoContent (..))
import Servant.Client (ClientEnv, ClientM)

import Tsundoku.Api.Client
import Tsundoku.Api.Json
import Tsundoku.Api.Routes
import Tsundoku.Api.Server
import Tsundoku.Examples
import Tsundoku.Shelf
import Tsundoku.Shelf.Memory (newMemoryShelf)
import Tsundoku.Shelf.Sqlite (withSqliteShelf)
import Tsundoku.Types

import ShelfAnswers.Episodes
import ShelfAnswers.Paging
-- ANCHOR_END: imports

-- ANCHOR: helpers
-- ⏰ A clock that never moves, so the notes are always written at the
--    same time.
-- 止まった時計。感想は、いつも同じ時刻に書かれる
stopped :: Clock
stopped = pure (UTCTime (fromGregorian 2026 11 9) 0)

-- 🆕 The API on a new, empty shelf in memory (hspec-wai makes one for
--    every `it`).
-- 空っぽのメモリの本棚の上の API（hspec-wai は it ごとに作り直す）
freshApp :: IO Application
freshApp = shelfApp stopped <$> newMemoryShelf

-- 📮 Send JSON (or text) with a method.
-- メソッドを付けて、JSON（か文字）を送る
send :: Method -> ByteString -> LBS.ByteString -> WaiSession () SResponse
send method path = request method path [(hContentType, "application/json")]

-- 🚫 The answer is a refusal: this status, and this word in `mistake`.
-- 断りの答え：この状態で、mistake がこの言葉
refusedWith :: Int -> Text -> ResponseMatcher
refusedWith status word = ResponseMatcher
  { matchStatus  = status
  , matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
  , matchBody    = MatchBody $ \_ body ->
      case decode body :: Maybe Problem of
        Just problem | problem.mistake == word -> Nothing
        _ -> Just ("expected the mistake " <> show word
                   <> ", but got " <> show body)
  }

-- 📖 The dragon book, as JSON that comes in.
-- ドラゴン学の本を、入ってくる JSON に
dragonJson :: LBS.ByteString
dragonJson = [json|{
  title: "はじめてのドラゴン学",
  creator: "ギルド図書室",
  kind: "book",
  toc: [ { heading: "第1部", size: 2, children: [] },
         { heading: "第2部", size: 4, children: [] } ]
}|]
-- ANCHOR_END: helpers

-- ANCHOR: waiSpec
apiSpec :: Spec
apiSpec = do
  -- 「道ごとの答え（ネットワークなし）」
  describe "the routes" $ with freshApp $ do
    -- 「はじめは空っぽ」
    it "starts with an empty list" $
      get "/works" `shouldRespondWith` [json|[]|]
    -- 「作品を足すと 201 で、目次に番号が付いて返ってくる」
    it "adds a work with 201, its table numbered" $
      send methodPost "/works" dragonJson `shouldRespondWith` 201
        { matchBody = MatchBody $ \_ body ->
            case decode body :: Maybe CardDetail of
              Just card | map (.id) card.toc == [1, 2] -> Nothing
              _ -> Just ("not a card numbered 1, 2: " <> show body)
        }
    -- 「ない作品は 404」
    it "says 404 for a work not on the shelf" $
      get "/works/9" `shouldRespondWith` refusedWith 404 "no-such-work"
    -- 「消したら 204。もう一度見ると 404」
    it "removes a work with 204, and then it is gone" $ do
      _ <- send methodPost "/works" dragonJson
      request methodDelete "/works/1" [] "" `shouldRespondWith` 204
      get "/works/1" `shouldRespondWith` refusedWith 404 "no-such-work"
-- ANCHOR_END: waiSpec

  -- ANCHOR: gateSpec
  -- 「門番：受け取れないものは、窓口まで届かない」
  describe "the gate of the API" $ with freshApp $ do
    -- 「題が空、長すぎる、量がマイナスの作品は 422」
    it "refuses a work the core would not take, with 422" $ do
      send methodPost "/works"
        [json|{title: " ", creator: "", kind: "game", toc: []}|]
        `shouldRespondWith` refusedWith 422 "empty-title"
      send methodPost "/works" (encode (asNewWork longTitled))
        `shouldRespondWith` refusedWith 422 "long-title"
      send methodPost "/works"
        [json|{title: "a", creator: "", kind: "book",
               toc: [{heading: "序", size: -1, children: []}]}|]
        `shouldRespondWith` refusedWith 422 "negative-size"
    -- 「知らない言葉と、読めない JSON は 400」
    it "refuses words it does not know, and broken JSON, with 400" $ do
      send methodPost "/works"
        [json|{title: "a", creator: "", kind: "novel", toc: []}|]
        `shouldRespondWith` refusedWith 400 "unknown-word"
      send methodPost "/works" "{\"title\": "
        `shouldRespondWith` refusedWith 400 "bad-body"
      get "/works/abc" `shouldRespondWith` refusedWith 400 "bad-url"
      get "/nowhere" `shouldRespondWith` refusedWith 404 "no-such-route"
    -- 「★の点は 0〜100。それ以外は 422」
    it "takes points from 0 to 100 only" $ do
      _ <- send methodPost "/works" dragonJson
      send methodPut "/works/1/rating" [json|{points: 100}|]
        `shouldRespondWith` 204
      send methodPut "/works/1/rating" [json|{points: 101}|]
        `shouldRespondWith` refusedWith 422 "bad-score"
      send methodPut "/works/1/entries/9/rating" [json|{points: 50}|]
        `shouldRespondWith` refusedWith 404 "no-such-entry"
    -- 「いまの状態でできないことは 409」
    it "says 409 for what cannot be done now" $ do
      _ <- send methodPost "/works" dragonJson
      send methodPost "/works/1/reading" [json|{event: "Finish"}|]
        `shouldRespondWith` refusedWith 409 "not-allowed"
      send methodPost "/works/1/reading" [json|{event: "Start"}|]
        `shouldRespondWith` [json|{reading: "Reading",
                                   next: ["Finish", "Stop"]}|]
      send methodPost "/works/1/reading" [json|{event: "Jump"}|]
        `shouldRespondWith` refusedWith 400 "unknown-word"
    -- 「感想・ログ・検索の門」
    it "checks notes, log lines, and searches" $ do
      _ <- send methodPost "/works" dragonJson
      send methodPost "/works/1/notes"
        [json|{entry: null, body: "  ", spoiler: false}|]
        `shouldRespondWith` refusedWith 422 "empty-note"
      send methodPost "/works/1/notes"
        [json|{entry: 2, body: "読みやすい", spoiler: false}|]
        `shouldRespondWith` [json|{entry: 2, body: "読みやすい",
                                   writtenAt: "2026-11-09T00:00:00Z",
                                   spoiler: false}|] { matchStatus = 201 }
      send methodPost "/works/1/log" [json|{day: "11/9", entry: 1}|]
        `shouldRespondWith` refusedWith 400 "unknown-word"
      get "/search?q=%20" `shouldRespondWith` refusedWith 422 "empty-search"
      get "/search" `shouldRespondWith` refusedWith 400 "bad-url"
  -- ANCHOR_END: gateSpec

  -- ANCHOR: clientSpec
  -- 「同じ型から作ったクライアントで、本物のサーバーに」
  describe "the client" $ do
    let onMemory k = newMemoryShelf >>= k
    -- 「メモリでも SQLite でも、同じ答え」
    describe "on memory" $ around (withApi onMemory) clientSpec
    describe "on SQLite" $
      around (withApi (withSqliteShelf ":memory:")) clientSpec

  -- 「作品の形は、行って帰っても同じ」
  describe "the wire" $
    it "brings every example work back as it was" $
      map (workFrom . asNewWork) examples `shouldBe` map Just examples
  -- ANCHOR_END: clientSpec

  -- ANCHOR: answersSpec
  -- 「トレーニングの解答」
  describe "the answers" $ do
    -- 「新しい話の道：足すと目次が育つ。本には 409」
    it "adds new episodes to a serial, and not to a book" $
      withShelfOn episodeApp $ \shelf env -> do
        serial <- shelf.works.addWork librarianSerial
        book   <- shelf.works.addWork dragonBook
        let WorkId s = serial
            WorkId b = book
        ok_ env (episodeClient.addEpisodes s (NewEpisodes 2))
        Just card <- shelf.works.getCard serial
        fmap (.total) (summaryOf (serial, card)).progress `shouldBe` Just 5
        failed <- call env (episodeClient.addEpisodes b (NewEpisodes 1))
        fmap (.mistake) (either problemOf (const Nothing) failed)
          `shouldBe` Just "not-a-serial"
    -- 「ページ送り：limit は 1〜100。それ以外は servant が 400」
    it "gives a page of works, and refuses a limit of 0" $
      withShelfOn pagedApp $ \shelf env -> do
        mapM_ shelf.works.addWork examples
        Right two  <- pure (limit 2)
        Right skip <- pure (offset 1)
        got <- ok env (pagedClient.pageOfWorks (Just two) (Just skip))
        map (.id) got `shouldBe` [2, 3]
        Right zero <- pure (offset 0)
        everything <- ok env (pagedClient.pageOfWorks Nothing (Just zero))
        length everything `shouldBe` length examples
        limit 0 `shouldSatisfy` either (const True) (const False)
    -- 「limit=0 は、ハンドラーに届く前に servant が 400」
    describe "a limit out of 1 to 100" $
      with (pagedApp <$> newMemoryShelf) $
        it "never reaches the handler" $ do
          get "/works?limit=0"   `shouldRespondWith` 400
          get "/works?limit=abc" `shouldRespondWith` 400
          get "/works?limit=1"   `shouldRespondWith` [json|[]|]
  -- ANCHOR_END: answersSpec

-- ANCHOR: clientCases
-- 🔁 What the client should see, on any shelf.
-- どの本棚の上でも、クライアントから見えるはずのこと
clientSpec :: SpecWith (Shelf, ClientEnv)
clientSpec = do
  -- 「足して、読んで、評価して、カードを読み戻す」
  it "adds, reads, rates, and reads the card back" $ \(_, env) -> do
    added <- ok env (shelfClient.addWork (asNewWork dragonBook))
    let wid  = added.work.id
        here = shelfClient.work wid
    now <- ok env (here.happen (Happened "Start"))
    now.reading `shouldBe` "Reading"
    mapM_ (\line -> ok_ env (here.logReading line))
      [ ReadOn "2026-11-01" 1, ReadOn "2026-11-02" 2 ]
    ok_ env (here.rate (Points 90))
    card <- ok env here.card
    card.work.rating `shouldBe` Just 90
    card.work.progress `shouldBe` Just ProgressNumbers
      { done = 6, total = 78, unit = "pages", percent = 8 }
    map (.done) card.toc `shouldBe` [True, False]
  -- 「クライアントの一覧は、窓口の一覧と同じ」
  it "lists what the window lists" $ \(shelf, env) -> do
    mapM_ shelf.works.addWork examples
    fromApi    <- ok env shelfClient.listWorks
    fromWindow <- map summaryOf <$> shelf.works.listCards
    fromApi `shouldBe` fromWindow
  -- 「断られたら、理由が Problem で読める」
  it "reads why, when the server says no" $ \(_, env) -> do
    failed <- call env (shelfClient.work 9).card
    fmap (.mistake) (either problemOf (const Nothing) failed)
      `shouldBe` Just "no-such-work"
  -- 「本文をしまって、探すと、抜き出しにしるしが付いてくる」
  it "keeps a text, and finds it with the words marked" $ \(_, env) -> do
    added <- ok env (shelfClient.addWork (asNewWork dragonBook))
    let here = shelfClient.work added.work.id
    ok_ env (here.setText 8 "ドラゴンは怠けものではありません。")
    hits <- ok env (shelfClient.search "怠けもの")
    [ (hit.place, hit.entry, filter (.match) hit.snippet) | hit <- hits ]
      `shouldBe` [("text", Just 8, [Snip "怠けもの" True])]
  -- 「点は 0〜100 だけが通る（どんな数でも）」
  it "lets through points from 0 to 100, and no others" $ \(_, env) ->
    property $ \n -> ioProperty $ do
      added    <- ok env (shelfClient.addWork (asNewWork dungeonGame))
      answered <- call env ((shelfClient.work added.work.id).rate (Points n))
      pure (either (const False) (const True) answered
              === (0 <= n && n <= 100))
  -- ANCHOR: formCase
  -- 「属性のフォームは定義から。まずい値は 422」
  it "sends the form from the definitions, and checks edits" $
    \(shelf, env) -> do
      mapM_ shelf.custom.defineAttr libraryAttrs
      added <- ok env (shelfClient.addWork (asNewWork dragonBook))
      let here = shelfClient.work added.work.id
      fields <- ok env here.form
      map (.kind) fields
        `shouldBe` ["number", "text", "date", "yesno", "choice"]
      edited <- ok env (here.editAttrs (Map.fromList [("推し度", "4")]))
      [ f.current | f <- edited, f.label == "推し度" ] `shouldBe` ["4"]
      failed <- call env (here.editAttrs (Map.fromList [("推し度", "many")]))
      fmap (.mistake) (either problemOf (const Nothing) failed)
        `shouldBe` Just "bad-attrs"
  -- ANCHOR_END: formCase
-- ANCHOR_END: clientCases

-- ANCHOR: withApi
-- 🌐 Start a real server for a shelf, on a free port of this machine,
--    and give the test the shelf and a client pointed at it.
-- 本棚に本物のサーバーを立てる（このマシンの空いているポートで）。
-- テストには、本棚と、そこを向いたクライアントを渡す
withApi :: ((Shelf -> IO ()) -> IO ()) -> ((Shelf, ClientEnv) -> IO ())
        -> IO ()
withApi open test = open $ \shelf ->
  testWithApplication (pure (shelfApp stopped shelf)) $ \port -> do
    env <- localApi port
    test (shelf, env)

-- 🌐 The same, for the answers' small APIs on a memory shelf.
-- 同じことを、解答の小さな API とメモリの本棚で
withShelfOn :: (Shelf -> Application) -> (Shelf -> ClientEnv -> IO ())
            -> IO ()
withShelfOn app test = do
  shelf <- newMemoryShelf
  testWithApplication (pure (app shelf)) $ \port -> do
    env <- localApi port
    test shelf env

-- ✅ Ask, and fail the test if the server said no.
-- 聞いて、サーバーが断ったらテストを落とす
ok :: ClientEnv -> ClientM a -> IO a
ok env ask = call env ask >>= either (fail . show) pure

-- 📏 A game whose title is one character too long.
-- 題が1字だけ長すぎるゲーム
longTitled :: Work
longTitled = Work
  { title    = T.replicate 201 "あ"
  , creator  = ""
  , contents = Game
  }

-- ✅ The same, for a change that answers nothing (204 No Content).
-- 同じことを、何も答えない変更で（204 No Content）
ok_ :: ClientEnv -> ClientM NoContent -> IO ()
ok_ env ask = do
  NoContent <- ok env ask
  pure ()

-- 📚 Every kind of work, once.
-- 作品の種類を、1つずつ
examples :: [Work]
examples =
  [dragonBook, librarianSerial, detectiveDrama, lastDragonFilm, dungeonGame]
-- ANCHOR_END: withApi
