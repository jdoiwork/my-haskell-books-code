-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Options.Applicative
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)
import Test.Hspec.Wai.Matcher (bodyEquals)
import Test.QuickCheck
import Control.Concurrent.STM
import Data.IORef
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

import MiniSearch.Answers
import MiniSearch.Api
import MiniSearch.Cli
import MiniSearch.Index
import MiniSearch.Live
import MiniSearch.Load
import MiniSearch.Naive
import MiniSearch.Query
import MiniSearch.Report
import MiniSearch.Samples
import MiniSearch.Search
import MiniSearch.Snippet
import MiniSearch.Store
import MiniSearch.Tokenize
import MiniSearch.Types

vocabulary :: [T.Text]
vocabulary = ["dragon", "fire", "ice", "gold", "slime", "cave"]

genQuery :: Gen T.Text
genQuery = T.unwords <$> listOf1 (elements vocabulary)

-- 🎯 Two scores are the same, up to rounding.
-- 2つの点数が、丸めの誤差を除いて同じ
close :: Double -> Double -> Bool
close a b = abs (a - b) < 1e-9

genDocs :: Gen [Document]
genDocs = do
  n <- chooseInt (0, 8)
  mapM genDoc [1 .. n]
  where
    genDoc i = do
      ws <- listOf (elements vocabulary)
      pure (Document (DocId i) (T.pack (show i)) (T.unwords ws))
-- ANCHOR_END: head

-- ANCHOR: parse
-- 🧪 Read the arguments the same way as the real command.
-- 本物のコマンドと同じように、引数を読む
parseArgs :: [String] -> Maybe Command
parseArgs args =
  getParseResult (execParserPure defaultPrefs commandInfo args)
-- ANCHOR_END: parse

main :: IO ()
main = hspec $ do
  -- 「文書を読む」
  describe "toDocument" $
    -- 「1行目が題、残りが本文」
    it "takes the first line as the title" $
      toDocument 3 "Slime\nSoft and blue.\n"
        `shouldBe` Document (DocId 3) "Slime" "Soft and blue.\n"

  -- 「抜粋」
  describe "snippet" $ do
    -- 「当たった単語を [ ] で囲む」
    it "marks the hit words" $
      snippet 2 (tokenize "fire") "Lava flows in the fire cave today"
        `shouldBe` "... in the [fire] cave today"

    -- 「長い本文は、両端を ... にする」
    it "cuts a long body with ..." $
      snippet 1 (tokenize "c") "a b c d e"
        `shouldBe` "... b [c] d ..."

  -- 「検索結果の表示」
  describe "formatResults" $ do
    let index = buildIndex sampleDocs
    -- 「順位、題、点数、抜粋を並べる」
    it "shows rank, title, score and snippet" $
      formatResults index "slime" (search index "slime")
        `shouldBe` T.unlines
          [ "1. Slime (0.32)"
          , "   A [slime] is soft and blue. It eats moss in the ..."
          ]

    -- 「何も当たらなければ、そう言う」
    it "says so when nothing matches" $
      formatResults index "unicorn" []
        `shouldBe` "No documents match \"unicorn\".\n"

  -- 「コマンドライン」
  describe "commandInfo" $ do
    -- 「検索の単語を1つにつなぐ」
    it "joins the search words" $
      parseArgs ["search", "-n", "3", "red", "dragon"]
        `shouldBe` Just
          (SearchCmd (SearchOptions "index.json" 3 "red dragon"))

    -- 「0 件の表示は断る」
    it "rejects a limit of 0" $
      parseArgs ["search", "-n", "0", "dragon"] `shouldBe` Nothing

    -- 「単語がなければ断る」
    it "needs at least one word" $
      parseArgs ["search"] `shouldBe` Nothing

  -- 「門番」
  describe "parseSearchQuery" $ do
    -- 「前後の空白を取り、トークンに切る」
    it "strips the text and cuts it into tokens" $
      fmap queryTokens (parseSearchQuery "  Red Dragon ")
        `shouldBe` Right [Token "red", Token "dragon"]

    -- 「ストップワードだけなら断る」
    it "rejects only stop words" $
      fmap queryText (parseSearchQuery "the of")
        `shouldBe` Left NoSearchableWords

  -- ANCHOR: wai
  -- 「Web API」
  with (app <$> newLive (pure sampleDocs)) $
    describe "GET /search" $ do
      -- 「当たった数と、件数までのヒットを返す」
      it "returns the total and up to limit hits" $
        get "/search?q=dragon%20fire&limit=1"
          `shouldRespondWith` 200
            { matchBody = bodyEquals (encode (SearchResponse
                "dragon fire" 3
                [HitView 6 "Dragon Tamer" 0.49302709040113113
                   ( "A [dragon] tamer speaks to a [dragon]"
                       <> " with a flute. A ..." )]))
            }

      -- 「q がなければ、JSON のメッセージで 400」
      it "answers 400 with a JSON message without q" $
        get "/search"
          `shouldRespondWith` [json|{"message": "the query q is empty"}|]
            { matchStatus = 400 }

      -- 「件数が多すぎれば 400」
      it "answers 400 for a too large limit" $
        get "/search?q=dragon&limit=51" `shouldRespondWith` 400

      -- 「長すぎる問い合わせは 400」
      it "answers 400 for a too long query" $
        get ("/search?q=" <> BS.replicate 201 97) `shouldRespondWith` 400
  -- ANCHOR_END: wai

  with (app <$> newLive (pure sampleDocs)) $
    describe "GET /docs/{id}" $
      -- 「ない番号なら 404」
      it "answers 404 for an unknown id" $
        get "/docs/99" `shouldRespondWith` 404

  -- ANCHOR: naive
  -- 「お手本（素朴な版）と比べる」
  describe "the fast version" $ do
    -- 「インデックスは、お手本と同じ」
    it "builds the same index as the naive one" $
      property $ forAll genDocs $ \docs ->
        buildIndex docs === buildIndexNaive docs

    -- 「検索の結果も、お手本と同じ（点数は誤差まで）」
    it "finds the same hits as the naive one" $
      property $ forAll genDocs $ \docs -> forAll genQuery $ \q ->
        let index = buildIndex docs
            fast  = search index q
            slow  = searchNaive index q
        in map hitDoc fast === map hitDoc slow
             .&&. and (zipWith close (map hitScore fast)
                                     (map hitScore slow))
  -- ANCHOR_END: naive

  -- 「バイナリーで保存して読み戻す」
  describe "saveIndex and loadIndex" $
    it "keeps the index the same in binary" $ do
      tmp <- getTemporaryDirectory
      let index = buildIndex sampleDocs
          path  = tmp </> "minisearch-test.bin"
      saveIndex path index
      loaded <- loadIndex path
      loaded `shouldBe` Right index

  -- ANCHOR: monoid
  -- 「インデックスを合わせる」
  describe "Index as a Monoid" $ do
    -- 「2つに分けて作って合わせても、同じインデックス」
    it "building two halves and merging is the same" $
      property $ forAll genDocs $ \docs -> forAll (chooseInt (0, 8)) $ \k ->
        let (xs, ys) = splitAt k docs
        in buildIndex xs <> buildIndex ys === buildIndex docs

    -- 「並列に作っても、同じインデックス」
    it "the parallel build is the same" $
      property $ forAll genDocs $ \docs -> forAll (chooseInt (1, 3)) $ \n ->
        buildIndexParallel n docs === buildIndex docs
  -- ANCHOR_END: monoid

  -- ANCHOR: live
  -- 「生きているインデックス」
  describe "Live" $
    -- 「作り直しのあいだも古いインデックスで検索でき、2つ目は断る」
    it "keeps the old index while rebuilding, and refuses a second" $ do
      docsRef <- newIORef (take 2 sampleDocs)
      -- A gate that holds the rebuild until we open it.
      -- 開けるまで、作り直しを止めておく門
      gate <- newTVarIO True
      let source = do
            atomically (readTVar gate >>= check)
            readIORef docsRef
      live <- newLive source
      writeIORef docsRef sampleDocs
      atomically (writeTVar gate False)
      first  <- startReindex live
      second <- startReindex live
      during <- documentCount <$> currentIndex live
      atomically (writeTVar gate True)
      waitIdle live
      done   <- documentCount <$> currentIndex live
      (first, second, during, done) `shouldBe` (True, False, 2, 6)
  -- ANCHOR_END: live

  with (app <$> newLive (pure sampleDocs)) $
    describe "POST /reindex" $
      -- 「作り直しを受け付けて 202」
      it "accepts a rebuild with 202" $
        post "/reindex" ""
          `shouldRespondWith` [json|{"message": "reindexing started"}|]
            { matchStatus = 202 }

  -- 「トレーニング」
  describe "answers" $ do
    -- ANCHOR: answer1
    -- 「合わせ方の順番によらない（結合法則）」（トレーニングのステップ1）
    it "merging is associative" $
      property $ forAll genDocs $ \docs ->
        let (a, rest) = splitAt 2 docs
            (b, c)    = splitAt 2 rest
            ia = buildIndex a
            ib = buildIndex b
            ic = buildIndex c
        in (ia <> ib) <> ic === ia <> (ib <> ic)

    -- 「空のインデックスは、何も変えない（単位元）」
    it "mempty changes nothing" $
      property $ forAll genDocs $ \docs ->
        let i = buildIndex docs
        in (mempty <> i === i) .&&. (i <> mempty === i)
    -- ANCHOR_END: answer1

    -- 「ファイルを同時に読んでも、同じ文書」（ステップ2）
    it "readDocumentsConcurrently reads the same documents" $ do
      a <- readDocuments "samples/docs"
      b <- readDocumentsConcurrently "samples/docs"
      b `shouldBe` a

    -- ANCHOR: answer3
    -- 「最後の作り直しが失敗した理由を覚えている」（ステップ3）
    it "remembers why the last rebuild failed" $ do
      status <- newTVarIO Nothing
      okRef  <- newIORef True
      let source = do
            ok <- readIORef okRef
            if ok
              then pure (take 2 sampleDocs)
              -- 「フォルダーがありません」
              else ioError (userError "the folder is gone")
      live <- newLive (reportingSource status source)
      writeIORef okRef False
      started <- startReindex live
      waitIdle live
      failed  <- readTVarIO status
      kept    <- documentCount <$> currentIndex live
      -- Once the folder is back, a rebuild works and clears the error.
      -- フォルダーが戻れば、作り直せて、エラーも消える
      writeIORef okRef True
      retried <- startReindex live
      waitIdle live
      cleared <- readTVarIO status
      (started, failed, kept, retried, cleared) `shouldBe`
        (True, Just "user error (the folder is gone)", 2, True, Nothing)
    -- ANCHOR_END: answer3

    -- 「保存して読み戻すと、同じインデックスになる」
    it "an index survives a round trip through JSON" $
      property $ forAll genDocs $ \docs ->
        let index = buildIndex docs
        in decode (encode index) === Just index
