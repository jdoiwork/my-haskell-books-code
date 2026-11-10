-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Options.Applicative
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)
import Test.Hspec.Wai.Matcher (bodyEquals)
import Test.QuickCheck
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import MiniSearch.Answers
import MiniSearch.Api
import MiniSearch.Cli
import MiniSearch.Index
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
  with (pure (app (buildIndex sampleDocs))) $
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

  with (pure (app (buildIndex sampleDocs))) $
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

  -- ANCHOR: store
  -- 「保存する」
  describe "saveIndex" $
    -- 「書いている途中で失敗しても、前のインデックスが残る」
    it "keeps the old index if writing fails midway" $
      withSystemTempDirectory "minisearch" $ \dir -> do
        let path  = dir </> "index.json"
            index = buildIndex sampleDocs
        saveIndex path index
        -- An index that blows up while it is being written.
        -- 書いている途中で、爆発するインデックス
        saveIndex path (error "boom") `shouldThrow` errorCall "boom"
        loadIndex path `shouldReturn` Right index
        -- No half-written file is left behind.
        -- 書きかけのファイルも残らない
        listDirectory dir `shouldReturn` ["index.json"]
  -- ANCHOR_END: store

  -- 「バイナリーで保存して読み戻す」
  describe "saveIndex and loadIndex" $
    it "keeps the index the same in binary" $
      withSystemTempDirectory "minisearch" $ \tmp -> do
        let index = buildIndex sampleDocs
            path  = tmp </> "index.bin"
        saveIndex path index
        loaded <- loadIndex path
        loaded `shouldBe` Right index

  -- 「トレーニング」
  describe "answers" $ do
    -- ANCHOR: answer2
    -- 「topK は、全部並べてから k 件取るのと同じ」
    it "topK is the same as sorting all and taking k" $
      property $ forAll genDocs $ \docs -> forAll genQuery $ \q ->
        forAll (chooseInt (0, 5)) $ \k ->
          let sorted = search (buildIndex docs) q
          -- Give topK the hits in the wrong order on purpose.
          -- わざと逆の順番で、topK に渡す
          in topK k (reverse sorted) === take k sorted
    -- ANCHOR_END: answer2

    -- 「版の違うファイルは断る」
    it "loadVersioned refuses another version" $
      withSystemTempDirectory "minisearch" $ \tmp -> do
        let path = tmp </> "old.bin"
        Binary.encodeFile path (3 :: Int, buildIndex sampleDocs)
        loaded <- loadVersioned path
        loaded `shouldBe`
          Left ("this index is version 3, but this program"
                <> " reads version 4: run index again")

    -- ANCHOR: answer1
    -- 「件数 0 も 400」（トレーニングのステップ1）
    with (pure (app (buildIndex sampleDocs))) $
      it "answers 400 for a limit of 0" $
        get "/search?q=dragon&limit=0"
          `shouldRespondWith`
            [json|{"message": "limit must be 1 to 50: 0"}|]
            { matchStatus = 400 }
    -- ANCHOR_END: answer1

    -- 「保存して読み戻すと、同じインデックスになる」
    it "an index survives a round trip through JSON" $
      property $ forAll genDocs $ \docs ->
        let index = buildIndex docs
        in decode (encode index) === Just index
