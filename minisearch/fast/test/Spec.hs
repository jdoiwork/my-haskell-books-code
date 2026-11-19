-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import Data.List (sort)
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

-- 📝 Tests with examples, kept from the design chapter.
-- 例のテスト（設計の章から、そのまま残す）
exampleTests :: Spec
exampleTests = do
  -- 「トークンに切る」
  describe "tokenize" $
    -- 「小文字にして、記号とよくある単語を捨てる」
    it "lowers, and drops punctuation and stop words" $
      tokenize "The red Dragon, of fire!"
        `shouldBe` [Token "red", Token "dragon", Token "fire"]

  -- 「検索する」
  describe "search" $ do
    -- 「すべての単語を含む文書だけを返す」
    it "returns only documents with all the words" $
      map hitTitle (search sampleIndex "dragon fire")
        `shouldBe` ["Dragon Tamer", "Ice Dragon", "Red Dragon"]

    -- 「知らない単語なら、何も返さない」
    it "returns nothing for an unknown word" $
      search sampleIndex "unicorn" `shouldBe` []

    -- 「同じ単語を2回書いても、1回と同じ」
    it "a word twice counts as once" $
      search sampleIndex "dragon dragon"
        `shouldBe` search sampleIndex "dragon"

    -- 「同じ点数なら、番号の小さい順」
    it "ties go to the smaller number first" $
      let twins = [ Document (DocId 2) "Twin" "dragon"
                  , Document (DocId 1) "Twin" "dragon" ]
      in map hitDoc (search (buildIndex twins) "dragon")
           `shouldBe` [DocId 1, DocId 2]

  -- 「珍しい単語ほど、IDF が大きい」
  describe "inverseDocFrequency" $
    -- 「1つにしか出ない lava は、3つに出る dragon より大きい」
    it "is larger for a rarer word" $
      inverseDocFrequency (Token "lava") sampleIndex
        `shouldSatisfy` (> inverseDocFrequency (Token "dragon") sampleIndex)

-- ⚖️ Properties, kept from the design chapter.
-- 性質（設計の章から、そのまま残す）
propertyTests :: Spec
propertyTests =
  -- 「性質」
  describe "properties" $ do
    -- 「当たるのは、問い合わせの単語を全部含む文書で、そのすべて」
    it "hits are exactly the documents with all the query tokens" $
      property $ forAll genDocs $ \docs -> forAll genQuery $ \q ->
        let tokens   = tokenize q
            -- Read each document itself, not the index.
            -- インデックスではなく、文書そのものを読む
            wordsOf d = tokenize (docTitle d <> " " <> docBody d)
            expected = [ docId d | d <- docs
                                 , all (`elem` wordsOf d) tokens ]
            actual   = map hitDoc (search (buildIndex docs) q)
        in sort actual === sort expected

    -- 「結果は、点の高い順に並ぶ」
    it "hits are sorted by score, high first" $
      property $ forAll genDocs $ \docs -> forAll genQuery $ \q ->
        let scores = map hitScore (search (buildIndex docs) q)
        in and (zipWith (>=) scores (drop 1 scores))

    -- 「文書を入れる順番を変えても、同じインデックスになる」
    it "the order of documents does not matter" $
      property $ forAll genDocs $ \docs ->
        forAll (shuffle docs) $ \shuffled ->
          buildIndex docs === buildIndex shuffled


main :: IO ()
main = hspec $ do
  exampleTests
  propertyTests

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

    -- 「数字だけを数として読む（0x5 や、Int に入らない数は断る）」
    it "reads digits only, and refuses 0x5 and numbers too big for Int" $ do
      parseArgs ["search", "-n", "0x5", "dragon"] `shouldBe` Nothing
      parseArgs ["search", "-n", "18446744073709551617", "dragon"]
        `shouldBe` Nothing
      parseArgs ["search", "-n", " 7", "dragon"] `shouldBe` Nothing

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

    -- 「境目の両側：200 字と 201 字、10 語と 11 語、空白だけ」
    it "takes 200 characters and 10 words, not 201 or 11" $ do
      let manyWords k =
            T.unwords [T.pack ('w' : show i) | i <- [1 .. k :: Int]]
      fmap queryText (parseSearchQuery (T.replicate 200 "a"))
        `shouldBe` Right (T.replicate 200 "a")
      fmap queryText (parseSearchQuery (T.replicate 201 "a"))
        `shouldBe` Left (QueryTooLong 201)
      fmap (length . queryTokens) (parseSearchQuery (manyWords 10))
        `shouldBe` Right 10
      fmap queryText (parseSearchQuery (manyWords 11))
        `shouldBe` Left (TooManyWords 11)
      fmap queryText (parseSearchQuery "   ") `shouldBe` Left EmptyQuery

  -- 「件数の門番」
  describe "parseLimit" $
    -- 「指定なしは 10。1 と 50 は通し、0 と 51 は断る」
    it "is 10 by default, takes 1 and 50, not 0 or 51" $
      map (fmap limitValue . parseLimit)
        [Nothing, Just 1, Just 50, Just 0, Just 51]
        `shouldBe` [ Right 10, Right 1, Right 50
                   , Left (LimitOutOfRange 0), Left (LimitOutOfRange 51) ]

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
    describe "GET /docs/{id}" $ do
      -- 「ある番号なら、その文書」
      it "answers the document for a known id" $
        get "/docs/5"
          `shouldRespondWith` 200
            { matchBody = bodyEquals (encode (DocumentView 5 "Goblin Camp"
                ( "Goblins steal gold from travelers"
                    <> " and hide it in the forest." )))
            }

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

    -- 「1つずつ入れても（addDocument）、一度に作っても、同じ」
    it "adding one by one builds the same index" $
      property $ forAll genDocs $ \docs ->
        foldl' (flip addDocument) emptyIndex docs === buildIndex docs

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
        -- An old file whose index has another shape: still the version.
        -- 形のちがうインデックスの入った古いファイルでも、版で断る
        Binary.encodeFile path (3 :: Int, [1, 2, 3 :: Int])
        loadVersioned path `shouldReturn`
          Left ("this index is version 3, but this program"
                <> " reads version 4: run index again")
        -- A file of this version reads back.
        -- この版のファイルは、読み戻せる
        saveVersioned path (buildIndex sampleDocs)
        loadVersioned path `shouldReturn` Right (buildIndex sampleDocs)

    -- ANCHOR: answer1
    -- 「件数 0 も 400」（検索 Web API の章のトレーニングのステップ1）
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
