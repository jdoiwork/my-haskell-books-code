-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text as T
import Options.Applicative
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON (json)
import Test.Hspec.Wai.Matcher (bodyEquals)
import Test.QuickCheck
import Control.Concurrent.STM
import Data.IORef
import qualified Data.Map as Map
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

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
      pure (Document (DocId (T.pack (show i))) (T.pack (show i))
                     (T.unwords ws))
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
      let twins = [ Document (DocId "2") "Twin" "dragon"
                  , Document (DocId "1") "Twin" "dragon" ]
      in map hitDoc (search (buildIndex twins) "dragon")
           `shouldBe` [DocId "1", DocId "2"]

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
      toDocument (DocId "abc") "Slime\nSoft and blue.\n"
        `shouldBe` Document (DocId "abc") "Slime" "Soft and blue.\n"

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
      parseArgs ["search", "-n", "18446744073709551617", "dragon"] `shouldBe` Nothing
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
                [HitView "6" "Dragon Tamer" 0.49302709040113113
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

  -- ANCHOR: ids
  -- 「文書の番号」
  describe "document ids" $ do
    -- 「名前と中身の SHA-256 の頭12文字（shasum -a 256 で求めた値）」
    it "is the first 12 hex digits of SHA-256 of the name and bytes" $
      fileDocId "slime.txt" "Slime\nSoft and blue.\n"
        `shouldBe` DocId "a0d3e1943e72"

    -- 「中身が同じでも、名前がちがえば別の番号」
    it "gives different ids to different names with the same bytes" $ do
      fileDocId "a.txt" "Slime" `shouldNotBe` fileDocId "b.txt" "Slime"
      -- The \0 keeps the name and the bytes apart.
      -- \0 が、名前と中身の境目を守る
      fileDocId "ab" "c" `shouldNotBe` fileDocId "a" "bc"

    -- 「ほかのファイルが増えても、同じファイルは同じ番号のまま」
    it "keeps the id of a file when another file is added" $
      withSystemTempDirectory "minisearch" $ \dir -> do
        writeFile (dir </> "slime.txt") "Slime\nSoft and blue.\n"
        older <- readDocuments dir
        -- A new file that comes first in name order.
        -- 名前の順で先頭に来る、新しいファイル
        writeFile (dir </> "elf-village.txt") "Elf Village\nTall trees.\n"
        newer <- readDocuments dir
        let idOf title docs = [ docId d | d <- docs, docTitle d == title ]
        length older `shouldBe` 1
        idOf "Slime" newer `shouldBe` idOf "Slime" older

    -- 「中身を変えて作り直すと、古い番号では見つからない」
    it "forgets the old id when the contents change" $
      withSystemTempDirectory "minisearch" $ \dir -> do
        let path = dir </> "slime.txt"
        writeFile path "Slime\nSoft and blue.\n"
        live  <- newLive (readDocuments dir)
        [old] <- Map.keys . indexDocs <$> currentIndex live
        writeFile path "Slime\nSoft and red.\n"
        started <- startReindex live
        waitIdle live
        docs <- indexDocs <$> currentIndex live
        started `shouldBe` True
        -- The new contents are there, but not under the old id.
        -- 新しい中身はある。でも、古い番号では引けない
        map docBody (Map.elems docs) `shouldBe` ["Soft and red.\n"]
        Map.lookup old docs `shouldBe` Nothing
  -- ANCHOR_END: ids

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
