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
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import MiniSearch.Answers
import MiniSearch.Api
import MiniSearch.Bigram
import MiniSearch.Config
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
import MiniSearch.Tokenizer
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

-- ANCHOR: archhelpers
-- 🏛️ The pure core: these modules must not touch the outside world.
-- 純粋な核：このモジュールたちは、外の世界に触れてはいけない
coreModules :: [FilePath]
coreModules =
  map (\m -> "src/MiniSearch/" <> m <> ".hs")
    [ "Types", "Tokenize", "Bigram", "Tokenizer", "Index", "Search"
    , "Snippet", "Query", "Report", "Config" ]

-- 🚫 Modules that touch files, threads, or the network.
-- ファイル、スレッド、ネットワークに触れるモジュール
outsideModules :: [String]
outsideModules =
  [ "System.", "Control.Concurrent", "Network.", "Data.IORef"
  , "MiniSearch.Load", "MiniSearch.Store", "MiniSearch.Live"
  , "MiniSearch.Api" ]

-- 🔎 The import lines of a file that bring in an outside module.
-- ファイルの import の行のうち、外の世界のモジュールを持ちこむもの
importsOutside :: FilePath -> IO [String]
importsOutside = importsAnyOf outsideModules

importsAnyOf :: [String] -> FilePath -> IO [String]
importsAnyOf forbidden path = do
  source <- TIO.readFile path
  let imported l = case words (T.unpack l) of
        ("import" : "qualified" : m : _) -> Just m
        ("import" : m : _)               -> Just m
        _                                -> Nothing
      bad = [ path <> ": " <> m
            | Just m <- map imported (T.lines source)
            , any (`isPrefixOf` m) forbidden ]
  pure bad
-- ANCHOR_END: archhelpers

-- ANCHOR: answer3
-- 🐛 The answer of step 3: Debug.Trace is not allowed in the core either.
-- ステップ3の解答：Debug.Trace も、核では使わない
outsideModulesWithTrace :: [String]
outsideModulesWithTrace =
  [ "System.", "Control.Concurrent", "Network.", "Data.IORef"
  , "Debug.Trace"
  , "MiniSearch.Load", "MiniSearch.Store", "MiniSearch.Live"
  , "MiniSearch.Api" ]
-- ANCHOR_END: answer3

-- ANCHOR: name
-- 🏷️ The name of the tokenizer in a config.
-- 設定の中のトークナイザーの名前
name :: Config -> T.Text
name = tokenizerName . configTokenizer
-- ANCHOR_END: name

main :: IO ()
main = hspec $ do
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
      formatResults english index "slime" (search index "slime")
        `shouldBe` T.unlines
          [ "1. Slime (0.32)"
          , "   A [slime] is soft and blue. It eats moss in the ..."
          ]

    -- 「何も当たらなければ、そう言う」
    it "says so when nothing matches" $
      formatResults english index "unicorn" []
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
      fmap queryTokens (parseSearchQuery english "  Red Dragon ")
        `shouldBe` Right [Token "red", Token "dragon"]

    -- 「ストップワードだけなら断る」
    it "rejects only stop words" $
      fmap queryText (parseSearchQuery english "the of")
        `shouldBe` Left NoSearchableWords

  -- ANCHOR: wai
  -- 「Web API」
  with (app <$> newLive english (pure sampleDocs)) $
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

  with (app <$> newLive english (pure sampleDocs)) $
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
        saveIndex path english index
        -- An index that blows up while it is being written.
        -- 書いている途中で、爆発するインデックス
        saveIndex path english (error "boom") `shouldThrow` errorCall "boom"
        fmap snd <$> loadIndex path `shouldReturn` Right index
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
        saveIndex path english index
        loaded <- loadIndex path
        fmap snd loaded `shouldBe` Right index

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
        live  <- newLive english (readDocuments dir)
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
      live <- newLive english source
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

  with (app <$> newLive english (pure sampleDocs)) $
    describe "POST /reindex" $
      -- 「作り直しを受け付けて 202」
      it "accepts a rebuild with 202" $
        post "/reindex" ""
          `shouldRespondWith` [json|{"message": "reindexing started"}|]
            { matchStatus = 202 }

  -- ANCHOR: japanese
  -- 「日本語」
  describe "Japanese" $ do
    -- ANCHOR: bigramtest
    -- 「2文字ずつに切る。英数字は単語のまま」
    it "cuts Japanese into bigrams, and keeps words" $
      bigramTokenize "HP 30のスライム"
        `shouldBe` map Token ["hp", "30", "のス", "スラ", "ライ", "イム"]
    -- ANCHOR_END: bigramtest

    -- 「日本語の文書を検索できる」
    it "searches Japanese documents" $ do
      docs <- readDocuments "samples/docs-ja"
      let index = buildIndexWith japanese docs
      map hitTitle (searchWith japanese index "ドラゴン 金貨")
        `shouldBe` ["レッドドラゴン"]

    -- 「保存したインデックスは、トークナイザーを覚えている」
    it "a saved index remembers its tokenizer" $
      withSystemTempDirectory "minisearch" $ \tmp -> do
        docs <- readDocuments "samples/docs-ja"
        let path = tmp </> "index-ja.bin"
        saveIndex path japanese (buildIndexWith japanese docs)
        loaded <- loadIndex path
        fmap (tokenizerName . fst) loaded `shouldBe` Right "ja"
  -- ANCHOR_END: japanese

  -- ANCHOR: config
  -- 「設定」
  describe "loadConfig" $ do
    -- 「何もなければ、決まった値」
    it "uses the defaults" $
      fmap (\c -> (configDocs c, configPort c, name c)) (loadConfig [])
        `shouldBe` Right ("samples/docs", 8080, "en")

    -- 「まちがった値は、起動のときに断る」
    it "refuses a bad port and a bad language" $ do
      fmap name (loadConfig [("PORT", "http")])
        `shouldBe` Left "PORT must be 1 to 65535: http"
      fmap name (loadConfig [("PORT", "0x1F90")])
        `shouldBe` Left "PORT must be 1 to 65535: 0x1F90"
      fmap name (loadConfig [("MINISEARCH_LANG", "fr")])
        `shouldBe` Left "MINISEARCH_LANG must be en or ja: fr"
  -- ANCHOR_END: config

  -- ANCHOR: arch
  -- 「設計の決まり」
  describe "architecture" $
    -- 「純粋な核は、外の世界に触れるモジュールを import しない」
    it "the pure core imports nothing that touches the outside" $ do
      problems <- concat <$> mapM importsOutside coreModules
      problems `shouldBe` []
  -- ANCHOR_END: arch

  -- 「トレーニング」
  describe "answers (architecture)" $ do
    -- 「1文字の問い合わせも当たる」（ステップ1）
    it "japaneseWithUnigrams finds a one-character query" $ do
      docs <- readDocuments "samples/docs-ja"
      let plain = buildIndexWith japanese docs
          both  = buildIndexWith japaneseWithUnigrams docs
      ( length (searchWith japanese plain "炎")
        , length (searchWith japaneseWithUnigrams both "炎") )
        `shouldBe` (0, 4)

    -- 「Debug.Trace も核に入っていない」（ステップ3）
    it "the core does not use Debug.Trace" $ do
      problems <- concat <$> mapM (importsAnyOf outsideModulesWithTrace)
                                  coreModules
      problems `shouldBe` []

    -- 「複数形も同じ単語に」（ステップ2）
    it "englishPlural treats dragons as dragon" $
      cut englishPlural "Two dragons and a dress"
        `shouldBe` [Token "two", Token "dragon", Token "dress"]

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

    -- 「保存して読み戻すと、同じインデックスになる」
    it "an index survives a round trip through JSON" $
      property $ forAll genDocs $ \docs ->
        let index = buildIndex docs
        in decode (encode index) === Just index
