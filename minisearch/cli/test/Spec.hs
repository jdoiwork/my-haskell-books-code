-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Text as T
import Options.Applicative
import Test.Hspec
import Test.QuickCheck
import System.Directory (createDirectory, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import MiniSearch.Answers
import MiniSearch.Cli
import MiniSearch.Index
import MiniSearch.Load
import MiniSearch.Report
import MiniSearch.Samples
import MiniSearch.Search
import MiniSearch.Snippet
import MiniSearch.Store
import MiniSearch.Tokenize
import MiniSearch.Types
-- ANCHOR_END: head

-- 🎲 A random small library: words come from a tiny vocabulary,
--    so searches often hit something.
-- でたらめな小さい書庫：単語は小さな語彙から選ぶので、
-- 検索がよく何かに当たる
vocabulary :: [T.Text]
vocabulary = ["dragon", "fire", "ice", "gold", "slime", "cave"]

genDocs :: Gen [Document]
genDocs = do
  n <- chooseInt (0, 8)
  mapM genDoc [1 .. n]
  where
    genDoc i = do
      ws <- listOf (elements vocabulary)
      pure (Document (DocId i) (T.pack (show i)) (T.unwords ws))

genQuery :: Gen T.Text
genQuery = T.unwords <$> listOf1 (elements vocabulary)


-- ANCHOR: main
-- 🧪 The tests of the last chapter, then the new ones.
-- 前の章のテストに、この章のテストを足す
main :: IO ()
main = hspec $ do
  exampleTests
  propertyTests
  cliTests
  answerTests
-- ANCHOR_END: main

-- 📝 Tests with examples: this input gives this output.
-- 例のテスト：この入力なら、この出力
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

-- ⚖️ Properties: true for any library and any query.
-- 性質：どんな書庫、どんな問い合わせでも成り立つ
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

-- ANCHOR: parse
-- 🧪 Read the arguments the same way as the real command.
-- 本物のコマンドと同じように、引数を読む
parseArgs :: [String] -> Maybe Command
parseArgs args =
  getParseResult (execParserPure defaultPrefs commandInfo args)
-- ANCHOR_END: parse

-- 🖥️ Tests for this chapter: reading, showing, saving, the command line.
-- この章のテスト：読む、見せる、保存する、コマンドライン
cliTests :: Spec
cliTests = do
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
    -- 「順位、題、点数、抜粋を並べる」
    it "shows rank, title, score and snippet" $
      formatResults sampleIndex "slime" (search sampleIndex "slime")
        `shouldBe` T.unlines
          [ "1. Slime (0.32)"
          , "   A [slime] is soft and blue. It eats moss in the ..."
          ]

    -- 「何も当たらなければ、そう言う」
    it "says so when nothing matches" $
      formatResults sampleIndex "unicorn" []
        `shouldBe` "No documents match \"unicorn\".\n"

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

-- 🏋️ Tests for the training answers.
-- トレーニングの解答のテスト
answerTests :: Spec
answerTests = do
  -- 「トレーニング」
  describe "answers" $ do
    -- 「インデックスの大きさ」
    it "counts the size of an index" $
      indexStats sampleIndex `shouldBe` Stats 6 46 67

    -- 「上限のある数」
    it "smallPositiveInt has an upper limit" $ do
      let readN s = getParseResult (execParserPure defaultPrefs
                      (info (option (smallPositiveInt 50) (long "n")) mempty)
                      ["--n", s])
      map readN ["1", "50", "51", "0", "0x10", "18446744073709551617"]
        `shouldBe` [Just 1, Just 50, Nothing, Nothing, Nothing, Nothing]

    -- ANCHOR: answer3
    -- 「保存して読み戻すと、同じインデックスになる」
    it "an index survives a round trip through JSON" $
      property $ forAll genDocs $ \docs ->
        let index = buildIndex docs
        in decode (encode index) === Just index
    -- ANCHOR_END: answer3

    -- ANCHOR: answer4
    -- 「index の失敗は、落ちずに Left になる」
    it "readDocumentsSafe says what went wrong" $
      withSystemTempDirectory "minisearch" $ \dir -> do
        r1 <- readDocumentsSafe (dir </> "nodir")
        r1 `shouldSatisfy` isLeft
        -- A folder named sub.txt is skipped.
        -- sub.txt という名前のフォルダーは飛ばす
        createDirectory (dir </> "sub.txt")
        writeFile (dir </> "slime.txt") "Slime\nSoft and blue.\n"
        r2 <- readDocumentsSafe dir
        fmap (map docTitle) r2 `shouldBe` Right ["Slime"]
        -- "caf\xe9" in Latin-1 is not UTF-8.
        -- Latin-1 の「café」は、UTF-8 ではない
        BS.writeFile (dir </> "cafe.txt") (BS.pack [99, 97, 102, 233])
        readDocumentsSafe dir
          `shouldReturn` Left (dir </> "cafe.txt" <> ": not UTF-8")
    -- ANCHOR_END: answer4
