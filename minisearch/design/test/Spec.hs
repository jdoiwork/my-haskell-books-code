-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.List (sort)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import MiniSearch.Answers
import MiniSearch.Index
import MiniSearch.Samples
import MiniSearch.Search
import MiniSearch.Tokenize
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: gen
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
-- ANCHOR_END: gen

-- ANCHOR: main
-- 🧪 Run all the tests: examples, properties, and the answers.
-- テストを全部動かす：例、性質、トレーニングの解答
main :: IO ()
main = hspec $ do
  exampleTests
  propertyTests
  answerTests
-- ANCHOR_END: main

-- ANCHOR: examples
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
-- ANCHOR_END: examples

-- ANCHOR: props
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
-- ANCHOR_END: props

-- ANCHOR: answer-tests
-- 🏋️ Tests for the training answers.
-- トレーニングの解答のテスト
answerTests :: Spec
answerTests =
  -- 「トレーニング」
  describe "training" $ do
    -- 「OR 検索は、どれか1つの単語を含む文書を返す」
    it "searchAny returns documents with any of the words" $
      searchAny sampleIndex "slime lava" `shouldBe` [DocId 2, DocId 4]

    -- 「よく出る単語」
    it "commonTokens counts documents" $
      commonTokens 2 sampleIndex
        `shouldBe` [(Token "fire", 4), (Token "dragon", 3)]
-- ANCHOR_END: answer-tests

    -- ANCHOR: answer3
    -- 「文書の題で検索すると、その文書が見つかる」
    it "searching a title finds its document" $
      property $ forAll genDocs $ \docs ->
        let index = buildIndex docs
            finds d = docId d `elem` map hitDoc (search index (docTitle d))
        in all finds docs
    -- ANCHOR_END: answer3
