{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import MiniSearch.Answers
import MiniSearch.Index
import MiniSearch.Samples
import MiniSearch.Search
import MiniSearch.Tokenize
import MiniSearch.Types

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

main :: IO ()
main = hspec $ do
  -- 「トークンに切る」
  describe "tokenize" $
    -- 「小文字にして、記号とよくある単語を捨てる」
    it "lowers, and drops punctuation and stop words" $
      tokenize "The red Dragon, of fire!"
        `shouldBe` [Token "red", Token "dragon", Token "fire"]

  -- 「検索する」
  describe "search" $ do
    let index = buildIndex sampleDocs
    -- 「すべての単語を含む文書だけを返す」
    it "returns only documents with all the words" $
      map hitTitle (search index "dragon fire")
        `shouldBe` ["Dragon Tamer", "Ice Dragon", "Red Dragon"]

    -- 「知らない単語なら、何も返さない」
    it "returns nothing for an unknown word" $
      search index "unicorn" `shouldBe` []

    -- 「OR 検索は、どれか1つの単語を含む文書を返す」（トレーニング）
    it "searchAny returns documents with any of the words" $
      searchAny index "slime lava" `shouldBe` [DocId 2, DocId 4]

    -- 「よく出る単語」（トレーニング）
    it "commonTokens counts documents" $
      commonTokens 2 index
        `shouldBe` [(Token "fire", 4), (Token "dragon", 3)]

  -- ANCHOR: props
  -- 「性質」
  describe "properties" $ do
    -- 「当たった文書は、どれも問い合わせの単語を全部含む」
    it "every hit has all the query tokens" $
      property $ forAll genDocs $ \docs -> forAll genQuery $ \q ->
        let index  = buildIndex docs
            tokens = tokenize q
            hasAll h = all (\t -> Map.member (hitDoc h) (postingsOf t index))
                           tokens
        in all hasAll (search index q)

    -- 「結果は、点の高い順に並ぶ」
    it "hits are sorted by score, high first" $
      property $ forAll genDocs $ \docs -> forAll genQuery $ \q ->
        let scores = map hitScore (search (buildIndex docs) q)
        in and (zipWith (>=) scores (drop 1 scores))

    -- 「文書を入れる順番を変えても、同じインデックスになる」
    it "the order of documents does not matter" $
      property $ forAll genDocs $ \docs ->
        buildIndex docs === buildIndex (sortOn (negate . idOf) docs)
  -- ANCHOR_END: props

    -- ANCHOR: answer3
    -- 「文書の題で検索すると、その文書が見つかる」
    it "searching a title finds its document" $
      property $ forAll genDocs $ \docs ->
        let index = buildIndex docs
            finds d = docId d `elem` map hitDoc (search index (docTitle d))
        in all finds docs
    -- ANCHOR_END: answer3
  where
    idOf d = let DocId i = docId d in i
