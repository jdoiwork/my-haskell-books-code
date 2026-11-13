-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Options.Applicative
import Test.Hspec
import Test.QuickCheck
import System.Directory (listDirectory)
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
      parseArgs ["search", "-n", "18446744073709551617", "dragon"] `shouldBe` Nothing
      parseArgs ["search", "-n", " 7", "dragon"] `shouldBe` Nothing

    -- 「単語がなければ断る」
    it "needs at least one word" $
      parseArgs ["search"] `shouldBe` Nothing

  -- 「トレーニング」
  describe "answers" $ do
    -- 「インデックスの大きさ」
    it "counts the size of an index" $
      indexStats (buildIndex sampleDocs) `shouldBe` Stats 6 46 67

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
