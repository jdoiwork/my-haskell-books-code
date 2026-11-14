-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import MiniSearch.Index (buildIndex)
import MiniSearch.Samples (sampleDocs)
import MiniSearch.Types
import Review.Suggest
-- ANCHOR_END: head

-- ANCHOR: apiImports
-- For the Web API of step 5.
-- ステップ5の Web API のため
import Data.Aeson (encode)
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher (bodyEquals)

import MiniSearch.Live (newLive)
import MiniSearch.Tokenizer (english, japanese)
import Review.SuggestApi
-- ANCHOR_END: apiImports

-- ANCHOR: genWord

-- 🔤 Short words from a few letters, so that they are often close.
-- 少ない文字で作る短い単語（近いものが多くなるように）
genWord :: Gen T.Text
genWord = T.pack <$> resize 6 (listOf (elements "abcd"))
-- ANCHOR_END: genWord

-- ANCHOR: genTypo
-- ⌨️ A typo: a known word, broken by one or two changes.
-- 打ちまちがい：知っている単語を、1回か2回こわしたもの
genTypo :: [T.Text] -> Gen T.Text
genTypo known = do
  w <- elements known
  n <- chooseInt (1, 2)
  T.pack <$> breakTimes n (T.unpack w)
  where
    breakTimes :: Int -> String -> Gen String
    breakTimes 0 s = pure s
    breakTimes n s = breakOnce s >>= breakTimes (n - 1)
    breakOnce s = do
      i <- chooseInt (0, length s)
      c <- elements ['a' .. 'z']
      let (front, back) = splitAt i s
      elements
        [ front <> drop 1 back          -- remove / 消す
        , front <> [c] <> back          -- add / 足す
        , front <> [c] <> drop 1 back   -- replace / 置きかえる
        ]
-- ANCHOR_END: genTypo

-- ANCHOR: distance
main :: IO ()
main = hspec $ do
  -- 「編集距離」
  describe "editDistance" $ do
    -- 「よく知られた例」
    it "knows the classic examples" $
      map (uncurry editDistance)
        [("dargon", "dragon"), ("kitten", "sitting"), ("", "abc")]
        `shouldBe` [2, 3, 3]

    -- ANCHOR: props
    -- 「自分との距離は 0」
    it "is 0 to itself" $
      property $ forAll genWord $ \a -> editDistance a a === 0

    -- 「違う単語なら、0 より大きい」
    it "is more than 0 to another word" $
      property $ forAll genWord $ \a -> forAll genWord $ \b ->
        a /= b ==> editDistance a b > 0

    -- 「向きによらない」
    it "is the same both ways" $
      property $ forAll genWord $ \a -> forAll genWord $ \b ->
        editDistance a b === editDistance b a

    -- 「寄り道すると、近くはならない（三角不等式）」
    it "is never shorter through a third word" $
      property $ forAll genWord $ \a -> forAll genWord $ \b ->
        forAll genWord $ \c ->
          editDistance a c <= editDistance a b + editDistance b c
    -- ANCHOR_END: props
  -- ANCHOR_END: distance

  -- ANCHOR: suggestTests
  -- 「近い単語を勧める」
  describe "suggest" $ do
    let index = buildIndex sampleDocs
        known = [w | Token w <- Map.keys (indexPostings index)]
    -- 「打ちまちがいに、近い単語を勧める」
    it "suggests a close known word" $
      map (suggest index) ["dargon", "slme", "zzzzzz"]
        `shouldBe` [Just "dragon", Just "slime", Nothing]

    -- 「同じ近さなら、多くの文書に出てくるほう（calm より cave）」
    it "prefers the word in more documents" $
      suggest index "cale" `shouldBe` Just "cave"

    -- 「長さの確認は、答えを変えない」
    it "gives the same answer as the naive one" $
      property $ forAll (genTypo known) $ \w ->
        suggest index w === suggestNaive index w
  -- ANCHOR_END: suggestTests

  -- ANCHOR: answerTests
  -- 「もしかして」
  describe "answer" $ do
    let index = buildIndex sampleDocs
    -- 「何も当たらなければ、もしかして、と聞く」
    it "asks did-you-mean when nothing is found" $
      answer index "dargon fier" `shouldBe` DidYouMean "dragon fire"

    -- 「切り方は search と同じ（ストップワードと記号）」
    it "cuts the query the way search does" $
      map (answer index) ["the dargon", "dargon, fier", "the"]
        `shouldBe` [ DidYouMean "dragon"
                   , DidYouMean "dragon fire"
                   , NothingFound ]

    -- 「知っている単語は、そのまま」
    it "keeps the known words" $
      answer index "ice sheild" `shouldBe` DidYouMean "ice shield"

    -- 「直しても当たらなければ、何もない」
    it "says nothing found when the fixed query finds nothing" $
      answer index "slme dargon" `shouldBe` NothingFound

    -- 「直しようがなければ、何もない」
    it "says nothing found when it cannot help" $
      answer index "unicorn" `shouldBe` NothingFound

    -- 「当たれば、そのまま返す」
    it "returns the hits when there are some" $
      fmap length (case answer index "slime" of
                     Found hs -> Just hs
                     _        -> Nothing)
        `shouldBe` Just 1
  -- ANCHOR_END: answerTests

  -- ANCHOR: apiTests
  -- 「もしかしての付いた Web API」
  with (suggestApp <$> newLive english (pure sampleDocs)) $
    describe "GET /search with a suggestion" $ do
      -- 「何も当たらなければ、suggestion に直した問い合わせ」
      it "suggests a fixed query when nothing is found" $
        get "/search?q=dargon%20fier"
          `shouldRespondWith` 200
            { matchBody = bodyEquals (encode (SuggestResponse
                "dargon fier" 0 [] (Just "dragon fire")))
            }

      -- 「直しようがなければ、suggestion は null」
      it "has no suggestion when it cannot help" $
        get "/search?q=unicorn"
          `shouldRespondWith` 200
            { matchBody = bodyEquals (encode (SuggestResponse
                "unicorn" 0 [] Nothing))
            }

  -- 「日本語の設定では、勧めない」
  with (suggestApp <$> newLive japanese (pure sampleDocs)) $
    describe "GET /search in Japanese" $
      -- 「英語の切り方で直さない」
      it "does not suggest with another tokenizer" $
        get "/search?q=dargon"
          `shouldRespondWith` 200
            { matchBody = bodyEquals (encode (SuggestResponse
                "dargon" 0 [] Nothing))
            }
  -- ANCHOR_END: apiTests
-- ANCHOR_END: all
