{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import MiniSearch.Index (buildIndex)
import MiniSearch.Samples (sampleDocs)
import Review.Suggest

-- 🔤 Short words from a few letters, so that they are often close.
-- 少ない文字で作る短い単語（近いものが多くなるように）
genWord :: Gen T.Text
genWord = T.pack <$> resize 6 (listOf (elements "abcd"))

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

  -- 「もしかして」
  describe "suggest and answer" $ do
    let index = buildIndex sampleDocs
    -- 「打ちまちがいに、近い単語を勧める」
    it "suggests a close known word" $
      map (suggest index) ["dargon", "slme", "zzzzzz"]
        `shouldBe` [Just "dragon", Just "slime", Nothing]

    -- 「何も当たらなければ、もしかして、と聞く」
    it "asks did-you-mean when nothing is found" $
      answer index "dargon fier" `shouldBe` DidYouMean "dragon fire"

    -- 「直しようがなければ、何もない」
    it "says nothing found when it cannot help" $
      answer index "unicorn" `shouldBe` NothingFound

    -- 「当たれば、そのまま返す」
    it "returns the hits when there are some" $
      fmap length (case answer index "slime" of
                     Found hs -> Just hs
                     _        -> Nothing)
        `shouldBe` Just 1
