module Main (main) where

import Test.Hspec

import MiniKv.Protocol
import MiniKv.Store
import MiniKv.Types

main :: IO ()
main = hspec $ do
  -- ANCHOR: pexpireSpec
  -- 「ミリ秒の期限」
  describe "PEXPIRE" $ do
    -- 100 ミリ秒の期限：99 ミリ秒ではまだ読め、100 ミリ秒で忘れる
    it "forgets the key after n milliseconds" $ do
      let (_, s) = applyAll 0 [Set "k" "v", PExpire "k" 100] emptyStore
      fst (apply 99 (Get "k") s)  `shouldBe` Bulk "v"
      fst (apply 100 (Get "k") s) `shouldBe` Nil
    -- run に足し忘れると、黙って nil と答える。答えは 1 のはず
    it "answers 1 for a key, and 0 for no key" $
      fst (applyAll 0 [Set "k" "v", PExpire "k" 100, PExpire "x" 100]
                    emptyStore)
        `shouldBe` [Ok, Number 1, Number 0]
    -- TTL は秒に切り上げ：1500 ミリ秒の残りは 2 秒
    it "is seen by TTL, rounded up to seconds" $
      fst (applyAll 0 [Set "k" "v", PExpire "k" 1500, Ttl "k"] emptyStore)
        `shouldBe` [Ok, Number 1, Number 2]
    -- 時刻があふれる期限は断り、キーは残す
    it "refuses a deadline that goes round" $
      fst (applyAll 1 [Set "k" "v", PExpire "k" maxBound, Get "k"]
                    emptyStore)
        `shouldBe` [Ok, Failed (BadExpire "k"), Bulk "v"]
    -- toCommand に足し忘れると、文字列の場合分けなので警告が出ない
    it "is read from the wire, and written back" $ do
      toCommand ["PEXPIRE", "k", "100"] `shouldBe` Right (PExpire "k" 100)
      commandWords (PExpire "k" 100) `shouldBe` ["PEXPIRE", "k", "100"]
  -- ANCHOR_END: pexpireSpec
