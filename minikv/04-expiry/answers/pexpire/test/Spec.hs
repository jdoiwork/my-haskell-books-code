module Main (main) where

import Data.Maybe (fromMaybe)
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
      let (_, s) = applyAll 0 [Set "k" "v", PExpire "k" (ms 100)] emptyStore
      fst (apply 99 (Get "k") s)  `shouldBe` Bulk "v"
      fst (apply 100 (Get "k") s) `shouldBe` Nil
    -- run に足し忘れると、黙って nil と答える。答えは 1 のはず
    it "answers 1 for a key, and 0 for no key" $
      fst (applyAll 0 [ Set "k" "v", PExpire "k" (ms 100)
                      , PExpire "x" (ms 100) ] emptyStore)
        `shouldBe` [Ok, Number 1, Number 0]
    -- TTL は秒に切り上げ：1500 ミリ秒の残りは 2 秒
    it "is seen by TTL, rounded up to seconds" $
      fst (applyAll 0 [Set "k" "v", PExpire "k" (ms 1500), Ttl "k"]
                    emptyStore)
        `shouldBe` [Ok, Number 1, Number 2]
    -- 時刻があふれる期限は、時の果てで止まる。キーは読める
    it "stops a deadline that goes round at the end of time" $
      fst (applyAll 1 [Set "k" "v", PExpire "k" (ms maxBound), Get "k"]
                    emptyStore)
        `shouldBe` [Ok, Number 1, Bulk "v"]
    -- 0 以下のミリ秒は、門で断る
    it "refuses 0 or fewer milliseconds at the gate" $ do
      toCommand ["PEXPIRE", "k", "0"]
        `shouldBe` Left "invalid expire time: 0"
      toCommand ["PEXPIRE", "k", "-5"]
        `shouldBe` Left "invalid expire time: -5"
    -- toCommand に足し忘れると、文字列の場合分けなので警告が出ない
    it "is read from the wire, and written back" $ do
      toCommand ["PEXPIRE", "k", "100"]
        `shouldBe` Right (PExpire "k" (ms 100))
      commandWords (PExpire "k" (ms 100)) `shouldBe` ["PEXPIRE", "k", "100"]
  -- ANCHOR_END: pexpireSpec

-- ⏱️ Milliseconds for the tests (stop if the number is not fine).
-- テスト用のミリ秒（よくない数なら止める）
ms :: Int -> Milliseconds
ms n = fromMaybe (error ("not Milliseconds: " <> show n)) (milliseconds n)
