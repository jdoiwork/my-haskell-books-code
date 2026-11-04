-- ANCHOR: all
-- 🧪 Tests of the concurrent code, and dejafu trying every order.
-- 並行なコードのテストと、すべての順番を試す dejafu
module Main (main) where

import Control.Concurrent.STM
import Test.DejaFu (autocheck, dejafu, deadlocksNever)
import Test.Hspec

import ConcurrencyAnswers
import Locks
import Scouts
import Treasury

places :: [(String, Int)]
places = [("forest", 300), ("cave", 200), ("tower", 100)]

main :: IO ()
main = hspec $ do
  describe "scouts" $
    -- 「いっせいに送っても、報告は1人ずつのときと同じ」
    it "report the same, all at once" $ do
      together <- scoutsAtOnce places
      one      <- scoutsOneByOne places
      together `shouldBe` one

  describe "treasury" $ do
    -- 「不可分な更新なら、数えもれがない」
    it "counts every step with an atomic update" $
      countWithAtomic `shouldReturn` 1000000
    -- 「給料日のあとも、金貨の合計は変わらない」
    it "keeps all the gold on pay day" $
      payDay `shouldReturn` (0, 1000)

  -- ANCHOR: dejafu
  describe "locks (dejafu)" $ do
    -- 「錠をかける順番がばらばらだと、デッドロックが見つかる」
    it "finds the deadlock in swapGifts" $
      dejafu "never deadlocks" deadlocksNever swapGifts
        `shouldReturn` False
    -- 「順番をそろえれば、どの順番でも、いつも同じ結果」
    it "finds no problem in swapGiftsSafe" $
      autocheck swapGiftsSafe `shouldReturn` True
  -- ANCHOR_END: dejafu

  -- answers for the training / トレーニングの解答
  describe "answers" $ do
    -- 「先に戻った偵察の報告を受け取る」
    it "takes the fastest scout" $
      fastestScout ("forest", 300) ("tower", 100)
        `shouldReturn` "tower: 1 goblins"
    -- 「足りなければ、移さずに False」
    it "refuses a transfer without enough gold" $ do
      result <- atomically $ do
        poor <- newTVar 5
        rich <- newTVar 0
        ok   <- tryTransfer poor rich 10
        left <- readTVar poor
        pure (ok, left)
      result `shouldBe` (False, 5)
-- ANCHOR_END: all
