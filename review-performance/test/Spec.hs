-- ANCHOR: all
-- 🧪 The fast answers must agree with the simple one.
-- 速い解は、素朴な解と同じ答えでなければならない
module Main (main) where

import Data.ByteString.Char8 qualified as BS
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Ledger

-- 🎲 Random records: a few heroes, small amounts of gold.
-- でたらめな記録：勇者は少し、金貨も少し
records :: Gen [Record]
records = listOf $ do
  name <- elements ["Lambda", "Python", "Rust", "Ruby"]
  amount <- choose (0, 100)
  pure (Record (BS.pack name) amount)

main :: IO ()
main = hspec $ do
  -- 「foldl' の合計は、素朴な合計と同じ」
  prop "totals agrees with the simple answer" $
    forAll records $ \rs -> totals rs === totalsNaive rs
  -- 「並列の合計も、素朴な合計と同じ」
  prop "totalsParallel agrees with the simple answer" $
    forAll records $ \rs ->
      forAll (choose (1, 8)) $ \parts ->
        totalsParallel parts rs === totalsNaive rs
  -- 「壊れた行は飛ばす」
  it "skips broken lines" $
    parseRecords (BS.pack "Lambda 10\nPython ten\nRust\nRuby 5\n")
      `shouldBe` [Record (BS.pack "Lambda") 10, Record (BS.pack "Ruby") 5]
-- ANCHOR_END: all
