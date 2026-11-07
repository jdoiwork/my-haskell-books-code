module DivideSpec (spec) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Test.Hspec
import Test.QuickCheck

import Algo.Divide
import Algo.Thinking (bestRun0)
import AlgoAnswers (msortBy)

spec :: Spec
spec = describe "Algo.Divide" $ do
  -- 標準の sort と同じ
  it "msort agrees with sort" $ property $ \xs ->
    msort xs === sort (xs :: [Int])
  it "qsort agrees with sort" $ property $ \xs ->
    qsort xs === sort (xs :: [Int])
  -- 速い版も、仕様と同じ数を数える
  it "inversions agrees with the spec" $ property $ \xs ->
    inversions xs === inversionsSpec (xs :: [Int])
  -- ANCHOR: monoidProps
  -- Monoid でまとめても、第6部の最初の章の仕様と同じ答え
  it "bestRunDC agrees with the spec" $ property $ \xs ->
    bestRunDC xs === bestRun0 xs
  -- どこで分けても、まとめた答えは同じ（結合法則）
  it "Run is associative" $ property $ \a b c ->
    let r :: [Int] -> Run
        r = foldMap room
    in  (r a <> r b) <> r c === r a <> (r b <> r c)
  -- ANCHOR_END: monoidProps
  -- トレーニングの解答：標準の sortBy と同じ（どちらも安定）
  it "msortBy agrees with sortBy" $ property $ \xs ->
    msortBy (comparing fst) xs
      === sortBy (comparing fst) (xs :: [(Int, Char)])
