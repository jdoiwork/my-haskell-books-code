-- ANCHOR: header
module Main (main) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import Review.Shuffle

main :: IO ()
main = hspec $ do
-- ANCHOR_END: header
  -- ANCHOR: spec
  -- どちらのシャッフルも、並べかえただけ（なくなったり増えたりしない）。
  -- 選び方は、それぞれの決まりどおりに作る
  it "both shuffles only reorder" $
    forAll (choose (1, 8)) $ \n ->
      forAll (vectorOf n (choose (0, n - 1))) $ \naive ->
        forAll (mapM (\i -> choose (0, i)) [n - 1, n - 2 .. 1]) $ \fy ->
          ( sort (naiveWith naive [1 .. n]) == [1 .. n]
          , sort (fisherYatesWith fy [1 .. n]) == [1 .. n] )
            == (True, True)
  -- フィッシャー－イェーツは、1〜6 枚のどれでも、どの並びもちょうど1通りずつ
  it "Fisher-Yates makes every order exactly once" $
    forM_ [1 .. 6] $ \n -> do
      let counts = exactCounts fisherYatesWith (fisherYatesChoices n) n
      (Map.size counts, all (== 1) counts) `shouldBe` (product [1 .. n], True)
  -- 素朴なシャッフルは、3〜6 枚のどれでも、どこかに偏りがある
  it "the naive shuffle is unfair for 3 to 6 items" $
    forM_ [3 .. 6] $ \n -> do
      let counts = exactCounts naiveWith (naiveChoices n) n
      minimum counts `shouldNotBe` maximum counts
  -- 6000 回試すと、どの並びも 1000 回くらい（乱数を進め忘れると、偏る）
  it "the simulation spreads over every order" $ do
    let Tally counts = simulate fisherYatesWith [2, 1] 3 6000 618
    Map.size counts `shouldBe` 6
    all (\c -> 800 < c && c < 1200) counts `shouldBe` True
  -- 試行が 0 回以下なら、空の集計
  it "no trials, no results" $
    ( simulate fisherYatesWith [2, 1] 3 0 618
    , simulate fisherYatesWith [2, 1] 3 (-1) 618 ) `shouldBe` (mempty, mempty)
  -- 2回分の集計を合わせると、件数は足し算になる
  it "tallies of separate batches add up" $
    let Tally merged = simulate fisherYatesWith [2, 1] 3 600 1
                       <> simulate fisherYatesWith [2, 1] 3 900 2
    in  sum merged `shouldBe` 1500
  -- ANCHOR_END: spec
