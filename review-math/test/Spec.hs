module Main (main) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import Review.Shuffle

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  -- どちらのシャッフルも、並べかえただけ（なくなったり増えたりしない）
  it "both shuffles only reorder" $
    forAll (choose (1, 6)) $ \n -> forAll (vectorOf n (choose (0, n - 1))) $
      \cs ->
        ( sort (naiveWith cs [1 .. n]) == [1 .. n]
        , sort (fisherYatesWith cs [1 .. n]) == [1 .. n] ) == (True, True)
  -- フィッシャー－イェーツは、どの並びも、ちょうど1通りずつ
  it "Fisher-Yates makes every order exactly once" $
    forAll (choose (1, 6)) $ \n ->
      let counts = exactCounts fisherYatesWith (fisherYatesChoices n) n
      in  Map.size counts == product [1 .. n] && all (== 1) counts
  -- 素朴なシャッフルは、3個以上だと、どこかに偏りがある
  it "the naive shuffle is unfair for 3 to 6 items" $
    forAll (choose (3, 6)) $ \n ->
      let counts = exactCounts naiveWith (naiveChoices n) n
      in  minimum counts /= maximum counts
  -- 2回分の集計を合わせると、件数は足し算になる
  it "tallies of separate batches add up" $
    let Tally merged = simulate fisherYatesWith [2, 1] 3 600 1
                       <> simulate fisherYatesWith [2, 1] 3 900 2
    in  sum merged `shouldBe` 1500
  -- ANCHOR_END: spec
