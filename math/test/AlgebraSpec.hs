module AlgebraSpec (spec) where

-- ANCHOR: imports
import Control.Exception (evaluate)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Test.Hspec
import Test.QuickCheck

import AlgebraAnswers
import Math.Algebra
-- ANCHOR_END: imports

-- ANCHOR: gens
-- 🎲 Random summaries, made from random response times.
-- でたらめな応答時間から作る、でたらめなまとめ
genSummary :: Gen Summary
genSummary = foldMap summarize <$> listOf (choose (1, 999))

-- 🎲 Random response times, at least one.
-- でたらめな応答時間（1つ以上）
genTimes :: Gen [Int]
genTimes = listOf1 (choose (1, 999))

-- 🎲 Random visits to some of the towns.
-- いくつかの町への、でたらめな訪問
genVisits :: Gen Visits
genVisits =
  foldMap visit <$> listOf (elements ["Start", "River", "Castle", "Forest"])
-- ANCHOR_END: gens

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 本文の5件：個数 5、合計 475、最速 10、最遅 400、平均 95
  it "summarizes the five response times" $ do
    let s = foldMap summarize [10, 20, 30, 400, 15]
    s `shouldBe` Summary 5 475 10 400
    meanOf s `shouldBe` Just 95
  -- 何もないもののまとめには、平均がない
  it "the summary of nothing has no mean" $
    meanOf mempty `shouldBe` Nothing
  -- まとめを合わせる <> は、組み方を変えても同じ（結合法則）
  it "combining summaries is associative" $
    forAll genSummary $ \a -> forAll genSummary $ \b ->
      forAll genSummary $ \c -> (a <> b) <> c == a <> (b <> c)
  -- mempty は、合わせても何も変えない（単位元）
  it "mempty changes nothing" $
    forAll genSummary $ \a -> (a <> mempty, mempty <> a) == (a, a)
  -- どう切ってまとめても、全部を一度にまとめたのと同じ
  it "summarizing in chunks is the same as all at once" $
    forAll genTimes $ \times -> forAll (choose (1, 10)) $ \n ->
      summarizeInChunks n times == foldMap summarize times
  -- 0 個ずつには切れない
  it "chunksOf stops on a width of 0" $
    evaluate (length (chunksOf 0 [10 :: Int])) `shouldThrow` anyErrorCall
  -- 訪れた町は、どの順で、何度合わせても同じ
  it "visits merge in any order, any number of times" $
    forAll genVisits $ \a -> forAll genVisits $ \b ->
      (a <> b, a <> a) == (b <> a, a)
  -- 別々の記録を合わせると、どちらの町も残る
  it "merging visits keeps every town" $
    (visit "River" <> visit "Start") <> visit "Castle"
      `shouldBe` foldMap visit ["Castle", "River", "Start"]
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "word counts of two files add up" $
      tally "fire ice" <> tally "fire" `shouldBe`
        Tally (Map.fromList [("fire", 2), ("ice", 1)])
    it "Top3 in chunks is the same as all at once" $
      forAll (listOf (choose (0, 100))) $ \scores ->
        forAll (choose (1, 5)) $ \n ->
          mconcat [ foldMap top3 piece | piece <- chunksOf n scores ]
            == foldMap top3 scores
    -- 上位3つは、大きい順に並べた先頭の3つ（同じ点も数える）
    it "Top3 keeps tied scores" $
      getTop3 (foldMap top3 [3, 9, 4, 9]) `shouldBe` [9, 9, 4]
    it "Top3 is the three best scores" $
      forAll (listOf (choose (0, 100))) $ \scores ->
        getTop3 (foldMap top3 scores)
          == take 3 (sortBy (comparing Down) scores)
