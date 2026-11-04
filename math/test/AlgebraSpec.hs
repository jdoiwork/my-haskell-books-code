module AlgebraSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import AlgebraAnswers
import Math.Algebra

-- 🎲 Random summaries, made from random response times.
-- でたらめな応答時間から作る、でたらめなまとめ
genSummary :: Gen Summary
genSummary = foldMap summarize <$> listOf time
  where
    time = fromIntegral <$> choose (1, 999 :: Int)

genTimes :: Gen [Double]
genTimes = listOf1 (fromIntegral <$> choose (1, 999 :: Int))

spec :: Spec
spec = do
  -- ANCHOR: spec
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
  -- 訪れた町は、どの順で、何度合わせても同じ
  it "visits merge in any order, any number of times" $
    forAll (listOf (elements ["Start", "River", "Castle"])) $ \towns ->
      let a = foldMap visit towns
          b = foldMap visit (reverse towns)
      in  (a <> b, a <> a) == (b <> a, a)
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
