module VectorsSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (scale)

import Math.Vectors
import VectorsAnswers

-- ANCHOR: gens
-- 🎲 Small whole numbers, so the products are exact.
-- 小さな整数（積がずれないように）
genMatrix :: Gen (Matrix Integer)
genMatrix = vectorOf 2 (vectorOf 2 (choose (-9, 9)))

-- 🎲 A small vector, the same way.
-- 小さなベクトルも、同じように
genVector :: Gen (Vector Integer)
genVector = vectorOf 2 (choose (-9, 9))
-- ANCHOR_END: gens

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 行列を順に当てるのは、先に掛けた行列を当てるのと同じ
  it "(A B) v == A (B v)" $
    forAll genMatrix $ \a -> forAll genMatrix $ \b -> forAll genVector $ \v ->
      apply (mmul a b) v == apply a (apply b v)
  -- 行列の積は、組み方を変えてもよい（結合法則）
  it "(A B) C == A (B C)" $
    forAll genMatrix $ \a -> forAll genMatrix $ \b -> forAll genMatrix $ \c ->
      mmul (mmul a b) c == mmul a (mmul b c)
  -- でも、順番を変えると、答えが変わることがある
  it "A B is not always B A" $
    (mmul rotate90 stretchX :: Matrix Int)
      `shouldNotBe` mmul stretchX rotate90
  -- 回しても、長さは変わらない
  it "rotating keeps the length" $
    forAll (vectorOf 2 (choose (-100, 100))) $ \v ->
      abs (norm (apply rotate90 v) - norm v) < 1e-9
  -- 2倍に伸ばしても、向き（コサイン）は同じ
  it "cosine ignores the length" $
    forAll (vectorOf 4 (choose (1, 100))) $ \v ->
      abs (cosine v (scale 2 v) - 1) < 1e-9
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "the distance from (0,0) to (3,4) is 5" $
      distance [0, 0] [3, 4] `shouldBe` 5
    it "fibByMatrix agrees with the list of Fibonacci numbers" $
      map fibByMatrix [0 .. 30] `shouldBe` take 31 fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs) :: [Integer]
