{-# OPTIONS_GHC -Wno-orphans #-}
module CrdtSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

import Modern.Crdt
import ModernAnswers (PNCounter, decrement, pnValue, pnIncrement)

-- 🎲 A few devices, so that they meet often.
-- 端末は少しだけ（よく出会うように）
deviceName :: Gen String
deviceName = elements ["lambda", "python", "rust"]

instance Arbitrary GCounter where
  arbitrary = do
    devices <- listOf deviceName
    pure (foldr increment (GCounter Map.empty) devices)

instance Arbitrary AddCounter where
  arbitrary = do
    GCounter counts <- arbitrary
    pure (AddCounter counts)

instance Arbitrary a => Arbitrary (Lww a) where
  arbitrary = Lww <$> chooseInt (0, 3) <*> deviceName <*> arbitrary

instance Arbitrary a => Arbitrary (LwwDraft a) where
  arbitrary = LwwDraft <$> arbitrary

-- 🎲 An OR-Set made by a random list of adds and removes.
-- 追加と削除のでたらめな並びで作った OR-Set
instance Arbitrary (OrSet Char) where
  arbitrary = do
    ops <- listOf ((,) <$> elements "abc" <*> arbitrary)
    dev <- deviceName
    let step set (n, (item, isAdd))
          | isAdd     = add (dev, n) item set
          | otherwise = remove item set
    pure (foldl step emptySet (zip [0 ..] ops))

-- ANCHOR: laws
-- 📏 The three laws of a CRDT merge.
-- CRDT の合わせ方の、3つの法則
crdtLaws
  :: (Semigroup a, Eq a, Show a, Arbitrary a) => String -> Gen a -> Spec
crdtLaws name gen = describe name $ do
  -- 順番を入れかえても同じ（交換法則）
  it "commutative" $ forAll ((,) <$> gen <*> gen) $ \(a, b) ->
    a <> b === b <> a
  -- 括弧の位置を変えても同じ（結合法則）
  it "associative" $ forAll ((,,) <$> gen <*> gen <*> gen) $ \(a, b, c) ->
    (a <> b) <> c === a <> (b <> c)
  -- 同じものを2回合わせても同じ（冪等）
  it "idempotent" $ forAll gen $ \a ->
    a <> a === a
-- ANCHOR_END: laws

spec :: Spec
spec = describe "Modern.Crdt" $ do
  -- ANCHOR: lawSpecs
  crdtLaws "GCounter" (arbitrary :: Gen GCounter)
  crdtLaws "Lww" (arbitrary :: Gen (Lww Int))
  crdtLaws "OrSet" (arbitrary :: Gen (OrSet Char))
  -- ANCHOR_END: lawSpecs
  -- ANCHOR: draftSpecs
  -- 足して合わせる下書きは、冪等が破れる（失敗するのが正解）
  it "AddCounter is not idempotent (expected to fail)" $ expectFailure $
    property $ \a -> (a :: AddCounter) <> a === a
  -- 左を残す下書きは、交換法則が破れる（失敗するのが正解）
  it "LwwDraft is not commutative (expected to fail)" $ expectFailure $
    withMaxSuccess 1000 $ property $ \a b ->
      (a :: LwwDraft Int) <> b === b <> a
  -- ANCHOR_END: draftSpecs
  -- 2台で別々に増やして、合わせる
  it "counts from two devices" $ do
    let start  = GCounter Map.empty
        laptop = increment "lambda" (increment "lambda" start)
        phone  = increment "python" start
    counterValue (laptop <> phone) `shouldBe` 3
  -- 消している間に、ほかの端末がもう一度追加したら、追加が勝つ
  it "adding wins over a concurrent remove" $ do
    let board  = add ("lambda", 1) "slay the slime" emptySet
        laptop = remove "slay the slime" board
        phone  = add ("python", 1) "slay the slime" board
    members (laptop <> phone) `shouldBe` Set.fromList ["slay the slime"]
    members laptop `shouldBe` Set.empty
  -- 増やして減らせるカウンター
  it "PNCounter" $ do
    let up = pnIncrement "lambda" (pnIncrement "lambda" mempty)
        c  = decrement "python" up
    pnValue (c :: PNCounter) `shouldBe` 1
