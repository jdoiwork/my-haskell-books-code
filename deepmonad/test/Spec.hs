-- ANCHOR: head
-- The Arbitrary instances live here, only for the tests.
-- Arbitrary のインスタンスは、テストのためだけに、ここに置く
{-# OPTIONS_GHC -Wno-orphans #-}
-- AllowAmbiguousTypes: applicativeHomomorphism names its box only in the
--   type, so we choose the box with @ when we use it.
-- AllowAmbiguousTypes：applicativeHomomorphism は、箱を型の中でしか
--   使わないので、使うときに @ で箱を選ぶ
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main (main) where

-- hspec: describe and it. QuickCheck: property, Arbitrary,
--   and Fun / Fn (random functions that can be shown).
-- hspec：describe と it。QuickCheck：property、Arbitrary、
--   Fun・Fn（見せられる、でたらめな関数）
import Control.Monad (ap)
import Test.Hspec
import Test.QuickCheck

import Counted
import DeepAnswers
import Instances
import Quest
import Steps
import Useful
import qualified Recap as R
import Validation
-- ANCHOR_END: head

-- ANCHOR: arbitrary
-- 🎲 Random quests: failed at some place, or cleared with a number.
-- でたらめな依頼：どこかで失敗したか、数を持って進んだか
instance Arbitrary a => Arbitrary (Quest a) where
  arbitrary = oneof
    [ Failed <$> elements ["forest", "cave", "tower"]
    , Cleared <$> arbitrary
    ]
-- ANCHOR_END: arbitrary

-- ANCHOR: arbitraryBroken
-- 🎲 Random counted boxes: a small count (never below 0) and a value.
--    shrink makes a failing example smaller, keeping the count >= 0.
-- でたらめな数える箱：小さな回数（0 より下にしない）と値。
-- shrink は、失敗した例を、回数を 0 以上に保ったまま小さくする
instance Arbitrary a => Arbitrary (Counted a) where
  arbitrary = Counted <$> chooseInt (0, 5) <*> arbitrary
  shrink (Counted n x) =
    [ Counted n' x' | (n', x') <- shrink (n, x), n' >= 0 ]

-- 🎲 Random step boxes, made the same way.
-- でたらめな歩数の箱。作り方は同じ
instance Arbitrary a => Arbitrary (Steps a) where
  arbitrary = Steps <$> chooseInt (0, 5) <*> arbitrary
  shrink (Steps n x) =
    [ Steps n' x' | (n', x') <- shrink (n, x), n' >= 0 ]
-- ANCHOR_END: arbitraryBroken

-- ANCHOR: arbitraryValidation
-- 🎲 Random validations: some reasons, or a value.
-- でたらめな Validation：理由がいくつか、または値
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Invalid <$> arbitrary, Valid <$> arbitrary]
-- ANCHOR_END: arbitraryValidation

-- ANCHOR: arbitraryPair
-- 🎲 Random pairs: two random values.
-- でたらめな2つ組：でたらめな値を2つ
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary
-- ANCHOR_END: arbitraryPair

-- ANCHOR: functorLaws
-- 🧺 The two Functor laws, for any Functor we can generate and compare.
-- Functor の2つの法則。生成できて、比べられる Functor なら何でも
functorIdentity :: (Functor f, Eq (f Int)) => f Int -> Bool
functorIdentity box = fmap id box == box

functorCompose
  :: (Functor f, Eq (f Int)) => Fun Int Int -> Fun Int Int -> f Int -> Bool
functorCompose (Fn f) (Fn g) box =
  fmap (g . f) box == (fmap g . fmap f) box
-- ANCHOR_END: functorLaws

-- ANCHOR: monadLaws
-- 🔗 The three Monad laws.
-- Monad の3つの法則
leftIdentity :: (Monad m, Eq (m Int)) => Int -> Fun Int (m Int) -> Bool
leftIdentity x (Fn f) = (pure x >>= f) == f x

rightIdentity :: (Monad m, Eq (m Int)) => m Int -> Bool
rightIdentity m = (m >>= pure) == m

associativity
  :: (Monad m, Eq (m Int))
  => m Int -> Fun Int (m Int) -> Fun Int (m Int) -> Bool
associativity m (Fn f) (Fn g) =
  ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
-- ANCHOR_END: monadLaws

-- ANCHOR: applicativeLaws
-- 🧩 The four Applicative laws. Random functions come in boxes as Fun,
--    and fmap applyFun turns them into real functions.
-- Applicative の4つの法則。でたらめな関数は Fun として箱に入って届くので、
-- fmap applyFun で、本物の関数にする
applicativeIdentity :: (Applicative f, Eq (f Int)) => f Int -> Bool
applicativeIdentity v = (pure id <*> v) == v

applicativeHomomorphism
  :: forall f. (Applicative f, Eq (f Int)) => Fun Int Int -> Int -> Bool
applicativeHomomorphism (Fn g) x =
  (pure g <*> pure x) == (pure (g x) :: f Int)

applicativeInterchange
  :: (Applicative f, Eq (f Int)) => f (Fun Int Int) -> Int -> Bool
applicativeInterchange u y = (fs <*> pure y) == (pure ($ y) <*> fs)
  where
    fs = fmap applyFun u

applicativeComposition
  :: (Applicative f, Eq (f Int))
  => f (Fun Int Int) -> f (Fun Int Int) -> f Int -> Bool
applicativeComposition u v w =
  (pure (.) <*> us <*> vs <*> w) == (us <*> (vs <*> w))
  where
    us = fmap applyFun u
    vs = fmap applyFun v

-- 🔗 <*> must agree with >>= (ap is <*> written with >>=).
-- <*> は >>= とそろっていること（ap は、>>= で書いた <*>）
apAgrees :: (Monad m, Eq (m Int)) => m (Fun Int Int) -> m Int -> Bool
apAgrees u x = (fs <*> x) == (fs `ap` x)
  where
    fs = fmap applyFun u
-- ANCHOR_END: applicativeLaws

-- ANCHOR: main
main :: IO ()
main = hspec $ do
-- ANCHOR_END: main
  -- ANCHOR: questSpec
  describe "Quest keeps the laws" $ do
    -- fmap id は、何もしない
    it "functor identity" $ property (functorIdentity @Quest)
    -- fmap を2回するのと、合成して1回するのは同じ
    it "functor composition" $ property (functorCompose @Quest)
    -- pure で包んで渡すのは、そのまま渡すのと同じ
    it "left identity" $ property (leftIdentity @Quest)
    -- pure で受けるのは、何もしないのと同じ
    it "right identity" $ property (rightIdentity @Quest)
    -- つなぐ順の括弧は、どこに付けても同じ
    it "associativity" $ property (associativity @Quest)
  -- ANCHOR_END: questSpec
  -- ANCHOR: applicativeSpec
  describe "Quest and Validation keep the Applicative laws" $ do
    -- pure id を使っても、何も変わらない
    it "identity (Quest)" $ property (applicativeIdentity @Quest)
    -- pure どうしを <*> でつなぐのは、先に計算して pure するのと同じ
    it "homomorphism (Quest)" $ property (applicativeHomomorphism @Quest)
    -- 関数の箱と値の箱は、入れ替えても同じ
    it "interchange (Quest)" $ property (applicativeInterchange @Quest)
    -- 関数をつないでから使っても、順に使っても同じ
    it "composition (Quest)" $ property (applicativeComposition @Quest)
    -- Quest の <*> は、>>= とそろっている
    it "<*> agrees with >>= (Quest)" $ property (apAgrees @Quest)
    it "identity (Validation)" $
      property (applicativeIdentity @(Validation [String]))
    it "homomorphism (Validation)" $
      property (applicativeHomomorphism @(Validation [String]))
    it "interchange (Validation)" $
      property (applicativeInterchange @(Validation [String]))
    it "composition (Validation)" $
      property (applicativeComposition @(Validation [String]))
  -- ANCHOR_END: applicativeSpec
  -- ANCHOR: brokenSpec
  describe "broken instances" $ do
    -- Counted は fmap id で数が増えてしまう
    it "Counted breaks functor identity" $
      property (expectFailure (functorIdentity @Counted))
    -- Steps は pure を通すと1歩増えてしまう
    it "Steps breaks left identity" $
      property (expectFailure (leftIdentity @Steps))
  -- ANCHOR_END: brokenSpec
  describe "examples" $ do
    -- 魚の演算子でつないでも、do と同じ
    it "journeyK agrees with journey" $ property $ \hp ->
      journeyK hp == journey hp
    -- 途中までの名前を使っても同じ
    it "lowerFloors >=> tower is the journey" $ property $ \hp ->
      (lowerFloors hp >>= tower) == journey hp
    -- pure を let に書き換えただけで、歩数が変わる
    it "Steps changes when pure is replaced by let" $
      (withPure, withoutPure) `shouldBe` (Steps 11 100, Steps 10 100)
    -- Validation は、問題を全部集める
    it "Validation collects every problem" $
      applyAll "" 9 "thief" `shouldBe`
        Invalid ["name is empty", "too young", "unknown job"]
    -- Either は、最初の問題で止まる
    it "Either stops at the first problem" $
      applyEither "" 9 "thief" `shouldBe` Left ["name is empty"]
    -- 組と関数と ZipList
    it "pairs, functions and ZipList" $
      (loot, found, doubleAfterHeal 5, everyPair, pairwise) `shouldBe`
        ( ("goblin", 30), ("sword, shield", 15), 30
        , [11, 21, 31, 12, 22, 32, 13, 23, 33], [11, 22, 33] )
  describe "what the classes buy" $ do
    -- 書き直した定義も、Maybe で本物と同じに動く
    it "the recap classes work on Maybe" $
      ( R.fmap (+ 1) (Just 2 :: Maybe Int)
      , (+) R.<$> Just 1 R.<*> (Just 2 :: Maybe Int)
      , Just 3 R.>>= (\x -> Just (x * 2 :: Int)) )
        `shouldBe` (Just 3, Just 3, Just 6)
    -- 一度書いた関数が、どの箱にも使える
    it "heal and pairUp work on any box" $ do
      heal 10 (Just 30) `shouldBe` Just 40
      heal 10 [30, 50, 70] `shouldBe` [40, 60, 80]
      pairUp ["Lambda", "Python"] ["Rust", "TS"] `shouldBe`
        [ ("Lambda", "Rust"), ("Lambda", "TS")
        , ("Python", "Rust"), ("Python", "TS") ]
    -- 同じ checkout が、箱ごとに別の働きをする
    it "checkout depends on the box" $ do
      checkout payMaybe 100 [30, 80] `shouldBe` Nothing
      checkout payEither 100 [30, 80] `shouldBe`
        Left "short for a 80 G item"
      checkout payHaggle 100 [30, 80] `shouldBe` [30, 5, 45]
      traverse readHp ["30", "x"] `shouldBe` Nothing
  describe "answers" $ do
    -- ANCHOR: pairSpec
    -- ステップ2：Pair は恒等の法則を守る
    it "Pair keeps functor identity" $ property (functorIdentity @Pair)
    -- ステップ2：Pair は合成の法則も守る
    it "Pair keeps functor composition" $ property (functorCompose @Pair)
    -- ANCHOR_END: pairSpec
    -- ステップ3：お金の確かめも、まとめて集まる
    it "applyWithGold collects the gold problem too" $
      applyWithGold "" 30 "mage" 20 `shouldBe`
        Invalid ["name is empty", "not enough gold"]
