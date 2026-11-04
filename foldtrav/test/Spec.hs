module Main (main) where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import qualified Derived as D
import FoldAnswers
import Shop
import Stats
import Tree

-- 🔁 The same tree, as the derived type.
-- 同じ木を、deriving した型に
toDerived :: Tree a -> D.Tree a
toDerived Leaf                = D.Leaf
toDerived (Node left v right) = D.Node (toDerived left) v (toDerived right)

main :: IO ()
main = hspec $ do
  describe "Foldable Tree" $ do
    -- 左から順にたどると、小さい順に、重なりなく並ぶ
    it "toList gives the values in order" $ property $ \xs ->
      toList (fromList xs) == sort (nub (xs :: [Int]))
    -- 手で書いた foldMap と、deriving の foldMap は同じ順
    it "agrees with the derived instance" $ property $ \xs ->
      let t = fromList (xs :: [Int])
      in  toList t == toList (toDerived t) && sum t == sum (toDerived t)
    -- fmap id は何もしない
    it "fmap keeps the laws" $ property $ \xs ->
      let t = fromList (xs :: [Int]) in fmap id t == t
  describe "Traversable Tree" $ do
    -- Identity で traverse しても、何も変わらない（Traversable の法則）
    it "traverse Identity changes nothing" $ property $ \xs ->
      let t = fromList (xs :: [Int])
      in  runIdentity (traverse Identity t) == t
    -- 手で書いた traverse と、deriving の traverse は同じ
    it "agrees with the derived instance" $ property $ \xs ->
      let t = fromList (xs :: [Int])
          f x = if x > 0 then Just (x * 2) else Nothing
      in  fmap toDerived (traverse f t) == traverse f (toDerived t)
  describe "Stats" $ do
    -- 1回の foldMap で、4つの答え
    it "report agrees with length, sum, minimum and maximum" $
      property $ \(NonEmpty xs) ->
        report (xs :: [Int]) ==
          Report (length xs) (sum xs) (minimum xs) (maximum xs)
  describe "Shop" $ do
    -- 1つでも値段がなければ Nothing
    it "pricesMaybe" $ do
      pricesMaybe bag `shouldBe` Nothing
      pricesMaybe (fromList ["potion", "herb", "sword"]) `shouldBe`
        Just (Node (Node Leaf 10 Leaf) 50 (Node Leaf 300 Leaf))
    -- 値段のないアイテムの名前を言う
    it "pricesEither" $
      pricesEither bag `shouldBe` Left "shield is not for sale"
    -- 左から順に番号
    it "numbered" $
      map fst (toList (numbered bag)) `shouldBe` [1 .. 5]
  describe "answers" $ do
    -- ステップ2：子どもがいくつでもある木も Foldable
    it "Rose is Foldable" $ do
      toList party `shouldBe`
        ["master", "lambda", "python", "ts", "rust", "ruby"]
      length party `shouldBe` 6
    -- ステップ3：Map も Traversable
    it "restock" $ do
      restock (Map.fromList [("potion", 3), ("herb", 20)]) `shouldBe`
        Right (Map.fromList [("herb", 30), ("potion", 13)])
      restock (Map.fromList [("potion", 3), ("herb", 120)]) `shouldBe`
        Left "cannot order 120"
