module PurelySpec (spec) where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

import Algo.Purely
import AlgoAnswers (kSmallest, peek, queueLength)

-- ANCHOR: model
-- 🎲 One step on the line: join with a number, or leave.
-- 列の1手：数を持って並ぶか、抜けるか
data Op = Push Int | Pop
  deriving Show

instance Arbitrary Op where
  arbitrary = frequency [(3, Push <$> arbitrary), (2, pure Pop)]

-- 📋 The model: a plain list, front first.
-- お手本：ふつうのリスト（先頭が列の前）
runModel :: [Op] -> [Maybe Int]
runModel = go []
  where
    go :: [Int] -> [Op] -> [Maybe Int]
    go _        []            = []
    go xs       (Push x : ops) = go (xs ++ [x]) ops
    go []       (Pop    : ops) = Nothing : go [] ops
    go (x : xs) (Pop    : ops) = Just x  : go xs ops

-- 🚶 The same steps on our Queue.
-- 同じ手順を、作った Queue で
runQueue :: [Op] -> [Maybe Int]
runQueue = go emptyQueue
  where
    go :: Queue Int -> [Op] -> [Maybe Int]
    go _ []             = []
    go q (Push x : ops) = go (push x q) ops
    go q (Pop    : ops) = case pop q of
      Nothing      -> Nothing : go q  ops
      Just (x, q') -> Just x  : go q' ops
-- ANCHOR_END: model

spec :: Spec
spec = describe "Algo.Purely" $ do
  -- 並べて全部取り出すと、並べた順
  it "drain (queueFromList xs) == xs" $ property $ \xs ->
    drain (queueFromList xs) === (xs :: [Int])
  -- ANCHOR: modelProp
  -- どんな手順でも、ふつうのリストと同じ答え
  it "behaves like a list for any steps" $ property $ \ops ->
    runQueue ops === runModel ops
  -- ANCHOR_END: modelProp
  -- 決まりがいつも守られている
  it "keeps its invariant" $ property $ \xs ys ->
    let q = foldr (const (maybe emptyQueue snd . pop))
                  (queueFromList (xs ++ ys :: [Int])) ys
    in queueInvariant q
  -- 古い版も使える
  it "keeps old versions" $ do
    let q0 = queueFromList "ab"
        q1 = push 'c' q0
        q2 = push 'z' q0
    drain q0 `shouldBe` "ab"
    drain q1 `shouldBe` "abc"
    drain q2 `shouldBe` "abz"
  -- 遅いキューも同じ答え
  it "SlowQueue agrees" $ property $ \xs ->
    let q = foldl (flip slowPush) slowEmpty (xs :: [Int])
        go s = maybe [] (\(x, s') -> x : go s') (slowPop s)
    in go q === xs
  -- ヒープソートは sort と同じ
  it "heapSort == sort" $ property $ \xs ->
    heapSort xs === sort (xs :: [Int])
  -- 入れても取っても、左偏ヒープのまま
  it "stays leftist" $ property $ \xs n ->
    let h = heapFromList (xs :: [Int])
        shrunk = iterate (maybe Empty snd . deleteMin) h !! (abs n `mod` 5)
    in leftist h .&&. leftist shrunk
  -- 合わせたヒープの最小値
  it "merge keeps the minimum" $ property $ \xs ys ->
    let both = xs ++ ys :: [Int]
        expected = if null both then Nothing else Just (minimum both)
    in findMin (merge (heapFromList xs) (heapFromList ys)) === expected
  -- トレーニングの解答
  it "answers" $ do
    queueLength (queueFromList "goblin") `shouldBe` 6
    peek (queueFromList "goblin") `shouldBe` Just 'g'
    peek (emptyQueue :: Queue Char) `shouldBe` Nothing
    kSmallest 3 [5, 2, 8, 1, 9, 3 :: Int] `shouldBe` [1, 2, 3]
  it "kSmallest == take k . sort" $ property $ \k xs ->
    kSmallest k xs === take k (sort (xs :: [Int]))
