module SearchSpec (spec) where

import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import Algo.Search
import AlgoAnswers (makeChange)

spec :: Spec
spec = describe "Algo.Search" $ do
  -- 予算 100 の3人組
  it "parties" $
    parties 100 `shouldBe`
      [["Lambda", "Python", "Ruby"], ["Lambda", "Ruby", "TS"],
       ["Python", "Ruby", "TS"], ["Python", "Ruby", "GHC"]]
  -- 8 クイーンの答えは 92 通り
  it "8 queens has 92 solutions" $ length (queens 8) `shouldBe` 92
  -- どの答えも、本当に安全で、答えの数も知られた数と同じ
  it "every queens answer is safe" $ property $ forAll (chooseInt (1, 7)) $
    \n -> all (safeBoard n) (queens n)
           -- The known numbers of answers for n = 1 .. 7.
           -- n = 1 .. 7 の答えの数（よく知られた数）
           && length (queens n) == [1, 0, 0, 2, 10, 4, 40] !! (n - 1)
  -- 選び方は、全部で長さの数だけ、残りは1つ少ない
  it "select picks each element once" $ property $ \xs ->
    sort (map fst (select (xs :: [Int]))) == sort xs
      && all (\(_, rest) -> length rest == length xs - 1) (select xs)
  -- 速い版も遅い版も、同じ答え
  it "SEND + MORE = MONEY" $ do
    sendMoreMoney `shouldBe` [Puzzle 9567 1085 10652]
    sendMoreMoneySlow `shouldBe` sendMoreMoney
  -- トレーニングの解答：両替の全通り
  it "makeChange" $
    makeChange 10 [1, 5, 10] `shouldBe`
      [[1,1,1,1,1,1,1,1,1,1], [1,1,1,1,1,5], [5,5], [10]]
  -- どの払い方も、合計はちょうど
  it "makeChange always pays exactly" $ property $
    forAll (chooseInt (0, 30)) $ \n ->
      let ways = makeChange n [1, 2, 5]
          -- Count the ways by hand: how many 5s, then how many 2s.
          -- 数え方を別に：5 を何枚、次に 2 を何枚
          count = length [ () | c <- [0 .. n `div` 5]
                              , _ <- [0 .. (n - 5 * c) `div` 2] ]
      in  all ((== n) . sum) ways
            && length ways == count
            && length (nubSorted ways) == count
  where
    nubSorted = nub . sort . map sort
    safeBoard n qs =
      length qs == n
        && and [ a /= b && abs (a - b) /= abs (i - j)
               | (i, a) <- zip [0 :: Int ..] qs, (j, b) <- zip [0 ..] qs
               , i < j ]
