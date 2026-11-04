module GraphsSpec (spec) where

import Data.List (sort, subsequences)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

import Algo.Graphs
import AlgoAnswers (islands)

-- 📜 The spec of the distance: try longer and longer walks.
-- 距離の仕様：歩く数を1歩ずつ増やして、着けるかを試す
distanceSpec :: Graph -> String -> String -> Maybe Int
distanceSpec g a b = lookupFirst [0 .. Map.size g]
  where
    reach 0 = [a]
    reach k = [ n | r <- reach (k - 1), n <- neighbours g r ]
    lookupFirst ks = case [ k | k <- ks, b `elem` reach k ] of
      k : _ -> Just k
      []    -> Nothing

spec :: Spec
spec = describe "Algo.Graphs" $ do
  -- 前線は、距離ごとの部屋
  it "frontiers of the dungeon" $
    map Set.toList (frontiers dungeon "gate") `shouldBe`
      [["gate"], ["hall"], ["armory", "library"], ["cellar", "tower"],
       ["throne"]]
  -- どの部屋への距離も、仕様と同じ
  it "distances agree with the spec" $
    and [ Map.lookup b (distances dungeon "gate")
            == distanceSpec dungeon "gate" b
        | b <- Map.keys dungeon ]
  -- ANCHOR: kruskalProps
  -- 作った道は、町の数 − 1 本で、全部の町がつながる
  it "kruskal connects every town with n - 1 roads" $ do
    let built = kruskal roads
        towns = Set.fromList (concat [ [from r, to r] | r <- roads ])
        uf    = foldl (\u r -> union u (from r) (to r)) emptyUF built
    length built `shouldBe` Set.size towns - 1
    Set.size (Set.map (find uf) towns) `shouldBe` 1
  -- ANCHOR_END: kruskalProps
  -- いちばん安い（全部の町をつなぐ道の組み合わせを、全部試した仕様と比べる）
  it "kruskal is the cheapest" $ do
    let towns = Set.fromList (concat [ [from r, to r] | r <- roads ])
        connects rs =
          let uf = foldl (\u r -> union u (from r) (to r)) emptyUF rs
          in  Set.size (Set.map (find uf) towns) == 1
        cheapest = minimum [ sum (map cost rs) | rs <- subsequences roads
                                               , connects rs ]
    sum (map cost (kruskal roads)) `shouldBe` cheapest
  -- ユニオン・ファインド：つないだものは、同じグループ
  it "union joins groups" $ property $ \pairs ->
    let towns = [ (show (a `mod` 5 :: Int), show (b `mod` 5 :: Int))
                | (a, b) <- pairs ]
        uf = foldl (\u (a, b) -> union u a b) emptyUF towns
    in  all (\(a, b) -> find uf a == find uf b) towns
  -- 依頼の輪を見つける
  it "finds the cycle" $
    map sort (cycles questBoard) `shouldBe` [["ask-sage", "find-tome"]]
  -- 輪がなければ、順番が決まる
  it "orders quests without a cycle" $
    questOrder (filter ((/= "ask-sage") . quest) questBoard')
      `shouldBe` Just ["sell-gems", "pay-smith", "get-sword", "find-tome",
                       "learn-fire", "slay-dragon"]
  -- トレーニングの解答：島の数
  it "islands" $
    islands ["##..#", "#...#", "..#..", ".....", "##.##"] `shouldBe` 5
  where
    questBoard' = [ if quest q == "find-tome" then q { needs = [] } else q
                  | q <- questBoard ]
