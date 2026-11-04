module GraphsSpec (spec) where

import Data.List (elemIndex, permutations)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import GraphsAnswers
import Math.Graphs

-- 🎲 Random roads among towns "A".."F", with lengths 1..9.
-- 町 A〜F のあいだの、長さ 1〜9 のでたらめな道
genRoads :: Gen Roads
genRoads = do
  edges <- listOf ((,,) <$> town <*> town <*> choose (1, 9))
  pure (Map.fromListWith (++)
          (concat [ [(a, [(b, h)]), (b, [(a, h)])]
                  | (a, b, h) <- edges, a /= b ]))
  where
    town = elements (map (: []) "ABCDEF")

-- 🐢 The slow way: try every order of the towns as a path.
-- 遅い方法：町の並べ方を全部、道として試す
bruteForce :: Roads -> String -> String -> Maybe Int
bruteForce roads start goal
  | start == goal = Just 0
  | otherwise =
      let others = filter (`notElem` [start, goal]) (Map.keys roads)
          paths  = [ start : mid ++ [goal]
                   | k <- [0 .. length others]
                   , perm <- permutations others
                   , let mid = take k perm ]
          costs  = [ c | p <- paths, Just c <- [cost p] ]
      in  if null costs then Nothing else Just (minimum costs)
  where
    -- There can be several roads between two towns: use the shortest.
    -- 2つの町のあいだに道が何本もあることがある：いちばん短い道を使う
    cost (a : b : rest) =
      case [ h | (t, h) <- Map.findWithDefault [] a roads, t == b ] of
        [] -> Nothing
        hs -> (minimum hs +) <$> cost (b : rest)
    cost _ = Just 0

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- 覚える順番では、どのスキルも、要るスキルより後に来る
  it "topoSort puts every skill after the skills it needs" $
    case topoSort skillTree of
      Left stuck  -> expectationFailure (show stuck)
      Right order ->
        and [ elemIndex need order < elemIndex skill order
            | (skill, needs) <- Map.toList skillTree, need <- needs ]
          `shouldBe` True
  -- 輪になったスキルは、覚えられない
  it "topoSort finds a cycle" $
    topoSort (Map.fromList [("a", ["b"]), ("b", ["a"]), ("c", [])])
      `shouldBe` Left ["a", "b"]
  -- ダイクストラ法は、全部の道を試すのと同じ答え
  it "dijkstra agrees with trying every path" $
    forAll genRoads $ \roads ->
      and [ Map.lookup goal (dijkstra roads "A") == bruteForce roads "A" goal
          | goal <- Map.keys roads, "A" `Map.member` roads ]
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "the kingdom has two islands" $
      islands townMap `shouldBe`
        [ ["Castle", "Forest", "Mountain", "River", "Start"]
        , ["Island", "Lighthouse"] ]
    it "the fastest route to the castle" $
      route roadMap "Start" "Castle"
        `shouldBe` Just ["Start", "River", "Forest", "Castle"]
