-- ANCHOR: all
-- | Answers to the training of "Graphs".
-- 「グラフ」のトレーニングの解答
module GraphsAnswers
  ( islands
  , route
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Math.Graphs

-- ANCHOR: islands
-- 🏝️ Step 2: the groups of towns connected to each other.
-- ステップ2：互いに行き来できる町のまとまり
islands :: Graph -> [[String]]
islands graph = go (Map.keys graph)
  where
    go [] = []
    go (town : rest) =
      let group = reachable graph town
      in  Set.toList group : go (filter (`Set.notMember` group) rest)
-- ANCHOR_END: islands

-- ANCHOR: route
-- 🧭 Step 3: the towns on a fastest route. Walk back from the goal:
--    a town before it is one whose distance + road == goal's distance.
-- ステップ3：いちばん早い道の町。ゴールから逆にたどる：
-- 1つ前の町は「その町までの時間 + 道の時間 == ゴールまでの時間」の町
route :: Roads -> String -> String -> Maybe [String]
route roads start goal = do
  let dist = dijkstra roads start
  _ <- Map.lookup goal dist
  pure (reverse (back dist goal))
  where
    back :: Map String Int -> String -> [String]
    back dist town
      | town == start = [start]
      | otherwise =
          let here = Map.lookup town dist
              prev = [ t | (t, h) <- Map.findWithDefault [] town roads
                         , Map.lookup t dist == fmap (subtract h) here ]
          in  case prev of
                (t : _) -> town : back dist t
                []      -> [town]
-- ANCHOR_END: route
-- ANCHOR_END: all
