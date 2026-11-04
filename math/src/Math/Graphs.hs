-- ANCHOR: all
-- | Graphs: towns and roads, and skills that need other skills.
-- グラフ：町と街道、ほかのスキルが要るスキル
module Math.Graphs
  ( Graph
  , fromRoads
  , townMap
  , reachable
  , hops
  , topoSort
  , skillTree
  , Roads
  , roadMap
  , dijkstra
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

-- ANCHOR: graph
-- 🗺️ A graph as an adjacency list: each town, and the towns next to it.
-- 隣接リストのグラフ：町ごとに、となりの町のリスト
type Graph = Map String [String]

-- 🛣️ Two-way roads: each road is added in both directions.
-- 両方向の道：どの道も、行きと帰りの両方を足す
fromRoads :: [(String, String)] -> Graph
fromRoads roads =
  Map.fromListWith (++)
    (concat [ [(a, [b]), (b, [a])] | (a, b) <- roads ])

-- 🏘️ The towns of the kingdom. The island has no road to the rest.
-- 王国の町。島には、ほかへの道がない
townMap :: Graph
townMap = fromRoads
  [ ("Start", "River"), ("Start", "Forest"), ("River", "Forest")
  , ("Forest", "Castle"), ("River", "Mountain"), ("Mountain", "Castle")
  , ("Island", "Lighthouse")
  ]
-- ANCHOR_END: graph

-- ANCHOR: dfs
-- 🧭 Depth-first: go as far as you can, then come back (with a set of
--    towns already visited, so we never walk in circles).
-- 深さ優先：行けるところまで進み、戻る（訪れた町の集合を持つので、
-- 同じところをぐるぐる回らない）
reachable :: Graph -> String -> Set String
reachable graph start = go Set.empty [start]
  where
    go visited [] = visited
    go visited (town : rest)
      | town `Set.member` visited = go visited rest
      | otherwise =
          let next = Map.findWithDefault [] town graph
          in  go (Set.insert town visited) (next ++ rest)
-- ANCHOR_END: dfs

-- ANCHOR: bfs
-- 🌊 Breadth-first: visit the towns 1 road away, then 2 roads away, ...
--    The first time we reach a town is by the fewest roads.
-- 幅優先：道1本の町、次に道2本の町、と広げていく。
-- 初めて着いたときが、いちばん少ない本数
hops :: Graph -> String -> Map String Int
hops graph start = go (Map.singleton start 0) (Seq.singleton start)
  where
    go seen Empty = seen
    go seen (town :<| queue) =
      let d     = seen Map.! town
          fresh = [ t | t <- Map.findWithDefault [] town graph
                      , not (t `Map.member` seen) ]
          seen' = foldr (\t -> Map.insert t (d + 1)) seen fresh
      in  go seen' (foldl (|>) queue fresh)
-- ANCHOR_END: bfs

-- ANCHOR: topo
-- 📜 Skills and the skills they need first.
-- スキルと、先に要るスキル
skillTree :: Map String [String]
skillTree = Map.fromList
  [ ("magic basics", [])
  , ("fire",         ["magic basics"])
  , ("ice",          ["magic basics"])
  , ("steam",        ["fire", "ice"])
  , ("sword",        [])
  , ("fire sword",   ["fire", "sword"])
  ]

-- 🪜 An order to learn everything: a skill comes after all it needs.
--    Repeatedly take the skills whose needs are all learned.
--    Left gives the skills stuck in a cycle.
-- 全部を覚える順番：どのスキルも、要るものの後に来る。
-- 要るものを全部覚えたスキルを、くり返し取り出す。
-- 輪になって進めないときは、残ったスキルを Left で返す
topoSort :: Map String [String] -> Either [String] [String]
topoSort needs = go Set.empty (Map.keys needs)
  where
    go _       []      = Right []
    go learned waiting =
      let ready = [ s | s <- waiting
                      , all (`Set.member` learned)
                            (Map.findWithDefault [] s needs) ]
      in  if null ready
            then Left waiting
            else do
              let learned' = foldr Set.insert learned ready
              rest <- go learned' (filter (`notElem` ready) waiting)
              pure (ready ++ rest)
-- ANCHOR_END: topo

-- ANCHOR: dijkstra
-- 🛣️ Roads with lengths (in hours), both ways.
-- 長さ（時間）の付いた道。両方向
type Roads = Map String [(String, Int)]

roadMap :: Roads
roadMap = Map.fromListWith (++)
  (concat [ [(a, [(b, h)]), (b, [(a, h)])] | (a, b, h) <- roads ])
  where
    roads = [ ("Start", "River", 2), ("Start", "Forest", 7)
            , ("River", "Forest", 3), ("Forest", "Castle", 4)
            , ("River", "Mountain", 6), ("Mountain", "Castle", 2) ]

-- 🏁 Dijkstra: always settle the nearest town not yet settled.
--    A Set of (hours, town) is our priority queue: its minimum is next.
-- ダイクストラ法：まだ決まっていない町のうち、いちばん近い町を決める。
-- (時間, 町) の Set を優先度つきの列に使う。最小のものが次
dijkstra :: Roads -> String -> Map String Int
dijkstra roads start = go Map.empty (Set.singleton (0, start))
  where
    go done frontier = case Set.minView frontier of
      Nothing -> done
      Just ((d, town), rest)
        | town `Map.member` done -> go done rest
        | otherwise ->
            let done'  = Map.insert town d done
                next   = [ (d + h, t)
                         | (t, h) <- Map.findWithDefault [] town roads
                         , not (t `Map.member` done') ]
            in  go done' (foldr Set.insert rest next)
-- ANCHOR_END: dijkstra
-- ANCHOR_END: all
