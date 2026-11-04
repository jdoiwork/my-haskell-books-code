-- ANCHOR: all
-- | Graph algorithms, the Haskell way: frontiers as a lazy list, a pure
--   union-find, and Data.Graph for components.
-- Haskell らしいグラフのアルゴリズム：遅延なリストの「前線」、
-- 純粋なユニオン・ファインド、Data.Graph の連結成分
module Algo.Graphs
  ( Graph
  , dungeon
  , neighbours
  , frontiers
  , distances
  , Road (..)
  , roads
  , UnionFind
  , emptyUF
  , find
  , union
  , kruskal
  , Quest (..)
  , questBoard
  , cycles
  , questOrder
  ) where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- ANCHOR: graph
-- 🗺️ Rooms and the corridors between them (both ways).
-- 部屋と、部屋をつなぐ通路（両方向）
type Graph = Map String [String]

dungeon :: Graph
dungeon = Map.fromListWith (++) both
  where
    both = concat [ [(a, [b]), (b, [a])] | (a, b) <- corridors ]
    corridors =
      [ ("gate", "hall"), ("hall", "library"), ("hall", "armory")
      , ("library", "tower"), ("armory", "cellar"), ("cellar", "tower")
      , ("tower", "throne") ]

neighbours :: Graph -> String -> [String]
neighbours g room = Map.findWithDefault [] room g
-- ANCHOR_END: graph

-- ANCHOR: frontiers
-- 🌊 Breadth-first search as a list of frontiers: the rooms at distance
--    0, 1, 2, ... Each frontier comes from the one before it.
-- 幅優先探索を「前線」のリストで：距離 0, 1, 2, ... の部屋たち。
-- どの前線も、1つ前の前線から作る
frontiers :: Graph -> String -> [Set String]
frontiers g start = map fst (takeWhile nonEmpty (iterate step first))
  where
    nonEmpty (frontier, _) = not (Set.null frontier)
    first = (Set.singleton start, Set.singleton start)
    step (frontier, seen) =
      let next = Set.fromList [ n | room <- Set.toList frontier
                                  , n <- neighbours g room ]
                   `Set.difference` seen
      in  (next, seen `Set.union` next)

-- 📏 The distance to every room we can reach.
-- たどり着ける部屋全部への距離
distances :: Graph -> String -> Map String Int
distances g start =
  Map.fromList [ (room, d) | (d, f) <- zip [0 ..] (frontiers g start)
                           , room <- Set.toList f ]
-- ANCHOR_END: frontiers

-- ANCHOR: unionFind
-- 🛣️ A road that could be built between two towns, and its cost.
-- 町と町のあいだに作れる道と、その費用
data Road = Road { from :: String, to :: String, cost :: Int }
  deriving (Show, Eq)

roads :: [Road]
roads =
  [ Road "Haskelia" "Pyland" 7, Road "Haskelia" "Rustburg" 5
  , Road "Pyland" "Rustburg" 8, Road "Pyland" "Tsville" 9
  , Road "Rustburg" "Tsville" 7, Road "Rustburg" "Rubyport" 6
  , Road "Tsville" "Rubyport" 8 ]

-- 🧺 Union-find: every town points to a "parent" town; the town at the
--    top (pointing to itself, or missing) names the group.
-- ユニオン・ファインド：どの町も「親」の町を指す。いちばん上の町
-- （自分を指す、または載っていない町）が、グループの名前
type UnionFind = Map String String

emptyUF :: UnionFind
emptyUF = Map.empty

find :: UnionFind -> String -> String
find uf town = case Map.lookup town uf of
  Just parent | parent /= town -> find uf parent
  _                            -> town

union :: UnionFind -> String -> String -> UnionFind
union uf a b = Map.insert (find uf a) (find uf b) uf
-- ANCHOR_END: unionFind

-- ANCHOR: kruskal
-- 🏗️ Kruskal: cheapest roads first; build a road only if it joins two
--    groups that are not joined yet.
-- クラスカル法：安い道から。まだつながっていない2つのグループを
-- つなぐ道だけを作る
kruskal :: [Road] -> [Road]
kruskal rs = reverse (snd (foldl build (emptyUF, []) (sortOn cost rs)))
  where
    build (uf, built) road
      | find uf (from road) == find uf (to road) = (uf, built)
      | otherwise = (union uf (from road) (to road), road : built)
-- ANCHOR_END: kruskal

-- ANCHOR: scc
-- 📜 A quest, and the quests that must be done before it.
-- 依頼と、その前に終わらせておく依頼
data Quest = Quest { quest :: String, needs :: [String] }
  deriving (Show, Eq)

questBoard :: [Quest]
questBoard =
  [ Quest "slay-dragon" ["get-sword", "learn-fire"]
  , Quest "get-sword"   ["pay-smith"]
  , Quest "pay-smith"   ["sell-gems"]
  , Quest "sell-gems"   []
  , Quest "learn-fire"  ["find-tome"]
  , Quest "find-tome"   ["ask-sage"]
  , Quest "ask-sage"    ["find-tome"] ]

-- 🔁 Groups of quests that wait for each other in a circle.
-- おたがいを待ち合って、輪になっている依頼のグループ
cycles :: [Quest] -> [[String]]
cycles qs = [ names | CyclicSCC names <- components qs ]

-- 📋 An order to do the quests in (only if there are no cycles).
-- 依頼をこなす順番（輪がないときだけ）
questOrder :: [Quest] -> Maybe [String]
questOrder qs = traverse single (components qs)
  where
    single (AcyclicSCC name) = Just name
    single (CyclicSCC _)     = Nothing

-- 🧩 stronglyConnComp wants (node, key, keys it points to); it answers
--    the components with every component after the ones it points to.
-- stronglyConnComp には（頂点, 鍵, 指す先の鍵）を渡す。答えの成分は、
-- 指す先の成分よりあとに並ぶ
components :: [Quest] -> [SCC String]
components qs = stronglyConnComp [ (quest q, quest q, needs q) | q <- qs ]
-- ANCHOR_END: scc
-- ANCHOR_END: all
