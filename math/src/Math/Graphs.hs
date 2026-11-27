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
  , Hours
  , hours
  , getHours
  , Roads
  , twoWay
  , roadsFrom
  , roadMap
  , dijkstra
  ) where

-- ANCHOR: imports
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
-- ANCHOR_END: imports

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
--    towns already visited, so we never walk in circles). Each turn,
--    either a new town is visited, or the waiting list gets shorter.
-- 深さ優先：行けるところまで進み、戻る（訪れた町の集合を持つので、
-- 同じところをぐるぐる回らない）。毎回、新しい町を訪れるか、
-- 待ちリストが1つ短くなる
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
--    Each next town is checked against the table updated so far, so a
--    town with several roads to it joins the queue only once.
-- 幅優先：道1本の町、次に道2本の町、と広げていく。
-- 初めて着いたときが、いちばん少ない本数。となりの町を1つ見るたびに
-- 表を更新するので、道が何本もある町も、列に入るのは1回だけ
hops :: Graph -> String -> Map String Int
hops graph start = go (Map.singleton start 0) (Seq.singleton start)
  where
    go seen Empty = seen
    go seen (town :<| queue) =
      let d = seen Map.! town
          visit (s, q) t
            | t `Map.member` s = (s, q)
            | otherwise        = (Map.insert t (d + 1) s, q |> t)
          (seen', queue') =
            foldl visit (seen, queue) (Map.findWithDefault [] town graph)
      in  go seen' queue'
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
--    A skill only named as a need counts too (it needs nothing).
--    If there is a cycle, Left gives every skill it blocked: the skills
--    in the cycle, and the skills that need them.
-- 全部を覚える順番：どのスキルも、要るものの後に来る。
-- 要るものを全部覚えたスキルを、くり返し取り出す。
-- 要るものの欄にだけ出てくるスキルも並べる（何も要らないスキル）。
-- 輪があれば、そのせいで覚えられなかったスキル（輪の中のスキルと、
-- それを要るスキル）を全部 Left で返す
topoSort :: Map String [String] -> Either [String] [String]
topoSort needs = go Set.empty allSkills
  where
    allSkills =
      Set.toList (Set.fromList (Map.keys needs ++ concat (Map.elems needs)))
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

-- ANCHOR: roads
-- ⏱️ The hours a road takes: 1 or more. The constructor Hours is not
--    exported, so outside this module the only way to make one is
--    hours, which says Nothing for 0 or less.
-- 道にかかる時間：1 時間以上。構成子 Hours は外に出さないので、
-- モジュールの外で作るには hours を通すしかない（0 以下は Nothing）
newtype Hours = Hours Int
  deriving (Eq, Ord, Show)

hours :: Int -> Maybe Hours
hours n
  | n >= 1    = Just (Hours n)
  | otherwise = Nothing

getHours :: Hours -> Int
getHours (Hours n) = n

-- 🛣️ Roads with the hours they take, both ways.
-- かかる時間の付いた道。両方向
type Roads = Map String [(String, Hours)]

-- 🔁 Add each road in both directions.
-- どの道も、行きと帰りの両方を足す
twoWay :: [(String, String, Hours)] -> Roads
twoWay roads =
  Map.fromListWith (++)
    (concat [ [(a, [(b, h)]), (b, [(a, h)])] | (a, b, h) <- roads ])

-- 🧾 Roads from plain numbers: Nothing if any road is under 1 hour.
-- ただの数から道を作る：1 時間に満たない道があれば Nothing
roadsFrom :: [(String, String, Int)] -> Maybe Roads
roadsFrom roads = twoWay <$> traverse check roads
  where
    check (a, b, n) = (\h -> (a, b, h)) <$> hours n

-- 🗺️ Inside this module we may use Hours directly.
-- このモジュールの中では、Hours を直接使える
roadMap :: Roads
roadMap = twoWay
  [ ("Start", "River", Hours 2), ("Start", "Forest", Hours 7)
  , ("River", "Forest", Hours 3), ("Forest", "Castle", Hours 4)
  , ("River", "Mountain", Hours 6), ("Mountain", "Castle", Hours 2)
  , ("Island", "Lighthouse", Hours 1) ]
-- ANCHOR_END: roads

-- ANCHOR: dijkstra
-- 🏁 Dijkstra: always settle the nearest town not yet settled.
--    A Set of (hours, town) is our priority queue: its minimum is next.
-- ダイクストラ法：まだ決まっていない町のうち、いちばん近い町を決める。
-- (時間, 町) の Set を優先度つきの列に使う。最小のものが次。
-- 合計の時間は、あふれない Integer で足す
dijkstra :: Roads -> String -> Map String Integer
dijkstra roads start = go Map.empty (Set.singleton (0, start))
  where
    go done frontier = case Set.minView frontier of
      Nothing -> done
      Just ((d, town), rest)
        | town `Map.member` done -> go done rest
        | otherwise ->
            let done'  = Map.insert town d done
                next   = [ (d + toInteger (getHours h), t)
                         | (t, h) <- Map.findWithDefault [] town roads
                         , not (t `Map.member` done') ]
            in  go done' (foldr Set.insert rest next)
-- ANCHOR_END: dijkstra
-- ANCHOR_END: all
