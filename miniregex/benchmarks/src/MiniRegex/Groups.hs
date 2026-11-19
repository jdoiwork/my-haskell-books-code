-- ANCHOR: all
-- ANCHOR: header
-- | The groups of a tree: walk it from the left, and count each ( that
--   keeps what it found. That order is the groups' numbers.
-- 木の中のグループ：左からなぞって、見つけたものを覚える ( を数える。
-- その順番が、グループの番号
module MiniRegex.Groups
  ( patternGroups
  , branchGroups
  , pieceGroups
  , groupCount
  , groupNames
  , groupBody
  , calls
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import MiniRegex.Syntax
-- ANCHOR_END: imports

-- ANCHOR: walk
-- 🚶 The groups that keep what they found, in the order of their (.
-- 見つけたものを覚えるグループを、( の順に
patternGroups :: Pattern -> [Capture]
patternGroups (Pattern bs) = concatMap branchGroups (NE.toList bs)

branchGroups :: Branch -> [Capture]
branchGroups (Branch ps) = concatMap pieceGroups ps

pieceGroups :: Piece -> [Capture]
pieceGroups (Anchor _)       = []
pieceGroups KeepOut          = []
pieceGroups (Quantified a _) = atomGroups a

-- A group's own ( comes before the ( inside it. In (?| ... ), the
-- branches share their numbers: as many as the branch with the most.
-- グループ自身の ( は、中の ( より先。(?| ... ) の枝は番号を分け合う：
-- いちばん多い枝の数だけ
atomGroups :: Atom -> [Capture]
atomGroups (Group NonCapturing p) = patternGroups p
atomGroups (Group BranchReset (Pattern bs)) =
  foldr longer [] (map branchGroups (NE.toList bs))
  where
    longer a b = if length a >= length b then a else b
atomGroups (Group c p)            = c : patternGroups p
-- the groups inside an atomic group or a look count, too
-- アトミックグループや「見る」ものの中のグループも数える
atomGroups (Atomic p)             = patternGroups p
atomGroups (Look _ _ p)           = patternGroups p
atomGroups _                      = []
-- ANCHOR_END: walk

-- ANCHOR: names
-- 🔢 How many groups there are.
-- グループがいくつあるか
groupCount :: Pattern -> Int
groupCount = length . patternGroups

-- 📛 The named groups and their numbers: (a)(?<year>b) gives
--    [("year", 2)].
-- 名前付きのグループと、その番号：(a)(?<year>b) なら [("year", 2)]
groupNames :: Pattern -> [(Text, Int)]
groupNames p = [(n, i) | (Named n, i) <- zip (patternGroups p) [1 ..]]
-- ANCHOR_END: names

-- ANCHOR: body
-- 📦 The pattern of group n, to call: the whole pattern for 0. With
--    (?| ... ), two groups may have the same number: the first one.
-- 呼ぶための、n 番のグループのパターン。0 ならパターン全体。(?| ... ) では
-- 2つのグループが同じ番号のことがある。そのときは最初のもの
groupBody :: Int -> Pattern -> Maybe Pattern
groupBody 0 whole = Just whole
groupBody n whole = listToMaybe (inPattern 1 whole)
  where
    -- k: the number the next group gets
    -- k：次のグループが付ける番号
    inPattern k (Pattern bs) =
      concat (zipWith inBranch (starts k branchGroups bs') bs')
      where bs' = NE.toList bs
    inBranch k (Branch ps) =
      concat (zipWith inPiece (starts k pieceGroups ps) ps)
    inPiece k (Quantified a _) = inAtom k a
    inPiece _ _                = []
    inAtom k a = case a of
      Group NonCapturing p -> inPattern k p
      Group BranchReset (Pattern bs) ->
        concat [inBranch k b | b <- NE.toList bs]
      Group _ p
        | k == n    -> p : inPattern (k + 1) p
        | otherwise -> inPattern (k + 1) p
      Atomic p     -> inPattern k p
      Look _ _ p   -> inPattern k p
      _            -> []
    starts k groupsOf xs = scanl (+) k (map (length . groupsOf) xs)

-- 📞 The groups that the pattern calls somewhere, each once.
-- パターンのどこかで呼んでいるグループ（1つにつき1回）
calls :: Pattern -> [Int]
calls = nub . inPattern
  where
    inPattern (Pattern bs) = concatMap inBranch (NE.toList bs)
    inBranch (Branch ps) = concat [inAtom a | Quantified a _ <- ps]
    inAtom a = case a of
      Call g     -> [g]
      NoCase x   -> inAtom x
      Group _ p  -> inPattern p
      Atomic p   -> inPattern p
      Look _ _ p -> inPattern p
      _          -> []
-- ANCHOR_END: body
-- ANCHOR_END: all
