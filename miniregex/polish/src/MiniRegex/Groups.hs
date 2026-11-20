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
  , alwaysGroups
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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

-- ANCHOR: always
-- ☑️ The groups that every match sets, by number. A group is sure to
--    take part if the way to it has no ? or * (at least once), and if
--    it is in every branch of a |. When unsure, say "not always": a
--    group said to be always set must be set.
-- どの一致でも必ず使われるグループの番号。そこまでの道に ? や * がなく
-- （少なくとも1回）、| のどの枝にもあれば、必ず使われる。わからない
-- ときは「必ずではない」と答える。必ずと言ったグループは、必ず使われて
-- いなければならない
alwaysGroups :: Pattern -> Set Int
alwaysGroups = inPattern 1
  where
    -- k: the number the next group gets
    -- k：次のグループが付ける番号
    inPattern k (Pattern bs) = everyBranch (zipWith inBranch ks bs')
      where
        bs' = NE.toList bs
        ks  = scanl (+) k (map (length . branchGroups) bs')
    -- the branches of (?| ... ) start from the same number
    -- (?| ... ) の枝は、同じ番号から始まる
    resetBranches k (Pattern bs) =
      everyBranch (map (inBranch k) (NE.toList bs))
    everyBranch = foldr1 Set.intersection
    inBranch k (Branch ps) = Set.unions (zipWith inPiece ks ps)
      where
        ks = scanl (+) k (map (length . pieceGroups) ps)
    inPiece k (Quantified a (Quantifier c _))
      | atLeast c >= 1 = inAtom k a
    inPiece _ _        = Set.empty
    inAtom k a = case a of
      Group NonCapturing p -> inPattern k p
      Group BranchReset p  -> resetBranches k p
      Group _ p            -> Set.insert k (inPattern (k + 1) p)
      Atomic p             -> inPattern k p
      Look _ Positive p    -> inPattern k p
      NoCase x             -> inAtom k x
      -- a look that must fail keeps nothing; a call gives back
      -- the groups as they were (from the chapter on recursion)
      -- 失敗するはずの「見る」ものは何も残さない。呼び出しは、
      -- グループを呼ぶ前に戻す（再帰の章から）
      _                    -> Set.empty
-- ANCHOR_END: always
-- ANCHOR_END: all
