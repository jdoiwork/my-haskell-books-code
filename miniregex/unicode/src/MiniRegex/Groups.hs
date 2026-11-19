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
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE
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
pieceGroups (Quantified a _) = atomGroups a

-- A group's own ( comes before the ( inside it.
-- グループ自身の ( は、中の ( より先
atomGroups :: Atom -> [Capture]
atomGroups (Group NonCapturing p) = patternGroups p
atomGroups (Group c p)            = c : patternGroups p
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
-- ANCHOR_END: all
