-- ANCHOR: all
-- | How many characters a part of a pattern can take: at least this
--   many, and at most that many (or with no limit). A lookbehind needs
--   it, to know how far back to step.
-- パターンの一部が、何文字を食べうるか：少なくとも何文字、多くても
-- 何文字（か、上限なし）。後読みが、どこまで下がるかを知るために要る
module MiniRegex.Width
  ( Width (..)
  , patternWidth
  , branchWidth
  , hasBackref
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE
import Numeric.Natural (Natural)

import MiniRegex.Rules (lookCount)
import MiniRegex.Syntax
-- ANCHOR_END: imports

-- ANCHOR: width
-- 📏 At least `shortest` characters, and at most `longest` (Nothing:
--    as many as there are).
-- 少なくとも shortest 文字、多くても longest 文字（Nothing は、いくらでも）
data Width = Width
  { shortest :: Natural
  , longest  :: Maybe Natural
  }
  deriving (Show, Eq)

-- 🌳 Of the branches, the shortest of the short ends and the longest of
--    the long ones.
-- 枝のうち、短いほうの端はいちばん短いもの、長いほうの端はいちばん
-- 長いもの
patternWidth :: Pattern -> Width
patternWidth (Pattern bs) = foldr1 either' (fmap branchWidth bs)
  where
    either' (Width s1 l1) (Width s2 l2) =
      Width (min s1 s2) (max <$> l1 <*> l2)

-- 🪵 One piece after another: add them up.
-- 部品を順に並べるので、足し合わせる
branchWidth :: Branch -> Width
branchWidth (Branch ps) = foldr andThen (Width 0 (Just 0)) (map pieceWidth ps)
  where
    andThen (Width s1 l1) (Width s2 l2) = Width (s1 + s2) ((+) <$> l1 <*> l2)

-- 🧩 An anchor and \K take nothing. A quantified atom takes the atom's
--    width, times the count. A look's count is read as PCRE2 reads it
--    ((?=a)* is {0,1}: lookCount), so it has an upper end; (?:)* has
--    none, and PCRE2 does not take it in a lookbehind.
-- アンカーと \K は何も食べない。量指定子の付いたアトムは、アトムの幅に
-- 回数をかけたもの。「見る」ものの回数は PCRE2 と同じに読む（(?=a)* は
-- {0,1}。lookCount）ので上の端がある。(?:)* にはなく、PCRE2 も後読みに
-- 入れない
pieceWidth :: Piece -> Width
pieceWidth (Anchor _)       = Width 0 (Just 0)
pieceWidth KeepOut          = Width 0 (Just 0)
pieceWidth (Quantified a q) = Width (s * n) longest'
  where
    Quantifier (Count n more) _ = lookCount a q
    Width s l = atomWidth a
    longest' = case (l, more) of
      (Just w, Just e) -> Just (w * (n + e))
      _                -> Nothing

-- ⚛️ One character, a group's pattern, or nothing for a look.
--    A back reference can be any length (the parser does not let one
--    into a lookbehind).
-- 1文字か、グループの中のパターンか、見るだけなら0文字。後方参照は
-- どんな長さにもなる（パーサーが後読みには入れない）
atomWidth :: Atom -> Width
atomWidth a = case a of
  Group _ p   -> patternWidth p
  Atomic p    -> patternWidth p
  Look _ _ _  -> Width 0 (Just 0)
  Backref _   -> Width 0 Nothing
  NoCase x    -> atomWidth x
  _           -> Width 1 (Just 1)  -- Lit, AnyChar, Anything, Class
-- ANCHOR_END: width

-- ANCHOR: backref
-- 🔎 Is there a back reference anywhere in it?
-- どこかに後方参照があるか
hasBackref :: Branch -> Bool
hasBackref (Branch ps) = any inPiece ps
  where
    inPiece (Quantified a _) = inAtom a
    inPiece _                = False
    inAtom a = case a of
      Backref _  -> True
      NoCase x   -> inAtom x
      Group _ p  -> inPattern p
      Atomic p   -> inPattern p
      Look _ _ p -> inPattern p
      _          -> False
    inPattern (Pattern bs) = any hasBackref (NE.toList bs)
-- ANCHOR_END: backref
-- ANCHOR_END: all
