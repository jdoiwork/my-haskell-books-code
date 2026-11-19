-- ANCHOR: all
-- | The tree of a pattern. Shapes that PCRE2 refuses (a quantifier
--   with nothing before it, a repeated ^, {5,2}) cannot be written.
-- パターンの木。PCRE2 が断る形（前に何もない量指定子、くり返した ^、
-- {5,2}）は、書けないようにしてある
module MiniRegex.Syntax
  ( Pattern (..)
  , Branch (..)
  , Piece (..)
  , Atom (..)
  , Capture (..)
  , Anchor (..)
  , CharClass (..)
  , ClassItem (..)
  , Quantifier (..)
  , Count (..)
  , Greed (..)
  , once
  , star
  , plus
  , optional
  , between
  , literal
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric.Natural (Natural)
-- ANCHOR_END: imports

-- ANCHOR: pattern
-- 🌳 A whole pattern: branches split by |. There is always at least
--    one (a pattern with no | is one branch).
-- パターン全体：| で分かれた枝。いつも1本以上ある
-- （| のないパターンは、枝が1本）
newtype Pattern = Pattern (NonEmpty Branch)
  deriving (Show, Eq)

-- 🪵 A branch: pieces from left to right. It may be empty
--    (the right of "a|").
-- 枝：左から並べた部品。空でもよい（"a|" の右）
newtype Branch = Branch [Piece]
  deriving (Show, Eq)

-- 🧩 A piece: something that can be repeated, with how many times,
--    or an anchor, which cannot be repeated.
-- 部品：くり返せるものと、その回数。または、くり返せないアンカー
data Piece
  = Quantified Atom Quantifier
  | Anchor Anchor
  deriving (Show, Eq)
-- ANCHOR_END: pattern

-- ANCHOR: atom
-- ⚛️ The things that can be repeated.
-- くり返せるもの
data Atom
  -- a, b, 日 ...: the character itself.
  -- その文字
  = Lit Char
  -- . : any one character but a newline.
  -- 改行のほかの、どの1文字でも
  | AnyChar
  -- [a-z], [^0-9]: one of a set of characters.
  -- 文字の集まりのどれか1文字
  | Class CharClass
  -- ( ... ), (?<name> ... ), (?: ... ): a pattern in a pattern.
  -- パターンの中のパターン
  | Group Capture Pattern
  deriving (Show, Eq)

-- 🏷️ What a group keeps.
-- グループが、見つけたものを覚えておくか
data Capture
  = Numbered      -- ( ... )
  | Named Text    -- (?<name> ... )
  | NonCapturing  -- (?: ... )
  deriving (Show, Eq)

-- ⚓ The places that take no character.
-- 文字を食べない、場所の印
data Anchor
  = LineStart  -- ^
  | LineEnd    -- $
  deriving (Show, Eq)

-- 🔤 [ ... ] has at least one item; [^ ... ] is the negated one.
-- [ ... ] には、いつも1つ以上の項目がある。[^ ... ] は、その逆
data CharClass = CharClass
  { negated :: Bool
  , items   :: NonEmpty ClassItem
  }
  deriving (Show, Eq)

data ClassItem
  = Single Char       -- a
  | Range Char Char   -- a-z
  deriving (Show, Eq)
-- ANCHOR_END: atom

-- ANCHOR: quantifier
-- 🔁 How many times, and how hungry.
-- 何回くり返すかと、どれだけ欲ばるか
data Quantifier = Quantifier
  { count :: Count
  , greed :: Greed
  }
  deriving (Show, Eq)

-- 🔢 At least `atLeast` times, and then up to `extra` more times
--    (Nothing: as many as there are). The upper end is never below
--    the lower one, and neither is negative.
-- 少なくとも atLeast 回、そこから、あと extra 回まで（Nothing は
-- いくらでも）。上の端が下の端を下回ることも、負になることもない
data Count = Count
  { atLeast :: Natural
  , extra   :: Maybe Natural
  }
  deriving (Show, Eq)

-- 🍽️ a* (as many as it can), a*? (as few as it can),
--    a*+ (as many as it can, and never gives back).
-- a*（できるだけ多く）、a*?（できるだけ少なく）、
-- a*+（できるだけ多く、一度取ったら返さない）
data Greed = Greedy | Lazy | Possessive
  deriving (Show, Eq)
-- ANCHOR_END: quantifier

-- ANCHOR: shorthands
-- 🧰 The usual counts, greedy.
-- よく使う回数（どれも欲ばり）
once, star, plus, optional :: Quantifier
once     = Quantifier (Count 1 (Just 0)) Greedy  -- a
star     = Quantifier (Count 0 Nothing)  Greedy  -- a*
plus     = Quantifier (Count 1 Nothing)  Greedy  -- a+
optional = Quantifier (Count 0 (Just 1)) Greedy  -- a?

-- 📏 {n,m}. Nothing when m is smaller than n, as PCRE2 refuses it.
-- {n,m}。m が n より小さいときは、PCRE2 と同じく作らない
between :: Natural -> Natural -> Maybe Quantifier
between n m
  | m < n     = Nothing
  | otherwise = Just (Quantifier (Count n (Just (m - n))) Greedy)

-- 📝 A pattern that is just these characters, one after another.
-- この文字を順に並べただけのパターン
literal :: Text -> Pattern
literal t = Pattern (Branch pieces :| [])
  where
    pieces = [Quantified (Lit c) once | c <- T.unpack t]
-- ANCHOR_END: shorthands
-- ANCHOR_END: all
