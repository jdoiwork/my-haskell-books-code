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
  , Direction (..)
  , Sense (..)
  , Anchor (..)
  , CharClass (..)
  , ClassItem (..)
  , CharType (..)
  , Reach (..)
  , Property (..)
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
import Unicode.Char.General.Scripts (Script)
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
--    an anchor, which cannot be repeated, or \K.
-- 部品：くり返せるものと、その回数。または、くり返せないアンカーか \K
data Piece
  = Quantified Atom Quantifier
  | Anchor Anchor
  -- ANCHOR: keepout
  -- \K: the match that is told starts here (what is before it was
  -- needed, but is left out).
  -- \K：知らせる一致は、ここから始まる（手前は要るが、外す）
  | KeepOut
  -- ANCHOR_END: keepout
  deriving (Show, Eq)
-- ANCHOR_END: pattern

-- ANCHOR: atom
-- ⚛️ The things that can be repeated. The flags are already in the
--    tree: the parser writes down, for each atom, what (?i) and (?s)
--    said at its place.
-- くり返せるもの。フラグは、もう木の中にある。パーサーが、アトムごとに、
-- その場所で (?i) と (?s) が何と言っていたかを書き込む
data Atom
  -- a, b, 日 ...: the character itself.
  -- その文字
  = Lit Char
  -- . (without s) and \N: any one character but a newline.
  -- .（s なし）と \N：改行のほかの、どの1文字でも
  | AnyChar
  -- . with s: any one character, a newline too.
  -- s のもとの .：どの1文字でも（改行も）
  | Anything
  -- [a-z], [^0-9], \d, \p{Han}: one of a set of characters.
  -- 文字の集まりのどれか1文字
  | Class CharClass
  -- ( ... ), (?<name> ... ), (?: ... ): a pattern in a pattern.
  -- パターンの中のパターン
  | Group Capture Pattern
  -- \1, \k<name> ...: the same text that group 1 found (names are
  -- turned into numbers when the pattern is read).
  -- 1番のグループが見つけたのと同じ文字列（名前は、パターンを読む
  -- ときに番号にする）
  | Backref Int
  -- (?i): a Lit, a Class or a Backref that does not tell A from a.
  -- (?i)：A と a を区別しない Lit・Class・Backref
  | NoCase Atom
  -- ANCHOR: look
  -- (?>...): the first way the inside works is the only one; it is
  -- never tried again (a*+ is (?>a*)).
  -- (?>...)：中がうまくいった最初のやり方だけ。やり直さない
  -- （a*+ は (?>a*)）
  | Atomic Pattern
  -- (?=...) (?!...) (?<=...) (?<!...): look ahead or behind, without
  -- taking a character.
  -- (?=...) (?!...) (?<=...) (?<!...)：前か後ろを見る。文字は食べない
  | Look Direction Sense Pattern
  deriving (Show, Eq)

-- 👀 Which way to look, and whether it should be there or not.
-- どちらを見るか。そこにあってほしいか、なくてほしいか
data Direction = Ahead | Behind
  deriving (Show, Eq)

data Sense = Positive | Negative
  deriving (Show, Eq)
  -- ANCHOR_END: look

-- 🏷️ What a group keeps.
-- グループが、見つけたものを覚えておくか
data Capture
  = Numbered      -- ( ... )
  | Named Text    -- (?<name> ... )
  | NonCapturing  -- (?: ... )
  | BranchReset   -- (?| ... ): each branch numbers from the same place
  deriving (Show, Eq)

-- ⚓ The places that take no character.
-- 文字を食べない、場所の印
data Anchor
  = LineStart    -- ^ with m: the start of any line
  | LineEnd      -- $ with m: the end of any line
  | TextStart    -- \A, and ^ without m
  | TextEnd      -- \z
  | LastLineEnd  -- \Z, and $ without m: the end, or before a last \n
  -- ANCHOR: boundary
  -- \b and \B: between a word character and a non-word one (or not).
  -- What a word character is, is \w's: so they have a Reach, too.
  -- \b と \B：単語の文字と、そうでない文字のあいだ（か、そうでない所）。
  -- 単語の文字は \w が決めるので、Reach を持つ
  | WordBoundary Reach
  | NotWordBoundary Reach
  -- ANCHOR_END: boundary
  deriving (Show, Eq)

-- 🔤 [ ... ] has at least one item; [^ ... ] is the negated one.
-- [ ... ] には、いつも1つ以上の項目がある。[^ ... ] は、その逆
data CharClass = CharClass
  { negated :: Bool
  , items   :: NonEmpty ClassItem
  }
  deriving (Show, Eq)

data ClassItem
  = Single Char        -- a
  | Range Char Char    -- a-z
  -- \d, [:alpha:], \p{L}; True for the opposite (\D, [:^alpha:], \P{L})
  -- \d・[:alpha:]・\p{L}。True なら、その逆（\D・[:^alpha:]・\P{L}）
  | Builtin Bool CharType
  deriving (Show, Eq)
-- ANCHOR_END: atom

-- ANCHOR: chartype
-- 🧺 The sets of characters that have a name.
-- 名前の付いた、文字の集まり
data CharType
  = Digit Reach        -- \d
  | WhiteSpace Reach   -- \s
  | Word Reach         -- \w
  | HSpace             -- \h: the same list, with or without Unicode
  | VSpace             -- \v
  | Posix Reach Text   -- [:alpha:], [:digit:] ... (the name)
  | Property Property  -- \p{...}
  deriving (Show, Eq)

-- 🌏 How far a name reaches: only ASCII, or all of Unicode.
-- 名前がどこまで届くか：ASCII だけか、Unicode の全部か
data Reach = AsciiOnly | AllUnicode
  deriving (Show, Eq)

-- 🏷️ \p{...}: a general category (L, Lu, ...), a script, or Any.
-- \p{...}：一般カテゴリー（L・Lu など）、文字体系、Any
data Property
  = Category Text      -- \p{L}, \p{Lu}, \p{Lc}
  | Script Script      -- \p{sc:Han}: the script of the character
  | ScriptExt Script   -- \p{Han}: the script, or one it is used with
  | AnyCode            -- \p{Any}
  | Special Text       -- \p{Xan}, \p{Xps}, \p{Xsp}, \p{Xwd}
  deriving (Show, Eq)
-- ANCHOR_END: chartype

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
