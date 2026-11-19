-- ANCHOR: all
-- | The types the user sees: a compiled pattern, what was found,
--   the flags, and what can go wrong.
-- 使う人が目にする型：コンパイルしたパターン、見つけたもの、
-- フラグ、うまくいかなかったとき
module MiniRegex.Types
  ( Regex (..)
  , Match (..)
  , Span (..)
  , Option (..)
  , RegexError (..)
  , errorOffset
  ) where

-- ANCHOR: imports
import Data.Map.Strict (Map)
import Data.Text (Text)

import MiniRegex.Syntax (Pattern)
-- ANCHOR_END: imports

-- ANCHOR: regex
-- 🧭 A compiled pattern: what was written, the flags, and the tree.
--    Made only by `compile`, so a Regex is always a good one.
-- コンパイルしたパターン：書いた文字列、フラグ、木。
-- compile だけが作るので、Regex はいつも正しいパターン
data Regex = Regex
  { source  :: Text
  , options :: [Option]
  , tree    :: Pattern
  }

-- Show what was written, not the whole tree.
-- 木の全体ではなく、書いたパターンを見せる
instance Show Regex where
  show r = "<regex " ++ show (source r) ++ ">"
-- ANCHOR_END: regex

-- ANCHOR: match
-- 🔍 What was found: the text, and where it was (counted in
--    characters, from 0; the end is not included). Then what each
--    group found (group 1 first; Nothing for a group that took no
--    part), and the numbers of the named groups.
-- 見つけたもの：その文字列と、場所（0 から数えた文字の位置。
-- 終わりの位置の文字は含まない）。そのあとに、グループごとに見つけた
-- もの（1番から。使われなかったグループは Nothing）と、名前付きの
-- グループの番号
data Match = Match
  { matchText   :: Text
  , matchStart  :: Int
  , matchEnd    :: Int
  , matchGroups :: [Maybe Span]
  , matchNames  :: Map Text Int
  }
  deriving (Show, Eq)

-- 📏 What one group found, and where.
-- グループ1つが見つけたものと、その場所
data Span = Span
  { spanText  :: Text
  , spanStart :: Int
  , spanEnd   :: Int
  }
  deriving (Show, Eq)
-- ANCHOR_END: match

-- ANCHOR: option
-- 🚩 The flags, the same as PCRE2's i, m, s and x.
-- フラグ。PCRE2 の i・m・s・x と同じ
data Option
  -- (?i) A and a are the same.
  -- 大文字と小文字を区別しない
  = Caseless
  -- (?m) ^ and $ match at the start and end of each line.
  -- ^ と $ が、行ごとの頭と終わりに合う
  | Multiline
  -- (?s) . matches a newline, too.
  -- . が改行にも合う
  | DotAll
  -- (?x) Spaces and # comments in the pattern are skipped.
  -- パターンの中の空白と # のコメントを読み飛ばす
  | Extended
  deriving (Show, Eq, Ord, Enum, Bounded)
-- ANCHOR_END: option

-- ANCHOR: error
-- 🚧 A pattern that cannot be compiled, and where (the character
--    position in the pattern). A wrong pattern, and a pattern that is
--    fine but that MiniRegex cannot run yet, are told apart.
-- コンパイルできなかったパターンと、その場所（パターンの何文字目か）。
-- まちがったパターンと、正しいけれど MiniRegex がまだ扱えないパターンを
-- 分けておく
data RegexError
  = Invalid     Int Text
  | Unsupported Int Text
  deriving (Show, Eq, Ord)

-- 📍 Where the trouble is.
-- どこでつまずいたか
errorOffset :: RegexError -> Int
errorOffset (Invalid     at _) = at
errorOffset (Unsupported at _) = at
-- ANCHOR_END: error
-- ANCHOR_END: all
