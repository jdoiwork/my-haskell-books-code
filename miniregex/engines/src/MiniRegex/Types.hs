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
  , Engine (..)
  , RegexError (..)
  , errorOffset
  , GaveUp (..)
  , defaultLimit
  ) where

-- ANCHOR: imports
import Control.Exception (Exception)
import Data.Map.Strict (Map)
import Data.Text (Text)

import MiniRegex.Prefilter (Prefilter)
import MiniRegex.Program (Program)
import MiniRegex.Syntax (Pattern)
-- ANCHOR_END: imports

-- ANCHOR: regex
-- 🧭 A compiled pattern: what was written, the flags, the tree, and
--    the program for the VM. Made only by `compile`, so a Regex is
--    always a good one.
-- コンパイルしたパターン：書いた文字列、フラグ、木、VM のプログラム。
-- compile だけが作るので、Regex はいつも正しいパターン
data Regex = Regex
  { source  :: Text
  , options :: [Option]
  , tree    :: Pattern
  , program :: Program
  -- which engine runs the program (from the linear-time chapter on)
  -- プログラムを動かすエンジン（線形時間の章から）
  , engine  :: Engine
  -- ANCHOR: prefilterfield
  -- what is known before the VM runs (from the chapter on looking
  -- ahead first)
  -- VM を動かす前にわかっていること（先に探す章から）
  , prefilter :: Prefilter
  -- ANCHOR_END: prefilterfield
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
-- 🚩 The flags, the same as PCRE2's i, m, s, x and a. They can be set
--    in the pattern, too: (?i), (?m), (?s), (?x), (?a).
-- フラグ。PCRE2 の i・m・s・x・a と同じ。パターンの中でも
-- (?i)・(?m)・(?s)・(?x)・(?a) で付けられる
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
  -- (?a) \d, \s, \w and [:alpha:] ... are ASCII only (MiniRegex's
  -- default is Unicode, as Python's and Rust's).
  -- \d・\s・\w・[:alpha:] などを ASCII だけにする（MiniRegex の
  -- 既定は、Python・Rust と同じく Unicode）
  | Ascii
  -- ANCHOR: linearopt
  -- Linear time only: a pattern the Pike VM cannot run is an error
  -- when it is compiled (MiniRegex's own; PCRE2 has no such flag).
  -- 線形時間だけ：Pike VM で動かせないパターンは、コンパイルのときに
  -- エラー（MiniRegex だけのフラグ。PCRE2 にはない）
  | Linear
  -- ANCHOR_END: linearopt
  deriving (Show, Eq, Ord, Enum, Bounded)
-- ANCHOR_END: option

-- ANCHOR: engine
-- 🚂 The engines. The backtracking VM can run every program; the Pike
--    VM runs only some, but takes time in proportion to the length of
--    the text. Switching (from the chapter on switching engines) runs
--    the backtracking VM first, and the Pike VM when it takes too long.
-- エンジン。バックトラックの VM は、どのプログラムも動かせる。Pike VM は、
-- 動かせるものが限られるが、時間が文字列の長さに比例する。乗り換え
-- （エンジンを乗り換える章から）は、まずバックトラックの VM を動かし、
-- 時間がかかりすぎたら Pike VM に乗り換える
data Engine
  = Backtracking
  | PikeVM
  | Switching
  deriving (Show, Eq)
-- ANCHOR_END: engine

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
  -- ANCHOR: notlinear
  -- With Linear: the Pike VM cannot run this instruction of the
  -- program (it says where in the program, not in the pattern).
  -- Linear のとき：Pike VM は、プログラムのこの命令を動かせない
  -- （パターンの場所ではなく、プログラムの命令で言う）
  | NotLinear   Text
  -- ANCHOR_END: notlinear
  deriving (Show, Eq, Ord)

-- 📍 Where the trouble is.
-- どこでつまずいたか
errorOffset :: RegexError -> Int
errorOffset (Invalid     at _) = at
errorOffset (Unsupported at _) = at
errorOffset (NotLinear _)     = 0
-- ANCHOR_END: error

-- ANCHOR: gaveup
-- 🏳️ The VM gave up. GaveUp n: it took n steps and had not finished
--    (PCRE2's error -47, "match limit exceeded"). Looping g p: in a call
--    of group g at place p, it called g at p again, and would go round
--    for ever (PCRE2's error -52, "nested recursion at the same subject
--    position"). From the chapter on recursion.
-- VM があきらめた。GaveUp n：n 手を使っても、終わらなかった（PCRE2 の
-- error -47「match limit exceeded」）。Looping g p：場所 p で g を
-- 呼んだ中で、また p で g を呼んだ。いつまでも回り続ける（PCRE2 の
-- error -52「nested recursion at the same subject position」）。
-- 再帰の章から
data GaveUp
  = GaveUp Int
  | Looping Int Int
  deriving (Show, Eq)

instance Exception GaveUp

-- 🔟 The steps a search may take, unless told otherwise: ten million,
--    the same number as PCRE2's default match limit.
-- ことわりがなければ、1回の探しものに使ってよい手数：1000万。PCRE2 の
-- 手数の上限の既定値と同じ数
defaultLimit :: Int
defaultLimit = 10_000_000
-- ANCHOR_END: gaveup
-- ANCHOR_END: all
