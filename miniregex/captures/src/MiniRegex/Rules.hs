-- ANCHOR: all
-- | The small rules that both matchers share: does one character fit
--   an atom, and does a place fit an anchor. The flags m and s change
--   them.
-- 2つのマッチャーが使う、小さな決まり：1文字がアトムに合うか、場所が
-- アンカーに合うか。フラグ m と s で変わる
module MiniRegex.Rules
  ( fitsChar
  , holds
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import MiniRegex.Cursor
import MiniRegex.Syntax
import MiniRegex.Types (Option (..))
-- ANCHOR_END: imports

-- ANCHOR: char
-- 🔤 Does this character fit an atom of one character? (A group and a
--    back reference are not one character; the matchers see to them
--    themselves.)
-- この1文字は、1文字のアトムに合うか（グループと後方参照は1文字では
-- ないので、マッチャーが自分で見る）
fitsChar :: [Option] -> Atom -> Char -> Bool
fitsChar _    (Lit x)     c = c == x
fitsChar opts AnyChar     c = DotAll `elem` opts || c /= '\n'
fitsChar _    (Class cls) c = negated cls /= any has (NE.toList (items cls))
  where
    has (Single x)  = c == x
    has (Range a b) = a <= c && c <= b
fitsChar _    (Group _ _) _ = False
fitsChar _    (Backref _) _ = False
-- ANCHOR_END: char

-- ANCHOR: anchor
-- ⚓ Does the place fit the anchor? Without m, ^ is only the start of
--    the text, and $ is the end or just before a newline at the end.
--    With m, they fit at every line (but ^ not after a newline that
--    ends the text).
-- 場所がアンカーに合うか。m なしなら、^ は文字列の頭だけ、$ は終わりか、
-- 最後の改行のすぐ前。m があれば、行ごとに合う（ただし ^ は、文字列を
-- 終える改行のうしろには合わない）
holds :: [Option] -> Anchor -> Cursor -> Bool
holds opts LineStart c
  | Multiline `elem` opts =
      at c == 0 || (before c == Just '\n' && not (T.null (rest c)))
  | otherwise = at c == 0
holds opts LineEnd c
  | Multiline `elem` opts = T.null (rest c) || "\n" `T.isPrefixOf` rest c
  | otherwise             = T.null (rest c) || rest c == "\n"
-- ANCHOR_END: anchor
-- ANCHOR_END: all
