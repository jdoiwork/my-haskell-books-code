-- ANCHOR: all
-- | The small rules that both matchers share: does one character fit
--   an atom, and does a place fit an anchor. The flags are in the tree
--   now, so the rules no longer need them.
-- 2つのマッチャーが使う、小さな決まり：1文字がアトムに合うか、場所が
-- アンカーに合うか。フラグは木の中に入ったので、決まりにはもう要らない
module MiniRegex.Rules
  ( fitsChar
  , inClass
  , holds
  , eatAll
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import MiniRegex.Cursor
import MiniRegex.Syntax
import MiniRegex.Unicode (caseVariants, hasType, sameCase)
-- ANCHOR_END: imports

-- ANCHOR: char
-- 🔤 Does this character fit an atom of one character? (A group, a
--    back reference, an atomic group and a look are not one character;
--    the matchers see to them themselves.)
-- この1文字は、1文字のアトムに合うか（グループ・後方参照・アトミック
-- グループ・「見る」ものは1文字ではないので、マッチャーが自分で見る）
fitsChar :: Atom -> Char -> Bool
fitsChar (Lit x)              c = c == x
fitsChar AnyChar              c = c /= '\n'
fitsChar Anything             _ = True
fitsChar (Class cls)          c = inClass False cls c
fitsChar (NoCase (Lit x))     c = sameCase x c
fitsChar (NoCase (Class cls)) c = inClass True cls c
fitsChar (NoCase a)           c = fitsChar a c
fitsChar (Group _ _)          _ = False
fitsChar (Backref _)          _ = False
fitsChar (Atomic _)           _ = False
fitsChar (Look _ _ _)         _ = False
fitsChar (Call _)             _ = False
-- ANCHOR_END: char

-- ANCHOR: eatall
-- 🍽️ For takeall: go past every character that fits the atom, one
--    step each, with n steps left. Gives back where it stopped, and
--    the steps left (0 or less: they ran out on the way).
-- takeall のため：アトムに合う文字を全部、1文字1手で通りすぎる。残りの
-- 手数は n。止まった場所と、残りの手数を返す（0 以下なら、途中で尽きた）
eatAll :: Atom -> Int -> Cursor -> (Cursor, Int)
eatAll a = go
  where
    go n c
      | n <= 0    = (c, n)
      | otherwise = case step c of
          Just (x, c') | fitsChar a x -> go (n - 1) c'
          _                           -> (c, n)
-- ANCHOR_END: eatall

-- ANCHOR: class
-- 🔤 Is the character in the class? Without case, a or A is in [a-z]
--    when any of its other cases is in it. \d, [:alpha:] and \p{...}
--    look at the character itself, as PCRE2 does.
-- 文字は、文字の集まりに入っているか。大文字と小文字を区別しないなら、
-- ほかの形のどれかが入っていれば入っている。\d・[:alpha:]・\p{...} は、
-- PCRE2 と同じく、その文字そのものを見る
inClass :: Bool -> CharClass -> Char -> Bool
inClass noCase (CharClass neg is) c = neg /= any has (NE.toList is)
  where
    forms = if noCase then caseVariants c else [c]
    has (Single x)    = x `elem` forms
    has (Range a b)   = any (\f -> a <= f && f <= b) forms
    has (Builtin o t) = o /= hasType t c
-- ANCHOR_END: class

-- ANCHOR: anchor
-- ⚓ Does the place fit the anchor? ^ and $ with m fit at every line
--    (but ^ not after a newline that ends the text). Without m, the
--    parser has made them \A and \Z. \b fits where the characters on
--    the two sides are not both word characters, or both not.
-- 場所がアンカーに合うか。m のある ^ と $ は、行ごとに合う（ただし ^ は、
-- 文字列を終える改行のうしろには合わない）。m がなければ、パーサーが
-- \A と \Z にしてある。\b は、両側の文字の「単語の文字か」が、
-- ちがう所に合う
holds :: Anchor -> Cursor -> Bool
holds LineStart c =
  at c == 0 || (before c == Just '\n' && not (T.null (rest c)))
holds LineEnd     c = T.null (rest c) || "\n" `T.isPrefixOf` rest c
holds TextStart   c = at c == 0
holds TextEnd     c = T.null (rest c)
holds LastLineEnd c = T.null (rest c) || rest c == "\n"
holds (WordBoundary r)    c = wordBefore r c /= wordAfter r c
holds (NotWordBoundary r) c = wordBefore r c == wordAfter r c

-- 🔤 Is the character just before (or just after) the cursor a word
--    character, as \w says? Before the start and after the end, no.
-- しおりのすぐ前（すぐうしろ）の文字は、\w の言う単語の文字か。
-- 頭より前と、終わりより先は「いいえ」
wordBefore, wordAfter :: Reach -> Cursor -> Bool
wordBefore r c = maybe False (hasType (Word r)) (before c)
wordAfter  r c = maybe False (hasType (Word r) . fst) (T.uncons (rest c))
-- ANCHOR_END: anchor
-- ANCHOR_END: all
