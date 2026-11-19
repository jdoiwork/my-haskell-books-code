-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on looking first.
-- トレーニングの解答：設計の章のものと、先に探す章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , minLength
  , kmpPlacesBy
  ) where

import Data.Array (Array, listArray, (!))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Cursor (Cursor (..), onwards)
import MiniRegex.Syntax
-- ANCHOR_END: head

-- The design chapter's training
-- 設計の章のトレーニング

-- 🌈 colou?r: c, o, l, o, then u zero or one time, then r.
-- colou?r：c・o・l・o、u を 0 回か 1 回、そして r
colourTree :: Pattern
colourTree = Pattern (Branch pieces :| [])
  where
    pieces =
      [ Quantified (Lit 'c') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'l') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'u') optional
      , Quantified (Lit 'r') once
      ]

-- 🍀 (?:ab)+|c: two branches. The left one is a group that does not
--    keep what it found, one time or more.
-- (?:ab)+|c：枝が2本。左は、見つけたものを覚えないグループを1回以上
choiceTree :: Pattern
choiceTree = Pattern (left :| [right])
  where
    left  = Branch [Quantified (Group NonCapturing ab) plus]
    ab    = Pattern (Branch [ Quantified (Lit 'a') once
                            , Quantified (Lit 'b') once ] :| [])
    right = Branch [Quantified (Lit 'c') once]

-- 🛡️ Put \ before every character with a meaning in a pattern, so
--    that the pattern finds the text just as it is.
-- パターンの中で意味を持つ文字の前に \ を付けて、その文字列そのものを
-- 探すパターンにする
escape :: Text -> Text
escape = T.concatMap one
  where
    one c
      | c `elem` special = T.pack ['\\', c]
      | otherwise        = T.singleton c
    special = "\\^$.|?*+()[]{}" :: String

-- The training of the chapter on looking first
-- 先に探す章のトレーニング

-- ANCHOR: minlength
-- 📏 The fewest characters any match takes, as PCRE2's "Subject length
--    lower bound". Walk the tree: a character is one, a branch the
--    sum of its pieces, a choice the smallest of its branches, and a
--    repeat its least count times its atom. A look takes nothing (it
--    goes back), and where we cannot tell (\1, a call), say 0: it may
--    be too small, but never too big.
-- どの一致も食べる、いちばん少ない文字の数。PCRE2 の「Subject length
-- lower bound」。木を歩く：文字は 1、枝は部品の合計、選択は枝のいちばん
-- 小さいもの、くり返しは「いちばん少ない回数 × アトム」。「見る」ものは
-- 何も食べない（戻るから）。わからない所（\1・呼び出し）は 0 とする。
-- 小さすぎることはあっても、大きすぎることはない
minLength :: Pattern -> Int
minLength (Pattern bs) = minimum (fmap branch bs)
  where
    branch (Branch ps) = sum (map piece ps)
    piece p = case p of
      Quantified a q -> fromIntegral (atLeast (count q)) * atom a
      Anchor _       -> 0
      KeepOut        -> 0
    atom a = case a of
      Group _ p   -> minLength p
      Atomic p    -> minLength p
      NoCase a'   -> atom a'
      Look {}     -> 0
      Backref _   -> 0
      Call _      -> 0
      _           -> 1  -- one character / 1文字
-- ANCHOR_END: minlength

-- ANCHOR: kmpby
-- 🧮 KMP with your own "same": the places where t starts, comparing
--    the characters with `same` (sameCase: a and A are the same). Text's
--    search compares bytes, so it cannot do this; KMP only needs a way
--    to say "the same", if it is an equivalence.
-- 自分で決めた「同じ」で KMP：t が始まる場所を、文字を same で
-- くらべながら（sameCase なら a と A は同じ）。Text の検索はバイトを
-- くらべるので、これはできない。KMP は「同じ」の決め方さえあればよい
-- （同値関係なら）
kmpPlacesBy :: (Char -> Char -> Bool) -> Text -> Cursor -> [Cursor]
kmpPlacesBy same t c =
  [ d | (d, k) <- zip (onwards c) (drop (m - 1) states), k == m ]
  where
    m     = T.length t
    p     = listArray (0, m - 1) (T.unpack t) :: Array Int Char
    table = listArray (0, m - 1)
              (0 : [border i (table ! (i - 1)) | i <- [1 .. m - 1]])
              :: Array Int Int
    border i k
      | same (p ! i) (p ! k) = k + 1
      | k == 0               = 0
      | otherwise            = border i (table ! (k - 1))
    states = drop 1 (scanl stepK 0 (T.unpack (rest c)))
    stepK k x
      | k == m          = stepK (table ! (m - 1)) x
      | same (p ! k) x  = k + 1
      | k == 0          = 0
      | otherwise       = stepK (table ! (k - 1)) x
-- ANCHOR_END: kmpby
-- ANCHOR_END: all
