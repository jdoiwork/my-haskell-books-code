-- ANCHOR: all
-- ANCHOR: header
-- | MiniRegex: regular expressions that are easy to use from Haskell.
--   Each function has one kind of answer. The engine inside is
--   MiniRegex.Engine; every function here only calls it.
-- MiniRegex：Haskell から使いやすい正規表現。どの関数も、答えの型は
-- 1つだけ。中身のエンジンは MiniRegex.Engine で、ここの関数は、
-- それを呼ぶだけ
module MiniRegex
  ( -- * Making a regex / 作る
    Regex
  , source
  , compile
  , compileWith
  , Option (..)
  , RegexError (..)
  , errorOffset
  , explain
    -- * Finding / 探す
  , Match (..)
  , isMatch
  , find
  , findAll
    -- * Replacing and splitting / 置き換える・切る
  , replace
  , replaceAll
  , replaceWith
  , split
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Engine (compilePattern, searchFrom)
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Make a regex from a pattern. A wrong pattern is told here, not
--    when it is used.
-- パターンから正規表現を作る。まちがったパターンは、使うときではなく、
-- ここでわかる
compile :: Text -> Either RegexError Regex
compile = compileWith []

-- 🚩 The same, with flags: compileWith [Caseless, Multiline] "^name:".
-- フラグつき：compileWith [Caseless, Multiline] "^name:"
compileWith :: [Option] -> Text -> Either RegexError Regex
compileWith opts src = do
  tree <- compilePattern opts src
  pure (Regex src opts tree)
-- ANCHOR_END: compile

-- ANCHOR: explain
-- 👉 The pattern, a ^ under the place of the trouble, and what it is.
--    The place is where PCRE2 says it is: often just after the
--    character that was wrong.
-- パターンと、つまずいた場所を指す ^ と、そのわけ。場所は PCRE2 が言う
-- 場所（まちがった文字の、すぐうしろのことが多い）
explain :: Text -> RegexError -> Text
explain src err = T.unlines [src, caret, why]
  where
    caret = T.replicate (width (T.take (errorOffset err) src)) " " <> "^"
    why = case err of
      Invalid at m     -> "wrong pattern at " <> place at <> ": " <> m
      Unsupported at w -> "not yet at " <> place at <> ": " <> w
    place = T.pack . show

-- 📐 How many columns the text takes on a terminal. A Japanese or
--    Chinese character takes two (a rough rule, good enough here).
-- 文字列が端末で何桁を占めるか。日本語や中国語の文字は2桁
-- （おおまかな決まりだが、ここでは十分）
width :: Text -> Int
width = sum . map columns . T.unpack
  where
    columns c
      | wide (ord c) = 2
      | otherwise    = 1
    wide n = any (\(lo, hi) -> lo <= n && n <= hi)
      [ (0x1100, 0x115F), (0x2E80, 0xA4CF), (0xAC00, 0xD7A3)
      , (0xF900, 0xFAFF), (0xFF00, 0xFF60), (0xFFE0, 0xFFE6)
      , (0x1F300, 0x1FAFF), (0x20000, 0x3FFFD) ]
-- ANCHOR_END: explain

-- ANCHOR: find
-- ✅ Is it anywhere in the text?
-- 文字列のどこかにあるか
isMatch :: Regex -> Text -> Bool
isMatch re t = isJust (find re t)

-- 🔍 The first match.
-- 最初に見つかったもの
find :: Regex -> Text -> Maybe Match
find re t = searchFrom re t 0

-- 🔎 All the matches, from left to right, not overlapping. After an
--    empty match, look again one character later (or it would find the
--    same empty match for ever).
-- 見つかったもの全部。左から順に、重ならないように。空の一致のあとは、
-- 1文字先から探し直す（でないと、同じ空の一致を見つけ続ける）
findAll :: Regex -> Text -> [Match]
findAll re t = go 0
  where
    go from = case searchFrom re t from of
      Nothing -> []
      Just m  -> m : go (next m)
    next m
      | matchStart m == matchEnd m = matchEnd m + 1
      | otherwise                  = matchEnd m
-- ANCHOR_END: find

-- ANCHOR: replace
-- ✏️ Replace the first match with the text, as it is ($1 is just "$1").
-- 最初に見つかったものを、その文字列に置き換える
-- （$1 も、ただの "$1"）
replace :: Regex -> Text -> Text -> Text
replace re new t = rebuild (const new) (take 1 (findAll re t)) t

-- 🖍️ Replace every match.
-- 見つかったもの全部を置き換える
replaceAll :: Regex -> Text -> Text -> Text
replaceAll re new = replaceWith re (const new)

-- 🪄 Replace every match with what the function makes of it.
-- 見つかったもの全部を、関数が作った文字列に置き換える
replaceWith :: Regex -> (Match -> Text) -> Text -> Text
replaceWith re f t = rebuild f (findAll re t) t

-- 🧵 Put the text back together, with each match made anew by f.
-- 見つかったものを f で作り直しながら、文字列を組み立て直す
rebuild :: (Match -> Text) -> [Match] -> Text -> Text
rebuild f matches t = T.concat (go 0 matches)
  where
    go at []       = [T.drop at t]
    go at (m : ms) = slice at (matchStart m) t : f m : go (matchEnd m) ms
-- ANCHOR_END: replace

-- ANCHOR: split
-- ✂️ The pieces between the matches.
-- 見つかったもののあいだの切れ端
split :: Regex -> Text -> [Text]
split re t = go 0 (findAll re t)
  where
    go at []       = [T.drop at t]
    go at (m : ms) = slice at (matchStart m) t : go (matchEnd m) ms

-- 🍰 The characters from `from` up to `to` (not including `to`).
-- from 文字目から、to 文字目の手前まで
slice :: Int -> Int -> Text -> Text
slice from to = T.take (to - from) . T.drop from
-- ANCHOR_END: split
-- ANCHOR_END: all
