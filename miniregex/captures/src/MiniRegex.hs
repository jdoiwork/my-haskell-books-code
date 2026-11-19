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
  , Span (..)
  , isMatch
  , find
  , findAll
    -- * Groups / グループ
  , group
  , named
  , groups
  , groupSpan
  , namedSpan
    -- * Replacing and splitting / 置き換える・切る
  , replace
  , replaceAll
  , replaceWith
  , Template
  , template
  , expand
  , replaceTemplate
  , replaceAllTemplate
  , split
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Char (ord)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Engine (compilePattern, nonEmptyAt, searchFrom)
import MiniRegex.Template (Template, expand, template)
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

-- ANCHOR_END: find

-- ANCHOR: findall
-- 🔎 All the matches, from left to right, not overlapping. After an
--    empty match, first try for a match that is not empty, at the same
--    place; only if there is none, go one character on. (PCRE2's rule,
--    the same as pcre2test's /g, Perl and Python.)
-- 見つかったもの全部。左から順に、重ならないように。空の一致のあとは、
-- まず同じ場所で、空でない一致を探す。それがなければ、1文字先へ進む
-- （PCRE2 の決まり。pcre2test の /g、Perl、Python と同じ）
findAll :: Regex -> Text -> [Match]
findAll re t = go 0
  where
    go from = case searchFrom re t from of
      Nothing -> []
      Just m
        | matchStart m == matchEnd m -> m : afterEmpty (matchEnd m)
        | otherwise                  -> m : go (matchEnd m)
    afterEmpty here = case nonEmptyAt re t here of
      Just m                     -> m : go (matchEnd m)
      Nothing
        | here < T.length t      -> go (here + 1)
        | otherwise              -> []
-- ANCHOR_END: findall

-- ANCHOR: groups
-- 🧺 What group n found (0: the whole match; Nothing for a group that
--    took no part, or that is not there).
-- n 番のグループが見つけたもの（0 は一致の全体。使われなかったグループと、
-- ないグループは Nothing）
group :: Int -> Match -> Maybe Text
group n m = spanText <$> groupSpan n m

-- 📛 What the group with this name found.
-- この名前のグループが見つけたもの
named :: Text -> Match -> Maybe Text
named name m = spanText <$> namedSpan name m

-- 📋 What each group found, group 1 first.
-- グループごとに見つけたもの。1番から
groups :: Match -> [Maybe Text]
groups m = map (fmap spanText) (matchGroups m)

-- 📏 The same, with where it was.
-- 同じものを、場所つきで
groupSpan :: Int -> Match -> Maybe Span
groupSpan 0 m = Just (Span (matchText m) (matchStart m) (matchEnd m))
groupSpan n m = case drop (n - 1) (matchGroups m) of
  g : _ | n > 0 -> g
  _             -> Nothing

namedSpan :: Text -> Match -> Maybe Span
namedSpan name m = do
  n <- Map.lookup name (matchNames m)
  groupSpan n m
-- ANCHOR_END: groups

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

-- ANCHOR_END: replace

-- ANCHOR: template
-- 🧾 Replace the first match with the template ($1, ${name}, $$).
-- 最初に見つかったものを、テンプレート（$1・${name}・$$）で置き換える
replaceTemplate :: Regex -> Template -> Text -> Text
replaceTemplate re tpl t = rebuild (expand tpl) (take 1 (findAll re t)) t

-- 🧾 Replace every match with the template.
-- 見つかったもの全部を、テンプレートで置き換える
replaceAllTemplate :: Regex -> Template -> Text -> Text
replaceAllTemplate re tpl = replaceWith re (expand tpl)
-- ANCHOR_END: template

-- ANCHOR: rebuild
-- 🧵 Put the text back together, with each match made anew by f.
-- 見つかったものを f で作り直しながら、文字列を組み立て直す
rebuild :: (Match -> Text) -> [Match] -> Text -> Text
rebuild f matches t = T.concat (go 0 matches)
  where
    go at []       = [T.drop at t]
    go at (m : ms) = slice at (matchStart m) t : f m : go (matchEnd m) ms
-- ANCHOR_END: rebuild

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
