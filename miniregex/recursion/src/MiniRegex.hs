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
    -- * With a limit on the steps / ステップ数の上限つきで
  , GaveUp (..)
  , defaultLimit
  , findWithin
  , findAllWithin
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
    -- * Places in bytes / バイトで数えた場所
  , matchSpan
  , byteOffset
  , byteSpan
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Char (ord)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign (lengthWord8)

import MiniRegex.Engine
  ( compilePattern, compileTree, nonEmptyAt, nonEmptyWithin
  , searchFrom, searchWithin )
import MiniRegex.Template (Template, expand, template)
import MiniRegex.Types hiding (source)
import MiniRegex.Types qualified as Types
-- ANCHOR_END: imports

-- ANCHOR: source
-- 📜 The pattern as it was written. A plain function, not the field of
--    the record: a field could also be used to change a Regex from
--    outside (re { source = "[" }), and its tree would not follow.
-- 書いたときのパターン。レコードのフィールドではなく、ふつうの関数に
-- する。フィールドだと、外から Regex を書き換える（re { source = "[" }）
-- のにも使えてしまい、木はついてこない
source :: Regex -> Text
source = Types.source
-- ANCHOR_END: source

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
  pure (Regex src opts tree (compileTree tree))
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

-- 🔍 The first match. If the steps run out (ten million, unless
--    told otherwise), GaveUp is thrown.
-- 最初に見つかったもの。ステップ数が尽きたら（既定は 1000万ステップ）、
-- GaveUp を投げる
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
findAll re t = runIdentity (walk (Identity . searchFrom re t)
                                 (Identity . nonEmptyAt re t)
                                 (T.length t))

-- 🚶 The walk, for any way of searching: `search` finds the first
--    match from a place, `nonEmpty` a match just at a place that is not
--    empty. With Identity, the list comes lazily, a match at a time;
--    with Either, a Left stops the walk.
-- どんな探し方でも使える歩き方：search は、その場所から最初の一致を、
-- nonEmpty は、ちょうどその場所の、空でない一致を探す。Identity なら、
-- リストは1つずつ遅延で出てくる。Either なら、Left で歩くのをやめる
walk
  :: Monad m
  => (Int -> m (Maybe Match)) -> (Int -> m (Maybe Match)) -> Int
  -> m [Match]
walk search nonEmpty len = go 0
  where
    go from = search from >>= maybe (pure []) next
    -- (with \K, even the match from nonEmpty may be empty)
    -- （\K があると、nonEmpty の一致でさえ、空のことがある）
    next m
      | matchStart m == matchEnd m = (m :) <$> afterEmpty (matchEnd m)
      | otherwise                  = (m :) <$> go (matchEnd m)
    afterEmpty here = nonEmpty here >>= \case
      Just m                -> next m
      Nothing
        | here < len        -> go (here + 1)
        | otherwise         -> pure []
-- ANCHOR_END: findall

-- ANCHOR: within
-- ⏱️ The first match, with at most this many steps; Left when they run
--    out. Nothing is thrown.
-- 最初に見つかったもの。ステップ数は多くてもこれだけ。尽きたら Left。
-- 何も投げない
findWithin :: Int -> Regex -> Text -> Either GaveUp (Maybe Match)
findWithin limit re t = searchWithin limit re t 0

-- ⏱️ All the matches; each search for one may take this many steps.
-- 見つかったもの全部。1つを探すごとに、このステップ数まで使ってよい
findAllWithin :: Int -> Regex -> Text -> Either GaveUp [Match]
findAllWithin limit re t =
  walk (searchWithin limit re t) (nonEmptyWithin limit re t) (T.length t)
-- ANCHOR_END: within

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

-- ANCHOR: bytes
-- 📏 The whole match, as a Span (the same as groupSpan 0).
-- 一致の全体を Span で（groupSpan 0 と同じ）
matchSpan :: Match -> Span
matchSpan m = Span (matchText m) (matchStart m) (matchEnd m)

-- 🧮 The n-th character of the text, counted in bytes of UTF-8 (the
--    way PCRE2, Rust and Go count). Text keeps UTF-8 inside, so this
--    is the length of the first n characters.
-- 文字列の n 文字目を、UTF-8 のバイトで数えたもの（PCRE2・Rust・Go の
-- 数え方）。Text は中身を UTF-8 で持っているので、頭の n 文字の長さ
byteOffset :: Text -> Int -> Int
byteOffset t n = lengthWord8 (T.take n t)

-- 📏 Where a span is in the text, in bytes: (start, end).
-- Span が文字列のどこにあるかを、バイトで：（始まり、終わり）
byteSpan :: Text -> Span -> (Int, Int)
byteSpan t s = (start, start + lengthWord8 (spanText s))
  where
    start = byteOffset t (spanStart s)
-- ANCHOR_END: bytes
-- ANCHOR_END: all
