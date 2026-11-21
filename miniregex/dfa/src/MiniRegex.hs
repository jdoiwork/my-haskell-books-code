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
  , Engine (..)
  , engine
  , RegexError (..)
  , errorOffset
  , explain
    -- * Finding / 探す
  , Match (..)
  , Span (..)
  , isMatch
  , find
  , findAll
  , findVM
  , findAllVM
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
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign (lengthWord8)

import MiniRegex.Cursor (Cursor, headOf, step)
import MiniRegex.Engine
  ( chooseEngine, compilePattern, compileTree, nonEmptyAtCursor
  , nonEmptyWithinAt, searchAt, searchFrom, searchWithin, searchWithinAt )
import MiniRegex.LazyDFA (dfaFind, dfaFindAll)
import MiniRegex.Prefilter (prefilterOf)
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
  let prog = compileTree tree
  eng <- chooseEngine opts prog
  pure (Regex src opts tree prog eng (prefilterOf tree prog))
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
      NotLinear w      -> "not linear: the program has " <> w
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
--    told otherwise), GaveUp is thrown. From the chapter on the lazy
--    DFA: the DFA first, if it can run the regex. The DFA counts no
--    steps; the groups come from the engine the regex chose, with the
--    limit.
-- 最初に見つかったもの。ステップ数が尽きたら（既定は 1000万ステップ）、
-- GaveUp を投げる。lazy DFA の章から：DFA で動かせるなら、まず DFA で。
-- DFA は、ステップ数を数えない。グループは、選んだエンジンで上限つき
find :: Regex -> Text -> Maybe Match
find re t = fromMaybe (findVM re t) (dfaFind re t 0)

-- 🔍 The same, with the VM only (the chapters before).
-- 同じものを、VM だけで（前の章まで）
findVM :: Regex -> Text -> Maybe Match
findVM re t = searchFrom re t 0

-- ANCHOR_END: find

-- ANCHOR: findall
-- 🔎 All the matches, from left to right, not overlapping. After an
--    empty match, first try for a match that is not empty, at the same
--    place; only if there is none, go one character on. (PCRE2's rule,
--    the same as pcre2test's /g, Perl and Python.)
-- 見つかったもの全部。左から順に、重ならないように。空の一致のあとは、
-- まず同じ場所で、空でない一致を探す。それがなければ、1文字先へ進む
-- （PCRE2 の決まり。pcre2test の /g、Perl、Python と同じ）
-- ANCHOR: findalldfa
findAll :: Regex -> Text -> [Match]
findAll re t = fromMaybe (findAllVM re t) (dfaFindAll re t)

-- 🔎 The same, with the VM only (the chapters before).
-- 同じものを、VM だけで（前の章まで）
findAllVM :: Regex -> Text -> [Match]
findAllVM re t = runIdentity (walk (Identity . searchAt re')
                                 (Identity . nonEmptyAtCursor re')
                                 (headOf t))
  where
    re' = walking re
-- ANCHOR_END: findalldfa

-- 🚶 For the walk, Bounded switches instead: a table for the rest of
--    the text at each match would cost more than it saves (from the
--    chapter on measuring).
-- 歩くときは、Bounded のかわりに乗り換えを使う。一致のたびに、残り全部の
-- 表を作ると、得より損のほうが大きい（測って比べる章から）
walking :: Regex -> Regex
walking re
  | engine re == Bounded = re { engine = Switching }
  | otherwise            = re

-- 🚶 The walk, for any way of searching: `search` finds the first
--    match from a place, `nonEmpty` a match just at a place that is not
--    empty. Each gives back the cursor where its match ended, and the
--    walk goes on from that cursor: no counting from the head of the
--    text for each match (from the chapter on switching engines). With
--    Identity, the list comes lazily, a match at a time; with Either, a
--    Left stops the walk.
-- どんな探し方でも使える歩き方：search は、その場所から最初の一致を、
-- nonEmpty は、ちょうどその場所の、空でない一致を探す。どちらも、一致が
-- 終わった場所のしおりも返し、歩き方は、そのしおりから続ける。一致の
-- たびに文字列の頭から数え直さない（エンジンを乗り換える章から）。
-- Identity なら、リストは1つずつ遅延で出てくる。Either なら、Left で
-- 歩くのをやめる
walk
  :: Monad m
  => (Cursor -> m (Maybe (Match, Cursor)))
  -> (Cursor -> m (Maybe (Match, Cursor)))
  -> Cursor -> m [Match]
walk search nonEmpty = go
  where
    go from = search from >>= maybe (pure []) next
    -- (with \K, even the match from nonEmpty may be empty)
    -- （\K があると、nonEmpty の一致でさえ、空のことがある）
    next (m, end)
      | matchStart m == matchEnd m = (m :) <$> afterEmpty end
      | otherwise                  = (m :) <$> go end
    afterEmpty here = nonEmpty here >>= \case
      Just found -> next found
      Nothing    -> case step here of
        Just (_, here') -> go here'
        Nothing         -> pure []
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
  walk (searchWithinAt limit re') (nonEmptyWithinAt limit re') (headOf t)
  where
    re' = walking re
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
replace re new t = rebuild (const new) (maybeToList (find re t)) t

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
replaceTemplate re tpl t =
  rebuild (expand tpl) (maybeToList (find re t)) t

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
