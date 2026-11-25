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
import Control.Exception (throw)
import Control.Monad.State.Lazy (StateT (..), evalState, evalStateT, state)
import Data.Char (ord)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign (lengthWord8)

import MiniRegex.Cursor (Cursor, headOf, step)
import MiniRegex.Engine
  ( budgetFor, chooseEngine, compilePattern, compileTree, nonEmptyAtCursor
  , nonEmptySharing, nonEmptyWithinAt, searchAt, searchFrom, searchSharing
  , searchWithin, searchWithinAt )
import MiniRegex.Prefilter (Prefilter (..), Start (..), prefilterOf)
import MiniRegex.Template (Template, expand, template)
import MiniRegex.Types hiding (engine, source)
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

-- 🚂 The engine compile chose. A plain function too: with the field,
--    re { engine = PikeVM } would run a program the Pike VM cannot run.
-- compile が選んだエンジン。これも、ふつうの関数にする。フィールドだと、
-- re { engine = PikeVM } で、Pike VM が動かせないプログラムを動かせてしまう
engine :: Regex -> Engine
engine = Types.engine
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
explain src err = T.unlines (upTo ++ [caret] ++ after ++ [why])
  where
    -- the ^ goes under the line of the trouble (a pattern may have
    -- newlines), counting from the start of that line
    -- ^ は、つまずいた行の下に置く（パターンに改行があることもある）。
    -- その行の頭から数える
    before        = T.take (errorOffset err) src
    (upTo, after) = splitAt (T.count "\n" before + 1) (T.splitOn "\n" src)
    lineHead      = T.takeWhileEnd (/= '\n') before
    caret         = T.replicate (width lineHead) " " <> "^"
    why = case err of
      Invalid at m     -> "wrong pattern at " <> place at <> ": " <> m
      Unsupported at w -> "not yet at " <> place at <> ": " <> w
      NotLinear w      -> "not linear: the program has " <> w
    place = T.pack . show

-- 📐 How many columns the text takes on a terminal. A Japanese or
--    Chinese character takes two (a rough rule, good enough here), and
--    a tab goes on to the next multiple of 8.
-- 文字列が端末で何桁を占めるか。日本語や中国語の文字は2桁
-- （おおまかな決まりだが、ここでは十分）。タブは、次の8の倍数まで進む
width :: Text -> Int
width = T.foldl' column 0
  where
    column col c
      | c == '\t'    = (col `div` 8 + 1) * 8
      | wide (ord c) = col + 2
      | otherwise    = col + 1
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
findAll re t
  -- switching: one budget for the whole walk (see searchSharing)
  -- 乗り換え：歩きの全体で予算を1つ（searchSharing を見てほしい）
  | sharesBudget re' =
      evalState (walk (sharing (searchSharing defaultLimit))
                      (sharing (nonEmptySharing defaultLimit)) (headOf t))
                (budgetFor (program re') (headOf t))
  | otherwise = runIdentity (walk (Identity . searchAt re')
                                  (Identity . nonEmptyAtCursor re')
                                  (headOf t))
  where
    re' = walking re
    sharing f c = state (\left -> let (r, left') = f left re' c
                                   in  (either throw id r, left'))

-- ANCHOR: sharesbudget
-- 👛 The walks that share one budget: switching, but not a pattern of
--    plain texts only (it has its own runner, with no VM: the shared
--    budget would go round it).
-- 予算を1つ分け合う歩き：乗り換えのとき。ただし、ただの文字列だけの
-- パターンは除く（VM を使わない自分の走り方がある。分け合う予算の道を
-- 通すと、そこを迂回してしまう）
sharesBudget :: Regex -> Bool
sharesBudget re =
  engine re == Switching && not (literals (start (prefilter re)))
  where
    literals (Literals _) = True
    literals _            = False
-- ANCHOR_END: sharesbudget

-- 🚶 For the walk, Bounded switches instead: a table for the rest of
--    the text at each match would cost more than it saves (from the
--    chapter on measuring).
-- 歩くときは、Bounded のかわりに乗り換えを使う。一致のたびに、残り全部の
-- 表を作ると、得より損のほうが大きい（測って比べる章から）
walking :: Regex -> Regex
walking re
  | engine re == Bounded = re { Types.engine = Switching }
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
    -- (at the end there is no match that is not empty: PCRE2 does not
    -- look, and a recursion there could only go round)
    -- （終わりには、空でない一致はない：PCRE2 も探さない。探すと、
    -- 再帰がそこで回るだけ）
    afterEmpty here = case step here of
      Nothing         -> pure []
      Just (_, here') -> nonEmpty here >>= \case
        Just found -> next found
        Nothing    -> go here'
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
findAllWithin limit re t
  -- switching: one budget for the whole walk, as in findAll (the limit
  -- is still for each search)
  -- 乗り換え：findAll と同じく、歩きの全体で予算を1つ（上限は、いまも
  -- 1つを探すごと）
  | sharesBudget re' =
      evalStateT (walk (sharing (searchSharing limit))
                       (sharing (nonEmptySharing limit)) (headOf t))
                 (budgetFor (program re') (headOf t))
  | otherwise =
      walk (searchWithinAt limit re') (nonEmptyWithinAt limit re') (headOf t)
  where
    re' = walking re
    sharing f c = StateT (\left -> let (r, left') = f left re' c
                                    in  fmap (\x -> (x, left')) r)
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
-- 🧾 Replace the first match with the template ($1, ${name}, $$),
--    made by `template` for this same regex.
-- 最初に見つかったものを、テンプレート（$1・${name}・$$）で置き換える。
-- テンプレートは、同じ正規表現で template が作ったもの
replaceTemplate :: Regex -> Template -> Text -> Text
replaceTemplate re tpl t = rebuild (expand tpl) (take 1 (findAll re t)) t

-- 🧾 Replace every match with the template.
-- 見つかったもの全部を、テンプレートで置き換える
replaceAllTemplate :: Regex -> Template -> Text -> Text
replaceAllTemplate re tpl = replaceWith re (expand tpl)
-- ANCHOR_END: template

-- ANCHOR: rebuild
-- 🧵 Put the text back together, with each match made anew by f. Each
--    piece is cut from what is left of the text (rest, which starts at
--    the character `at`), not from the start again, so a long text is
--    walked only once.
-- 見つかったものを f で作り直しながら、文字列を組み立て直す。切れ端は、
-- 文字列の残り（at 文字目から始まる rest）から切る。頭から数え直さない
-- ので、長い文字列も1回歩くだけ
rebuild :: (Match -> Text) -> [Match] -> Text -> Text
rebuild f matches t = T.concat (go 0 t matches)
  where
    go _  rest []       = [rest]
    go at rest (m : ms) =
      let (before, atMatch) = T.splitAt (matchStart m - at) rest
          after             = T.drop (matchEnd m - matchStart m) atMatch
      in  before : f m : go (matchEnd m) after ms
-- ANCHOR_END: rebuild

-- ANCHOR: split
-- ✂️ The pieces between the matches, cut from what is left of the text,
--    as rebuild does.
-- 見つかったもののあいだの切れ端。rebuild と同じく、文字列の残りから切る
split :: Regex -> Text -> [Text]
split re t = go 0 t (findAll re t)
  where
    go _  rest []       = [rest]
    go at rest (m : ms) =
      let (before, atMatch) = T.splitAt (matchStart m - at) rest
          after             = T.drop (matchEnd m - matchStart m) atMatch
      in  before : go (matchEnd m) after ms
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
