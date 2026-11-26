-- ANCHOR: all
-- ANCHOR: orphans
-- The instances here are orphans: the type (Regex) is in MiniRegex.Types,
-- the classes are in regex-base. They are kept to this one module, the
-- face that is imported for them (regex-tdfa does the same).
-- ここのインスタンスは孤児：型（Regex）は MiniRegex.Types に、クラスは
-- regex-base にある。インスタンスは、そのために import する顔の、この
-- モジュールだけに置く（regex-tdfa も同じ）
{-# OPTIONS_GHC -Wno-orphans #-}
-- ANCHOR_END: orphans
-- ANCHOR: header
-- | The other face of MiniRegex: the type classes of regex-base, the
--   ones regex-tdfa, regex-posix and regex-pcre-builtin share. With
--   them come =~ and =~~. The engine inside is the same as MiniRegex's;
--   the instances only call it.
-- MiniRegex のもう1つの顔：regex-base の型クラス（regex-tdfa・
-- regex-posix・regex-pcre-builtin が共通に使うもの）。それといっしょに
-- =~ と =~~ が使える。中のエンジンは MiniRegex と同じで、インスタンスは
-- それを呼ぶだけ
module Text.Regex.MiniRegex
  ( -- * The classes and the answers of regex-base / regex-base のクラスと答え
    module Text.Regex.Base
    -- * The regex and its settings / 正規表現と設定
  , Regex
  , Option (..)
  , CompOption (..)
  , ExecOption (..)
    -- * The operators / 演算子
  , (=~)
  , (=~~)
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Control.Monad.Fail qualified as Fail
import Data.Array (listArray)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Regex.Base
import Text.Regex.Base.Impl (polymatch, polymatchM)

import MiniRegex (compileWith, explain, find, findAll, isMatch)
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: options
-- 🚩 The settings for making a regex: MiniRegex's flags, as a list.
-- 正規表現を作るときの設定：MiniRegex のフラグのリスト
newtype CompOption = CompOption [Option]
  deriving (Show, Eq)

-- ⏱️ The settings for running it: how many steps a search may take.
-- 動かすときの設定：1回の探しものに使ってよいステップ数
newtype ExecOption = ExecOption { execLimit :: Int }
  deriving (Show, Eq)

-- 🎛️ The three types go together: the regex tells the settings, and
--    the settings tell the regex (the functional dependencies of
--    regex-base). Both kinds of settings start with nothing in them.
-- 3つの型は組になっている：正規表現から設定が、設定から正規表現が決まる
-- （regex-base の関数従属性）。どちらの設定も、空から始まる
instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt   = CompOption []
  blankExecOpt   = ExecOption defaultLimit
  defaultCompOpt = CompOption []
  defaultExecOpt = ExecOption defaultLimit
  setExecOpts (ExecOption n) re = re { stepLimit = n }
  getExecOpts re = ExecOption (stepLimit re)
-- ANCHOR_END: options

-- ANCHOR: maker
-- 🏗️ Make a regex from a Text. A wrong pattern is a `fail` of the
--    monad, with the same words as MiniRegex's explain.
-- Text から正規表現を作る。まちがったパターンは、そのモナドの fail。
-- 文は MiniRegex の explain と同じ
instance RegexMaker Regex CompOption ExecOption Text where
  makeRegexOptsM c e src =
    either (Fail.fail . T.unpack . explain src) pure (build c e src)
  -- without a monad, a wrong pattern can only be an error
  -- モナドがなければ、まちがったパターンは error にするしかない
  makeRegexOpts c e src =
    either (error . T.unpack . explain src) id (build c e src)

-- 🧱 compileWith, with the settings for running put in.
-- compileWith に、動かすときの設定を入れたもの
build :: CompOption -> ExecOption -> Text -> Either RegexError Regex
build (CompOption flags) e src = setExecOpts e <$> compileWith flags src

-- 🧶 From a String: the same, after T.pack.
-- String から：T.pack してから同じように
instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOptsM c e = makeRegexOptsM c e . T.pack
  makeRegexOpts c e  = makeRegexOpts c e . T.pack
-- ANCHOR_END: maker

-- ANCHOR: like
-- 🔍 Find in a Text. The places are counted in characters, the way
--    regex-base's Extract for Text cuts (T.take and T.drop). MiniRegex
--    counts in characters too, so they go over as they are.
-- Text の中を探す。場所は文字で数える。regex-base の Text の Extract が、
-- T.take と T.drop で切るから。MiniRegex も文字で数えるので、そのまま
-- 渡せる
instance RegexLike Regex Text where
  matchOnce re t  = toArray <$> find re t
  matchAll re t   = map toArray (findAll re t)
  matchCount re t = length (findAll re t)
  matchTest       = isMatch
  -- the texts are in the Match already: no cutting t from its start
  -- again for each match (regex-base's own definitions would)
  -- 文字列は Match の中にある。一致のたびに t を頭から切り直さない
  -- （regex-base の既定の定義は、そうする）
  matchOnceText re t = withTexts <$> find re t
    where
      withTexts m = ( T.take (matchStart m) t, toTexts m
                    , T.drop (matchEnd m) t )
  matchAllText re t = map toTexts (findAll re t)

-- 🧶 Find in a String: the places of a String are characters, too.
--    The texts come from the Text's answers, unpacked. A Char that a
--    Text cannot hold (a lone surrogate, U+D800 to U+DFFF) is looked
--    at as U+FFFD, in the text and in the pattern alike.
-- String の中を探す：String の場所も、文字で数える。文字列は、Text の
-- 答えを unpack したもの。Text に入らない文字（ひとりぼっちのサロゲート、
-- U+D800〜U+DFFF）は、文字列でもパターンでも U+FFFD として探す
instance RegexLike Regex String where
  matchOnce re  = matchOnce re . T.pack
  matchAll re   = matchAll re . T.pack
  matchCount re = matchCount re . T.pack
  matchTest re  = matchTest re . T.pack
  matchOnceText re s = unpack3 <$> matchOnceText re (T.pack s)
    where
      unpack3 (b, m, a) = (T.unpack b, fmap unpackFst m, T.unpack a)
  matchAllText re s = map (fmap unpackFst) (matchAllText re (T.pack s))
-- ANCHOR_END: like

-- ANCHOR: toarray
-- 🗂️ A Match as regex-base wants it: for group 0 (the whole match),
--    1, 2 …, where it starts and how long it is. A group that took no
--    part is (-1, 0).
-- regex-base が欲しい形の Match：0番（一致の全体）、1番、2番……の
-- グループごとに、始まりと長さ。使われなかったグループは (-1, 0)
toArray :: Match -> MatchArray
toArray m = listArray (0, length places - 1) places
  where
    places = (matchStart m, matchEnd m - matchStart m)
           : map (maybe (-1, 0) offLen) (matchGroups m)
    offLen s = (spanStart s, spanEnd s - spanStart s)

-- 🗂️ The same, with the text of each group beside its place. A group
--    that took no part is the empty text at (-1, 0).
-- 同じものに、グループごとの文字列を添えて。使われなかったグループは、
-- (-1, 0) の空の文字列
toTexts :: Match -> MatchText Text
toTexts m = listArray (0, length texts - 1) texts
  where
    texts = (matchText m, (matchStart m, matchEnd m - matchStart m))
          : map (maybe ("", (-1, 0)) withPlace) (matchGroups m)
    withPlace s = (spanText s, (spanStart s, spanEnd s - spanStart s))

-- 🧶 A text and its place, with the text as a String.
-- 文字列と場所の組の、文字列を String に
unpackFst :: (Text, a) -> (String, a)
unpackFst (x, place) = (T.unpack x, place)
-- ANCHOR_END: toarray

-- ANCHOR: context
-- 🎯 The one answer regex-base cannot write for every engine: the same
--    type as the text (the text of the first match, or empty). The
--    other answers (Bool, Int, the four, …) come from regex-base.
-- regex-base が、どのエンジンにも書いてはおけない答え：文字列と同じ型
-- （最初の一致の文字列か、空）。ほかの答え（Bool・Int・4つ組……）は
-- regex-base のもの
instance RegexContext Regex Text Text where
  match  = polymatch
  matchM = polymatchM

instance RegexContext Regex String String where
  match  = polymatch
  matchM = polymatchM
-- ANCHOR_END: context

-- ANCHOR: operators
-- 🎭 text =~ pattern: make the regex, and match it. What comes back is
--    chosen by the type the answer is wanted in.
-- 文字列 =~ パターン：正規表現を作って、当てる。何が返るかは、答えが
-- 欲しい型で決まる
(=~)
  :: ( RegexMaker Regex CompOption ExecOption source
     , RegexContext Regex text target )
  => text -> source -> target
text =~ pattern = match (makeRegex pattern :: Regex) text

-- 🎭 The same in a monad: a wrong pattern, or no match, is a `fail`.
-- モナドの中で同じことを：まちがったパターンも、見つからないのも fail
(=~~)
  :: ( RegexMaker Regex CompOption ExecOption source
     , RegexContext Regex text target
     , Fail.MonadFail m )
  => text -> source -> m target
text =~~ pattern = do
  (re :: Regex) <- makeRegexM pattern
  matchM re text

-- ANCHOR_END: operators
-- ANCHOR_END: all
