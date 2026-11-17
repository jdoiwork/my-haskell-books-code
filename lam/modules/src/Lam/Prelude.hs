-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR: all
-- | The prelude: Lam's standard library, written in Lam itself, now
--   cut into modules (Prelude, Maybe, Result, IO, Map, Json, Match,
--   Regex, Parser). Every program starts with the default imports.
-- プレリュード：Lam で書いた、Lam の標準ライブラリ。いまはモジュールに
-- 分けてある。どのプログラムも、はじめの import から始まる。
module Lam.Prelude
  ( standardSources
  , defaultImports
  , buildPrelude
  , preludeSession
  ) where

import Control.Monad (foldM)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)

import Lam.Module (importInto, standardModule)
import Lam.Parser (program, runLamParser)
import Lam.Program
import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: sources
-- 📚 The standard modules, in the order they are made. Each one can
--    use the ones above it (with their default imports).
-- 標準のモジュール。作る順に並べる。どれも、上にあるものを（はじめの
-- import の形で）使える
standardSources :: [(Name, Text)]
standardSources =
  [ ("Maybe",   maybeSource)
  , ("Result",  resultSource)
  , ("Prelude", basicsSource)
  , ("IO",      ioSource)
  , ("Map",     mapSource)
  , ("Json",    jsonSource)
  , ("Match",   matchSource)
  , ("Regex",   regexSource)
  , ("Parser",  parserSource) ]
-- ANCHOR_END: sources

-- ANCHOR: defaultImports
-- 📥 What every file has before its first line. A file writes its own
--    import only for another name (as) or more names without the
--    module (exposing).
-- どのファイルにも、1行目の前にあるもの。自分で import を書くのは、
-- 別の名前（as）や、モジュールの名前なしで使う名前（exposing）が
-- ほしいときだけ
defaultImports :: Text
defaultImports = """
  import Prelude exposing (..)
  import Maybe exposing (Maybe (..))
  import Result exposing (Result (..))
  import IO
  import Map
  import Json exposing (Json (..))
  import Match exposing (Match (..))
  import Regex
  import Parser
  """
-- ANCHOR_END: defaultImports

-- ANCHOR: maybe
maybeSource :: Text
maybeSource = """
  module Maybe exposing (Maybe (..), map, bind, andThen)

  -- A value that may be missing
  -- ないかもしれない値
  data Maybe a = Nothing | Just a

  -- Go on with a value that may be missing
  -- ないかもしれない値で、続きを計算する
  let map f m =
    case m of
      | Nothing -> Nothing
      | Just x -> Just (f x)
  -- bind takes the value first (like >>= of Haskell); andThen is
  -- the same with the function first (like Elm).
  -- bind は値が先（Haskell の >>= と同じ）。andThen は、同じものの
  -- 関数が先の形（Elm と同じ）
  let bind m f =
    case m of
      | Nothing -> Nothing
      | Just x -> f x
  let andThen f m = bind m f
  """
-- ANCHOR_END: maybe

-- ANCHOR: result
resultSource :: Text
resultSource = """
  module Result exposing (Result (..), map, bind, andThen)

  -- A result that may be an error
  -- エラーかもしれない結果
  data Result e a = Ok a | Err e

  let map f r =
    case r of
      | Ok x -> Ok (f x)
      | Err e -> Err e
  let bind r f =
    case r of
      | Ok x -> f x
      | Err e -> Err e
  let andThen f r = bind r f
  """
-- ANCHOR_END: result

-- ANCHOR: basics
basicsSource :: Text
basicsSource = """
  module Prelude exposing (..)

  -- Functions about functions
  -- 関数についての関数
  let id x = x
  let const x y = x
  let compose f g x = f (g x)
  let flip f x y = f y x
  let twice f x = f (f x)

  -- true and false
  -- 真と偽
  let not b = if b then false else true
  let and a b = if a then b else false
  let or a b = if a then true else b

  -- Numbers
  -- 数
  let max a b = if a < b then b else a
  let min a b = if a < b then a else b
  let abs n = if n < 0 then 0 - n else n
  let mod a b = a - a / b * b
  let even n = mod n 2 == 0
  let pow b e = if e == 0 then 1 else b * pow b (e - 1)
  let gcd a b = if b == 0 then a else gcd b (mod a b)

  -- Lists, taken apart with case
  -- リスト。case で分ける
  let foldr f z xs =
    case xs of
      | [] -> z
      | x : rest -> f x (foldr f z rest)
  let foldl f z xs =
    case xs of
      | [] -> z
      | x : rest -> foldl f (f z x) rest
  let map f xs = foldr (\\x rest -> f x : rest) [] xs
  let filter p xs =
    foldr (\\x rest -> if p x then x : rest else rest) [] xs
  let sum xs = foldl (\\total x -> total + x) 0 xs
  let reverse xs = foldl (\\acc x -> x : acc) [] xs
  let range a b = if a > b then [] else a : range (a + 1) b
  let drop n xs =
    if n < 1 then xs
    else case xs of
      | [] -> []
      | _ : rest -> drop (n - 1) rest
  -- The first n items (the top 10 of a ranking)
  -- 最初の n 個（ランキングの上から10個）
  let take n xs =
    if n < 1 then []
    else case xs of
      | [] -> []
      | x : rest -> x : take (n - 1) rest

  -- With Maybe (from the module Maybe, above)
  -- Maybe を使うもの（上のモジュール Maybe から）
  let fromMaybe d m =
    case m of
      | Nothing -> d
      | Just x -> x
  let find p xs =
    case xs of
      | [] -> Nothing
      | x : rest -> if p x then Just x else find p rest
  let lookup k pairs =
    case pairs of
      | [] -> Nothing
      | (k2, v) : rest -> if k == k2 then Just v else lookup k rest

  -- Do an action for each item of a list, in order
  -- リストの1つずつに、順に頼みごとをする
  let forEach xs f =
    case xs of
      | [] -> pure ()
      | x : rest -> do
          f x
          forEach rest f
  """
-- ANCHOR_END: basics

-- ANCHOR: io
ioSource :: Text
ioSource = """
  module IO exposing (andThen)

  -- bind for IO, with the function first, like Result.andThen.
  -- IO の bind の、関数が先の形。Result.andThen と同じ
  let andThen f m = bind m f
  """
-- ANCHOR_END: io

-- ANCHOR: map
mapSource :: Text
mapSource = """
  module Map exposing (..)

  -- Maps from strings. empty, insert, lookup, ... are builtins of
  -- this module; here are the ones that take a function.
  -- 文字列をキーにした Map。empty・insert・lookup などは、この
  -- モジュールの組み込み。ここには、関数を受け取るものを書く
  let insertWith f k v m =
    case lookup k m of
      | Nothing -> insert k v m
      | Just old -> insert k (f v old) m
  -- map of the Prelude is for lists: write Prelude.map, since the map
  -- defined here would call itself.
  -- Prelude の map はリスト用。ここで決める map が自分を呼ばないよう、
  -- Prelude.map と書く
  let map f m =
    fromList (Prelude.map (\\kv -> (fst kv, f (snd kv))) (toList m))
  """
-- ANCHOR_END: map

-- ANCHOR: json
jsonSource :: Text
jsonSource = """
  module Json exposing
    ( Json (JNull, JBool, JInt, JStr, JArr, JObj)
    , parse, show, get, at, int, string, bool, array
    , mkDec, decimal )

  -- JSON, as a data type. A number with a fraction is JDec:
  -- JDec 1999 2 is 1999 divided by 10 to the 2, that is 19.99.
  -- JDec is hidden: make one with mkDec, look inside with decimal.
  -- JSON を data の型で。小数は JDec：JDec 1999 2 は 19.99。
  -- JDec は隠す。作るのは mkDec、中を見るのは decimal
  data Json
    = JNull
    | JBool Bool
    | JInt Int
    | JDec Int Int
    | JStr String
    | JArr [Json]
    | JObj (Map Json)

  -- Take values out of JSON. Each one gives Nothing when the JSON
  -- has another shape.
  -- JSON から値を取り出す。形がちがえば、どれも Nothing
  let get k j =
    case j of
      | JObj m -> Map.lookup k m
      | _ -> Nothing
  let at path j =
    case path of
      | [] -> Just j
      | k : rest -> Maybe.andThen (at rest) (get k j)
  let int j = case j of | JInt n -> Just n | _ -> Nothing
  let string j = case j of | JStr s -> Just s | _ -> Nothing
  let bool j = case j of | JBool b -> Just b | _ -> Nothing
  let array j = case j of | JArr xs -> Just xs | _ -> Nothing

  -- ANCHOR: mkDec
  -- Make a number with a fraction, in the same shape as parse makes
  -- it: the 0s at the end of c move into d, and a whole number is
  -- JInt.  mkDec 19990 3 is JDec 1999 2, and mkDec 1900 2 is JInt 19.
  -- 小数を、parse と同じ形で作る。c の後ろの 0 は d へ移し、
  -- 整数なら JInt にする
  let mkDec c d =
    if c == 0 then JInt 0
    else if mod c 10 == 0 then mkDec (c / 10) (d - 1)
    else if d > 0 || d < 0 - 1024 then JDec c d
    else JInt (c * pow 10 (0 - d))
  -- The two numbers of a fraction:  decimal (mkDec 1999 2)  is
  -- Just (1999, 2). A whole number (JInt) is not a fraction: Nothing.
  -- 小数の2つの数。整数（JInt）は小数ではないので Nothing
  let decimal j = case j of | JDec c d -> Just (c, d) | _ -> Nothing
  -- ANCHOR_END: mkDec
  """
-- ANCHOR_END: json

-- ANCHOR: match
matchSource :: Text
matchSource = """
  module Match exposing (Match (..), text, start, groups, group, named)

  -- What a regex found: the whole text, where it starts (counted in
  -- letters, from 0), the groups (1, 2, ...), and the named groups.
  -- A group that took no part is "".
  -- 正規表現が見つけたもの：全体の文字、始まる場所（0 から数えた文字の
  -- 数）、グループ（1, 2, ...）、名前付きのグループ。使われなかった
  -- グループは ""
  data Match = Match String Int [String] (Map String)

  let text m = case m of | Match t _ _ _ -> t
  let start m = case m of | Match _ i _ _ -> i
  let groups m = case m of | Match _ _ gs _ -> gs
  -- group 0 is the whole text, as in Python.
  -- 0番は全体（Python と同じ）
  let group n m =
    if n == 0 then Just (text m)
    else case drop (n - 1) (groups m) of
      | g : _ -> Just g
      | [] -> Nothing
  let named name m =
    case m of | Match _ _ _ names -> Map.lookup name names
  """
-- ANCHOR_END: match

-- ANCHOR: regex
regexSource :: Text
regexSource = """
  module Regex exposing (..)

  -- Everything here is a builtin: find, findAll, replace, split, ...
  -- ここは全部、組み込み
  """
-- ANCHOR_END: regex

-- ANCHOR: parser
parserSource :: Text
parserSource = """
  module Parser exposing (..)

  -- Small tools for parsers, made of the builtins with Parser.do.
  -- Parser.do で、組み込みのパーサーから作る小さな道具
  -- p, then the spaces after it:  Parser.token Parser.int
  -- p と、その後ろの空白
  let token p = Parser.do
    x <- p
    _ <- spaces
    pure x
  -- This text, then the spaces after it:  Parser.symbol "("
  -- この文字と、その後ろの空白
  let symbol s = token (string s)
  -- p between two others:  Parser.between "(" ")" p
  -- 2つの文字ではさまれた p
  let between open close p = Parser.do
    _ <- symbol open
    x <- p
    _ <- symbol close
    pure x
  """
-- ANCHOR_END: parser

-- ANCHOR: build
-- 🌱 Make the standard modules in order. After each one, do its
--    default import, so that the next ones (and every program) can
--    use it.
-- 標準のモジュールを順に作る。1つ作るたびに、そのはじめの import を
-- して、次のモジュール（と、どのプログラム）からも使えるようにする
buildPrelude :: Either Text Session
buildPrelude = do
  imports <- either (Left . T.pack . errorBundlePretty)
               (Right . programImports)
               (runLamParser program "<default imports>" defaultImports)
  foldM (addModule imports) emptySession standardSources
  where
    addModule imports session (name, source) = do
      m <- standardModule session name source
      let known = session
            { sessionModules = Map.insert name m (sessionModules session) }
      case find ((== name) . importName) imports of
        Just imp -> importInto known imp m
        Nothing  -> Right known

-- 🌱 A session with the standard library. It is checked by the
--    tests, so a mistake in it is found before anyone runs Lam.
-- 標準ライブラリ入りのセッション。テストで確かめるので、まちがいは、
-- 誰かが Lam を動かす前に見つかる
preludeSession :: Session
preludeSession = case buildPrelude of
  Right s  -> s
  Left err -> error ("the prelude is broken: " <> T.unpack err)
-- ANCHOR_END: build
-- ANCHOR_END: all
