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
  , ("Parser",  parserSource)
  , ("Http",    httpSource)
  , ("Sql",     sqlSource) ]
-- ANCHOR_END: sources

-- ANCHOR: defaultImports
-- 📥 What every file has before its first line. A file writes its own
--    import only for another name (as) or more names without the
--    module (the list in parentheses).
-- どのファイルにも、1行目の前にあるもの。自分で import を書くのは、
-- 別の名前（as）や、モジュールの名前なしで使う名前（かっこの一覧）が
-- ほしいときだけ
defaultImports :: Text
defaultImports = """
  import Prelude (..)
  import Maybe (Maybe (..))
  import Result (Result (..))
  import IO
  import Map
  import Json (Json (..))
  import Match (Match (..))
  import Regex
  import Parser
  import Http (HttpError (..))
  import Sql
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
  module Result exposing (Result (..), map, mapError, bind, andThen)

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
  -- Change the error, and keep a value as it is
  -- エラーのほうを変える。値はそのまま
  let mapError f r =
    case r of
      | Ok x -> Ok x
      | Err e -> Err (f e)
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

  -- ANCHOR: oneLinerHelpers
  -- For one-liners: a number from text with a number to use when it is
  -- not one, show any value on a line, and do an action only when.
  --   toIntOr 0 "12"  is 12,   toIntOr 0 "-"  is 0
  -- ワンライナーのために：数でないときの数を決めて、文字列を数にする。
  -- どんな値も1行で見せる。条件が合うときだけ頼みごとをする
  let toIntOr d s = fromMaybe d (toInt s)
  let print x = putStrLn (display x)
  let when b io = if b then io else pure ()
  -- ANCHOR_END: oneLinerHelpers
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
    , mkDec, decimal, fixed, ratio )

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
  -- ANCHOR: fixed
  -- c / k, rounded half away from 0:  halfAway 1275 10 is 128
  -- c を k で割って、四捨五入（0 から遠いほうへ）
  let halfAway c k =
    let q = (abs c * 2 + k) / (k * 2) in
    if c < 0 then 0 - q else q
  -- v is the number times 10 to the n: scaled 1 30 is "3.0"
  -- v は、数を 10 の n 乗倍したもの
  let scaled n v =
    let whole = abs v / pow 10 n in
    let sign = if v < 0 then "-" else "" in
    if n == 0 then sign ++ showInt whole
    else sign ++ showInt whole ++ "." ++ zeroPad n (mod (abs v) (pow 10 n))
  -- A number with n digits after the point, rounded half away from 0:
  -- fixed 1 (JInt 3) is "3.0", fixed 1 (mkDec 1275 2) is "12.8".
  -- Not a number: as show writes it.
  -- 小数点以下を n けたにした数。四捨五入（0 から遠いほうへ）：
  -- fixed 1 (JInt 3) は "3.0"、fixed 1 (mkDec 1275 2) は "12.8"。
  -- 数でなければ、show と同じ
  let fixed n j =
    let digits = max n 0 in
    case j of
      | JInt i -> scaled digits (i * pow 10 digits)
      | JDec c d ->
          if d <= digits then scaled digits (c * pow 10 (digits - d))
          else scaled digits (halfAway c (pow 10 (d - digits)))
      | _ -> show j
  -- ANCHOR_END: fixed
  -- ANCHOR: ratio
  -- a divided by b, as a number with a fraction: 12 digits after the
  -- point, cut off (enough for fixed). ratio 217 3 is 72.333333333333.
  -- a を b で割った小数。小数点以下12けた（その先は切り捨て。fixed には
  -- 十分）。b が 0 なら、割り算のエラー
  let ratio a b =
    let sign = if (a < 0) == (b < 0) then 1 else 0 - 1 in
    mkDec (sign * (abs a * pow 10 12 / abs b)) 12
  -- ANCHOR_END: ratio
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

-- ANCHOR: http
-- ANCHOR: httpHead
httpSource :: Text
httpSource = """
  module Http exposing
    ( Response, HttpError (..)
    , send, request, get, post
    , status, headers, header, body
    , string, json, response, errorText )
-- ANCHOR_END: httpHead

  -- ANCHOR: response
  -- An answer: the status (200, 404, ...), the headers, and the body.
  -- The names of the headers are in small letters.
  -- 答え：状態コード（200、404 など）、ヘッダー、本文。
  -- ヘッダーの名前は小文字
  data Response = Response Int (Map String) String

  let status r = case r of | Response s _ _ -> s
  let headers r = case r of | Response _ hs _ -> hs
  let body r = case r of | Response _ _ b -> b
  let header name r = Map.lookup (toLower name) (headers r)
  -- ANCHOR_END: response

  -- ANCHOR: httpError
  -- What went wrong. e is the body of a failed answer (404, 500, ...),
  -- read by the second reader of request.
  -- うまくいかなかったわけ。e は、失敗した答え（404、500 など）の本文で、
  -- request の2つ目の読み方で読む
  data HttpError e
    = NoAnswer String        -- no answer came (no network, ...)
    | BadStatus Int e        -- the answer was 404, 500, ...
    | BadBody String         -- the body could not be read (bad JSON)
  -- ANCHOR_END: httpError

  -- ANCHOR: readers
  -- Readers: how to read the body of an answer.
  -- 読み方：答えの本文を、どう読むか
  let string r = Ok (body r)
  let json r = Json.parse (body r)
  let response r = Ok r
  -- ANCHOR_END: readers

  -- ANCHOR: request
  -- The long one: choose how to read a good answer (2xx), and how to
  -- read a bad one (4xx, 5xx).
  -- 長いほう：よい答え（2xx）の読み方と、悪い答え（4xx・5xx）の読み方を
  -- 選ぶ
  let request readOk readBad method url hs text = do
    answer <- send method url hs text
    pure (case answer of
      | Err e -> Err (NoAnswer e)
      | Ok r ->
          if status r >= 200 && status r < 300 then
            Result.mapError BadBody (readOk r)
          else
            case readBad r of
              | Ok e -> Err (BadStatus (status r) e)
              | Err e -> Err (BadBody e))
  -- ANCHOR_END: request

  -- ANCHOR: short
  -- The short ones: a good answer is Json, a bad one a String.
  -- 短いほう：よい答えは Json、悪い答えは String
  let errorText e =
    case e of
      | NoAnswer m -> m
      | BadStatus s text -> f"status {s}: {text}"
      | BadBody m -> m
  let get url = do
    answer <- request json string "GET" url {} ""
    pure (Result.mapError errorText answer)
  let post url j = do
    answer <-
      request json string "POST" url
        {"content-type": "application/json"} (Json.show j)
    pure (Result.mapError errorText answer)
  -- ANCHOR_END: short
  """
-- ANCHOR_END: http

-- ANCHOR: sql
sqlSource :: Text
sqlSource = """
  module Sql exposing
    (Db, open, openReadOnly, query, exec, close, column)

  -- An open database. The constructor is hidden: only Sql.open makes it.
  -- 開いたデータベース。構成子は隠す：作れるのは Sql.open だけ
  data Db = Db Int

  -- One column of a row, if it is there.
  -- 行の、1つの列（あれば）
  let column name row = Map.lookup name row
  """
-- ANCHOR_END: sql

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
