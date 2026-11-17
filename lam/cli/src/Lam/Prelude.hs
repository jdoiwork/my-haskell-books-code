-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR: all
-- | The prelude: Lam's standard library, written in Lam itself, now
--   cut into modules (Maybe, Result, List, Prelude, String, IO, Map,
--   Json, Match, Regex, Parser, Http, Sql). Every program starts with
--   the default imports.
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
  , ("List",    listSource)
  , ("Prelude", basicsSource)
  , ("String",  stringSource)
  , ("IO",      ioSource)
  , ("Map",     mapSource)
  , ("Json",    jsonSource)
  , ("Match",   matchSource)
  , ("Regex",   regexSource)
  , ("Parser",  parserSource)
  , ("Http",    httpSource)
  , ("Sql",     sqlSource)
  -- ANCHOR: filesSources
  , ("Time",    timeSource)
  , ("Path",    pathSource)
  , ("File",    fileSource)
  , ("Process", processSource)
  , ("Random",  randomSource)
  -- ANCHOR_END: filesSources
  -- ANCHOR: floatSources
  , ("Float",   floatSource) ]
  -- ANCHOR_END: floatSources
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
  import List
  import String
  import IO
  import Map
  import Json (Json (..))
  import Match (Match (..))
  import Regex
  import Parser
  import Http (HttpError (..))
  import Sql
  import Time (Time, Zone)
  import Path
  import File
  import Process (Outcome (..))
  import Random
  import Float
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

-- ANCHOR: list
listSource :: Text
listSource = """
  module List exposing (..)

  -- The list versions: the Foldable functions of the Prelude make any
  -- container a list (toList), and then call these.
  -- リストの版。Prelude の Foldable の関数は、どんな入れ物もリストに
  -- して（toList）から、これを呼ぶ
  let foldr f z xs =
    case xs of
      | [] -> z
      | x : rest -> f x (foldr f z rest)
  let foldl f z xs =
    case xs of
      | [] -> z
      | x : rest -> foldl f (f z x) rest
  let map f xs = foldr (\\x rest -> f x : rest) [] xs
  let null xs = case xs of | [] -> true | _ -> false
  -- || and && stop early: any stops at the first item that passes.
  -- || と && は打ち切るので、any は最初に合ったところで止まる
  let any p xs =
    case xs of
      | [] -> false
      | x : rest -> p x || any p rest
  let all p xs =
    case xs of
      | [] -> true
      | x : rest -> p x && all p rest
  let elem y xs = any (\\x -> x == y) xs
  let sum xs = foldl (\\total x -> total + x) 0 xs
  let product xs = foldl (\\total x -> total * x) 1 xs
  -- An empty list has no biggest item: Nothing.
  -- 空のリストには、いちばん大きいものがない：Nothing
  let maximum xs =
    case xs of
      | [] -> Nothing
      | x : rest -> Just (foldl (\\m y -> if y > m then y else m) x rest)
  let minimum xs =
    case xs of
      | [] -> Nothing
      | x : rest -> Just (foldl (\\m y -> if y < m then y else m) x rest)
  let find p xs =
    case xs of
      | [] -> Nothing
      | x : rest -> if p x then Just x else find p rest
  let concatMap f xs = foldr (\\x rest -> f x ++ rest) [] xs
  let forEach xs f =
    case xs of
      | [] -> pure ()
      | x : rest -> do
          f x
          forEach rest f
  """
-- ANCHOR_END: list

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
  let abs n = if n < 0 then -n else n
  let mod a b = a - a // b * b
  let even n = mod n 2 == 0
  let pow b e = if e == 0 then 1 else b * pow b (e - 1)
  let gcd a b = if b == 0 then a else gcd b (mod a b)

  -- ANCHOR: foldable
  -- Any container (Foldable: a list, a Maybe, the values of a Map):
  -- make it a list with toList, then ask the module List.
  --   sum (Just 3) is 3,  maximum counts is the biggest value
  -- どんな入れ物でも（Foldable：リスト、Maybe、Map の値）、toList で
  -- リストにしてから、モジュール List に頼む
  let foldr f z t = List.foldr f z (toList t)
  let foldl f z t = List.foldl f z (toList t)
  let null t = List.null (toList t)
  let any p t = List.any p (toList t)
  let all p t = List.all p (toList t)
  let elem x t = List.elem x (toList t)
  let sum t = List.sum (toList t)
  let product t = List.product (toList t)
  let maximum t = List.maximum (toList t)
  let minimum t = List.minimum (toList t)
  let find p t = List.find p (toList t)
  let concatMap f t = List.concatMap f (toList t)
  -- ANCHOR_END: foldable

  -- ANCHOR: lists
  -- Lists only
  -- リストだけのもの
  let filter p xs =
    List.foldr (\\x rest -> if p x then x : rest else rest) [] xs
  let range a b = if a > b then [] else a : range (a + 1) b
  let concat xss = List.foldr (\\xs rest -> xs ++ rest) [] xss
  let replicate n x = if n < 1 then [] else x : replicate (n - 1) x
  -- Pairs, side by side; the longer list is cut:
  --   zip [1, 2, 3] ["a", "b"] is [(1, "a"), (2, "b")]
  -- 2つのリストを並べて組にする。長いほうは切る
  let zipWith f xs ys =
    case (xs, ys) of
      | (x : xr, y : yr) -> f x y : zipWith f xr yr
      | _ -> []
  let zip xs ys = zipWith (\\x y -> (x, y)) xs ys
  let unzip pairs = (List.map fst pairs, List.map snd pairs)
  -- Each item with its number, from 0 (Python's enumerate)
  -- 1つずつに、0 からの番号を付ける（Python の enumerate）
  let indexed xs = zip (range 0 (length xs - 1)) xs
  -- The items that pass, and those that do not
  -- 合うものと、合わないもの
  let partition p xs = (filter p xs, filter (\\x -> not (p x)) xs)
  -- Each item once, in the order they first came
  -- 同じものは1つだけ。最初に出てきた順
  let unique xs =
    List.foldl (\\seen x -> if List.elem x seen then seen else seen ++ [x])
      [] xs
  -- ANCHOR_END: lists

  -- With Maybe (from the module Maybe, above)
  -- Maybe を使うもの（上のモジュール Maybe から）
  let fromMaybe d m =
    case m of
      | Nothing -> d
      | Just x -> x
  let lookup k pairs =
    case pairs of
      | [] -> Nothing
      | (k2, v) : rest -> if k == k2 then Just v else lookup k rest

  -- Do an action for each item of a container, in order
  -- 入れ物の1つずつに、順に頼みごとをする
  let forEach t f = List.forEach (toList t) f

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

-- ANCHOR: string
stringSource :: Text
stringSource = """
  module String exposing (..)

  -- trim, split, startsWith, ... are builtins of this module (Haskell's
  -- Data.Text does the work). Here are the ones made of them.
  -- trim・split・startsWith などは、このモジュールの組み込み（中身は
  -- Haskell の Data.Text）。ここには、それを組み合わせたものを書く
  let isEmpty s = s == ""
  -- Cut into lines, and drop the spaces around each one, and the
  -- empty lines.
  -- 行に切って、それぞれのまわりの空白を落とす。空の行も落とす
  let cleanLines s =
    filter (\\l -> not (isEmpty l)) (map trim (lines s))
  """
-- ANCHOR_END: string

-- ANCHOR: io
ioSource :: Text
ioSource = """
  module IO exposing (andThen, mapM, filterM)

  -- bind for IO, with the function first, like Result.andThen.
  -- IO の bind の、関数が先の形。Result.andThen と同じ
  let andThen f m = bind m f

  -- ANCHOR: mapM
  -- Do f for each item, in order, and keep the answers (forEach
  -- throws them away). The same names as Haskell's.
  -- 1つずつ順に f をやり、答えを残す（forEach は捨てる）。名前は
  -- Haskell と同じ
  let mapM f xs =
    case xs of
      | [] -> pure []
      | x : rest -> do
          y <- f x
          ys <- mapM f rest
          pure (y : ys)

  -- Keep the items for which the IO answers true.
  -- IO が true と答えたものだけ残す
  let filterM p xs =
    case xs of
      | [] -> pure []
      | x : rest -> do
          keep <- p x
          kept <- filterM p rest
          pure (if keep then x : kept else kept)
  -- ANCHOR_END: mapM
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
    fromList (List.map (\\kv -> (fst kv, f (snd kv))) (toList m))
  -- ANCHOR: mapMore
  -- Keep the values that pass (filterWithKey also sees the key)
  -- 合う値だけを残す（filterWithKey はキーも見る）
  let filterWithKey p m =
    fromList (Prelude.filter (\\kv -> p (fst kv) (snd kv)) (toList m))
  let filter p m = filterWithKey (\\k v -> p v) m
  -- Change the value at k with f, which sees Nothing when k is not
  -- there, and gives Nothing to delete it (Elm's Dict.update).
  -- k の値を f で変える。k がなければ f は Nothing を受け取る。
  -- f が Nothing を返せば消す（Elm の Dict.update と同じ形）
  let update k f m =
    case f (lookup k m) of
      | Nothing -> delete k m
      | Just v -> insert k v m
  -- Go through the keys in order, with the key and the value.
  -- キーの順に、キーと値を見ながらたたむ
  let foldWithKey f z m =
    List.foldr (\\kv acc -> f (fst kv) (snd kv) acc) z (toList m)
  -- Two maps into one; f decides when both have the key.
  -- 2つの Map を1つに。両方にあるキーは f で決める
  let unionWith f a b =
    foldWithKey (\\k v acc -> insertWith (\\new old -> f old new) k v acc) a b
  -- ANCHOR_END: mapMore
  """
-- ANCHOR_END: map

-- ANCHOR: json
jsonSource :: Text
jsonSource = """
  module Json exposing
    ( Json (JNull, JBool, JInt, JStr, JArr, JObj)
    , parse, show, get, at, int, string, bool, array
    , mkDec, decimal, fixed, ratio, pretty, toFloat, fromFloat )

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
    else if mod c 10 == 0 then mkDec (c // 10) (d - 1)
    else if d > 0 || d < -1024 then JDec c d
    else JInt (c * pow 10 (-d))
  -- The two numbers of a fraction:  decimal (mkDec 1999 2)  is
  -- Just (1999, 2). A whole number (JInt) is not a fraction: Nothing.
  -- 小数の2つの数。整数（JInt）は小数ではないので Nothing
  let decimal j = case j of | JDec c d -> Just (c, d) | _ -> Nothing
  -- ANCHOR_END: mkDec
  -- ANCHOR: fixed
  -- c // k, rounded half away from 0:  halfAway 1275 10 is 128
  -- c を k で割って、四捨五入（0 から遠いほうへ）
  let halfAway c k =
    let q = (abs c * 2 + k) // (k * 2) in
    if c < 0 then -q else q
  -- v is the number times 10 to the n: scaled 1 30 is "3.0"
  -- v は、数を 10 の n 乗倍したもの
  let scaled n v =
    let whole = abs v // pow 10 n in
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
    let sign = if (a < 0) == (b < 0) then 1 else -1 in
    mkDec (sign * (abs a * pow 10 12 // abs b)) 12
  -- ANCHOR_END: ratio
  """
-- ANCHOR_END: json

-- ANCHOR: float
floatSource :: Text
floatSource = """
  module Float exposing (..)

  -- roundTiesEven, roundTiesUp, exp, log, pow, pi, isNaN, isInfinite
  -- and fixed are all builtins. toFloat, round, floor, ceiling,
  -- truncate, sqrt and negate are builtins without Float. in front.
  -- どれも組み込み。toFloat・round・floor・ceiling・truncate・sqrt・
  -- negate は、Float. の付かない組み込み
  """
-- ANCHOR_END: float

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
  -- The names of the headers are in small letters. A record now:
  -- r.status, r.body (the functions below are kept, too).
  -- 答え：状態コード（200、404 など）、ヘッダー、本文。
  -- ヘッダーの名前は小文字。いまはレコード（下の関数も残す）
  type alias Response =
    { status : Int, headers : Map String, body : String }

  let status r = r.status
  let headers r = r.headers
  let body r = r.body
  let header name r = Map.lookup (toLower name) r.headers
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

-- ANCHOR: time
timeSource :: Text
timeSource = """
  module Time exposing
    ( Time, Zone, now, here, utc, offset
    , toIso, fromIso, format, toDate
    , toYear, toMonth, toDay, toHour, toMinute, toSecond
    , addSeconds, addDays, diffSeconds, diffDays
    , toMillis, fromMillis )

  -- ANCHOR: timeData
  -- A moment: the milliseconds since 1970-01-01 00:00 in UTC. The
  -- constructor is hidden, so a Time is never taken for a number.
  -- ある瞬間：UTC の 1970-01-01 00:00 からのミリ秒。構成子は隠すので、
  -- Time を、ただの数ととりちがえることはない
  data Time = Time Int

  -- A time zone: how many minutes it is ahead of UTC (+09:00 is 540).
  -- タイムゾーン：UTC より何分進んでいるか（+09:00 なら 540）
  data Zone = Zone Int

  let utc = Zone 0
  let offset z = case z of | Zone m -> m
  let toMillis t = case t of | Time ms -> ms
  let fromMillis ms = Time ms
  -- ANCHOR_END: timeData

  -- ANCHOR: timeMath
  -- Move a time, and the seconds or days from b to a (a - b, cut
  -- down: 1.9 days is 1).
  -- 時刻をずらす。b から a まで何秒・何日か（a - b。端数は切り捨て）
  let addSeconds n t = Time (toMillis t + n * 1000)
  let addDays n t = addSeconds (n * 86400) t
  let diffSeconds a b = (toMillis a - toMillis b) // 1000
  let diffDays a b = diffSeconds a b // 86400
  -- ANCHOR_END: timeMath
  """
-- ANCHOR_END: time

-- ANCHOR: path
pathSource :: Text
pathSource = """
  module Path exposing (..)

  -- join, basename, dirname, extension, stem and withExtension are all
  -- builtins (Haskell's filepath). They touch no disk.
  -- どれも組み込み（Haskell の filepath）。ディスクには触れない
  """
-- ANCHOR_END: path

-- ANCHOR: file
fileSource :: Text
fileSource = """
  module File exposing
    ( FileInfo, size, modified
    , list, glob, exists, isDir, info
    , makeDir, remove, rename, copy, append )

  -- ANCHOR: fileInfo
  -- The size in bytes, and when the file was last changed. A record
  -- now: info.size, info.modified (the functions are kept, too).
  -- 大きさ（バイト）と、最後に変えた時刻。いまはレコード（関数も残す）
  type alias FileInfo = { size : Int, modified : Time }

  size i = i.size
  modified i = i.modified
  -- ANCHOR_END: fileInfo
  """
-- ANCHOR_END: file

-- ANCHOR: process
processSource :: Text
processSource = """
  module Process exposing (Outcome (..), run, pipe, output)

  -- ANCHOR: outcome
  -- How a command ended: it could not start (no such command), it
  -- ended with an exit code that is not 0, or it ended with 0. The
  -- fields have names now: no more guessing which String is which.
  -- コマンドの終わり方：始まれなかった（そんなコマンドはない）、0 でない
  -- 終了コードで終わった、0 で終わった。いまはフィールドに名前がある。
  -- どの String がどれかを、もう覚えなくてよい
  data Outcome
    = CannotStart String
    | Failed { code : Int, stdout : String, stderr : String }
    | Succeeded { stdout : String, stderr : String }
  -- ANCHOR_END: outcome

  -- ANCHOR: run
  -- A command with nothing for its standard input.
  -- 標準入力に何も渡さないコマンド
  run : String -> [String] -> IO Outcome
  run command args = pipe command args ""

  -- Only the output, or why there is none.
  -- 出力だけ。なければ、そのわけ
  output : Outcome -> Result String String
  output o =
    case o of
      | Succeeded { stdout } -> Ok stdout
      | Failed { code, stderr } -> Err f"exit {code}: {String.trim stderr}"
      | CannotStart why -> Err why
  -- ANCHOR_END: run
  """
-- ANCHOR_END: process

-- ANCHOR: random
randomSource :: Text
randomSource = """
  module Random exposing (int, pick, shuffle)

  -- One of the list, or Nothing for [].
  -- リストの1つ。[] なら Nothing
  let pick xs =
    case xs of
      | [] -> pure Nothing
      | _ -> do
          i <- int 0 (length xs - 1)
          pure (case drop i xs of
            | y : _ -> Just y
            | [] -> Nothing)

  -- The same list in a new order: pick one, then shuffle the rest.
  -- 同じリストを、新しい順に。1つ選び、残りを混ぜる
  let shuffle xs =
    case xs of
      | [] -> pure []
      | _ -> do
          i <- int 0 (length xs - 1)
          case drop i xs of
            | [] -> pure []
            | y : after -> do
                rest <- shuffle (take i xs ++ after)
                pure (y : rest)
  """
-- ANCHOR_END: random

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
