-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR: all
-- | The prelude: Lam's standard library, written in Lam itself, now
--   cut into modules (Maybe, List, Result, Prelude, String, Map, Set,
--   Float, Json, IO, Time, Path, File, Process, Random). Every file
--   starts with the default imports.
-- プレリュード：Lam で書いた、Lam の標準ライブラリ。いまはモジュールに
-- 分けてある。どのファイルも、はじめの import から始まる。
module Lam.Prelude
  ( standardSources
  , defaultImports
  , formatNames
  , hiddenNames
  , buildPrelude
  , preludeSession
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty, eof)

import Lam.Module (importInto, standardModule)
import Data.Maybe (isJust)

import Lam.Parser
  ( fixitiesOf, importLine, runLamParser, spaces, fmtName, reName, shName
  , recvName )
import Lam.Program
import Lam.Sugar
  (builtinOperator, doAp, doBind, doFinally, doMap, doPure, tryBranch)
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
  , ("List",    listSource)
  , ("Result",  resultSource)
  , ("Prelude", basicsSource)
  , ("String",  stringSource)
  , ("Map",     mapSource)
  , ("Set",     setSource)
  , ("Float",   floatSource)
  , ("Json",    jsonSource)
  -- ANCHOR: filesSources
  , ("Time",    timeSource)
  -- ANCHOR: ioAfterTime
  -- IO comes after Time: IO.timed reads the clock.
  -- IO は Time のあと：IO.timed が時計を見る
  , ("IO",      ioSource)
  -- ANCHOR_END: ioAfterTime
  , ("Path",    pathSource)
  -- ANCHOR_END: filesSources
  -- ANCHOR: streamsSources
  , ("Bytes",   bytesSource)
  -- ANCHOR_END: streamsSources
  , ("File",    fileSource)
  , ("Process", processSource)
  , ("Random",  randomSource)
  -- ANCHOR: streamsSourcesAfter
  , ("Hash",    hashSource)
  , ("Base64",  base64Source)
  , ("Uuid",    uuidSource)
  -- ANCHOR_END: streamsSourcesAfter
  -- ANCHOR: regexSources
  , ("Regex",   regexSource)
  -- ANCHOR_END: regexSources
  -- ANCHOR: parserSources
  , ("Parser",  parserSource)
  -- ANCHOR_END: parserSources
  -- ANCHOR: decodeSources
  , ("Decode",  decodeSource)
  , ("Csv",     csvSource)
  , ("Toml",    tomlSource)
  -- ANCHOR_END: decodeSources
  -- ANCHOR: outsideSources
  , ("Http",    httpSource)
  -- ANCHOR: serverSources
  , ("Response", responseSource)
  , ("Route",   routeSource)
  -- ANCHOR_END: serverSources
  , ("Sql",     sqlSource)
  -- ANCHOR_END: outsideSources
  -- ANCHOR: cliSources
  , ("Cli",     cliSource)
  -- ANCHOR_END: cliSources
  -- ANCHOR: shellSources
  , ("Shell",   shellSource)
  , ("Term",    termSource)
  -- ANCHOR_END: shellSources
  -- ANCHOR: chanSources
  , ("Chan",    chanSource)
  , ("Actor",   actorSource)
  , ("Send",    sendSource) ]
  -- ANCHOR_END: chanSources
-- ANCHOR_END: sources

-- ANCHOR: defaultImports
-- 📥 What every file has before its first line, one line for each
--    standard module. A file writes its own import only for another
--    name (as) or more names without the module (the list in
--    parentheses).
-- どのファイルにも、1行目の前にあるもの（標準のモジュールごとに1行）。
-- 自分で import を書くのは、別の名前（as）や、モジュールの名前なしで
-- 使う名前（かっこの一覧）がほしいときだけ
defaultImports :: Name -> Text
defaultImports m = case m of
  "Maybe"   -> "import Maybe (Maybe (..))"
  "Result"  -> "import Result (Result (..))"
  "Prelude" -> "import Prelude (..)"
  "Json"    -> "import Json (Json (..), ToJson (..), ToKey (..))"
  -- ANCHOR: importIO
  "IO"      -> "import IO (putStr, putStrLn, getLine, readFile, writeFile, "
                 <> "getArgs, lookupEnv, getEnvironment, getContents, "
                 <> "ePutStr, ePutStrLn, exit, print, when)"
  -- ANCHOR_END: importIO
  -- ANCHOR: importFiles
  "Time"    -> "import Time (Time, Zone)"
  "File"    -> "import File (FileInfo, Handle)"
  "Process" -> "import Process (Outcome (..))"
  -- ANCHOR_END: importFiles
  -- ANCHOR: importBytes
  "Bytes"   -> "import Bytes (Decoding (..))"
  "Hash"    -> "import Hash (Hasher)"
  -- ANCHOR_END: importBytes
  -- ANCHOR: importRegex
  "Regex"   -> "import Regex (Match, (=~))"
  -- ANCHOR_END: importRegex
  -- ANCHOR: importParser
  "Parser"  -> "import Parser (ParseError)"
  -- ANCHOR_END: importParser
  -- ANCHOR: importDecode
  "Decode"  -> "import Decode (Decoder, Problem)"
  "Csv"     -> "import Csv (Column)"
  -- ANCHOR_END: importDecode
  -- ANCHOR: importOutside
  "Http"    -> "import Http (Response, HttpError (..))"
  "Sql"     -> "import Sql (Db)"
  -- ANCHOR_END: importOutside
  -- ANCHOR: importServer
  "Route"   -> "import Route (Route, Params)"
  -- ANCHOR_END: importServer
  -- ANCHOR: importCli
  "Cli"     -> "import Cli (FromArg (..))"
  -- ANCHOR_END: importCli
  -- ANCHOR: importShell
  "Shell"   -> "import Shell (Cmd, ToArgs (..), (.|))"
  -- ANCHOR_END: importShell
  -- ANCHOR: importChan
  "Actor"   -> "import Actor (Down (..))"
  "Send"    -> "import Send (Sendable (..), (!))"
  -- ANCHOR_END: importChan
  _         -> "import " <> m
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

-- ANCHOR: list
listSource :: Text
listSource = """
  module List exposing (..)

  -- ANCHOR: listBasics
  -- Lists, taken apart with case. These are for lists only; outside,
  -- their names start with List. The classes of the Prelude make the
  -- same names work on other containers too.
  -- リスト。case で分ける。リストだけのもので、外からは名前が List. で
  -- 始まる。Prelude のクラスが、同じ名前をほかの入れ物にも効くようにする
  let foldr f z xs =
    case xs of
      | [] -> z
      | x : rest -> f x (foldr f z rest)
  let foldl f z xs =
    case xs of
      | [] -> z
      | x : rest -> foldl f (f z x) rest
  let map f xs = foldr (\\x rest -> f x : rest) [] xs
  let reverse xs = foldl (\\acc x -> x : acc) [] xs
  let take n xs =
    if n < 1 then [] else
      case xs of
        | [] -> []
        | x : rest -> x : take (n - 1) rest
  let drop n xs =
    if n < 1 then xs else
      case xs of
        | [] -> []
        | _ : rest -> drop (n - 1) rest
  -- ANCHOR_END: listBasics
  -- ANCHOR: listFolds
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
  -- ANCHOR_END: listFolds

  -- ANCHOR: listSort
  -- Merge sort: two sorted lists into one, then the whole list.
  -- マージソート：並んだ2つのリストを1つに、それからリスト全体を
  let merge before xs ys =
    case (xs, ys) of
      | ([], _) -> ys
      | (_, []) -> xs
      | (x : xr, y : yr) ->
          if before y x then y : merge before xs yr
          else x : merge before xr ys
  let sortBy before xs =
    if length xs < 2 then xs else
      let half = length xs // 2 in
      merge before (sortBy before (take half xs))
        (sortBy before (drop half xs))
  -- ANCHOR_END: listSort
  """
-- ANCHOR_END: list

-- ANCHOR: result
resultSource :: Text
resultSource = """
  module Result exposing
    (Result (..), map, bind, andThen, fromMaybe, mapError)

  -- A result that may be an error
  -- エラーかもしれない結果
  data Result e a = Ok a | Err e

  -- ANCHOR: resultMore
  -- The same three as Maybe: an Err goes on to the end as it is.
  -- Maybe と同じ3つ。Err は、そのまま最後まで運ばれる
  let map f r =
    case r of
      | Ok x -> Ok (f x)
      | Err e -> Err e
  let bind r f =
    case r of
      | Ok x -> f x
      | Err e -> Err e
  let andThen f r = bind r f
  -- A Maybe as a Result: Nothing becomes Err e (as Elm's).
  -- Maybe を Result に。Nothing は Err e になる（Elm と同じ）
  let fromMaybe e m =
    case m of
      | Just x -> Ok x
      | Nothing -> Err e
  -- ANCHOR_END: resultMore
  -- ANCHOR: mapError
  -- Change the error, and leave an Ok as it is (as Elm's).
  -- エラーのほうを変える。Ok はそのまま（Elm と同じ）
  let mapError f r =
    case r of
      | Ok x -> Ok x
      | Err e -> Err (f e)
  -- ANCHOR_END: mapError
  """
-- ANCHOR_END: result

-- ANCHOR: basics
basicsSource :: Text
basicsSource = """
  module Prelude exposing (..)

  -- ANCHOR: preludeOperators
  -- The operators of Lam: each one is another name for a function.
  -- 9 is the tightest, 0 the loosest. infixl leans left (1 - 2 - 3 is
  -- (1 - 2) - 3), infixr leans right, infix does not chain (a == b == c
  -- is an error).
  -- Lam の演算子：どれも、関数の別名。9 がいちばん強く、0 がいちばん弱い。
  -- infixl は左に寄り、infixr は右に寄り、infix は続けて書けない
  infixr 9 composeRight as >>
  infixl 9 compose as <<
  infixl 8 mul as *
  infixl 8 div as /
  infixl 8 intDiv as //
  infixl 8 divMaybe as /?
  infixl 8 intDivMaybe as //?
  infixl 7 add as +
  infixl 7 sub as -
  infixr 6 append as ++
  infixr 6 cons as :
  infixr 5 orElse as ?:
  infix  3 eq as ==
  infix  3 notEq as /=
  infix  3 lessThan as <
  infix  3 lessThanOrEq as <=
  infix  3 greaterThan as >
  infix  3 greaterThanOrEq as >=
  infixr 2 and as &&
  infixr 1 or as ||
  infixl 0 pipe as |>
  infixr 0 apply as <|
  -- ANCHOR_END: preludeOperators

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
  -- 📦 Foldable: a container whose items can be folded, one by one.
  --    The same class as Haskell's: f is a type with a hole (List,
  --    Maybe, Map k, Set), and f a holds the items of type a.
  -- Foldable：中身を1つずつたためる入れ物。Haskell と同じクラス。
  -- f は穴のある型（List、Maybe、Map k、Set）で、f a が a の中身を持つ
  class Foldable f where
    foldr : (a -> b -> b) -> b -> f a -> b
    toList : f a -> [a]
    toList t = foldr (\\x xs -> x : xs) [] t

  instance Foldable List where
    foldr f z xs = List.foldr f z xs
    toList xs = xs

  instance Foldable Maybe where
    foldr f z m =
      case m of
        | Nothing -> z
        | Just x -> f x z

  -- A map folds its values, in the order of the keys (like Haskell).
  -- Map は、値をキーの順にたたむ（Haskell と同じ）
  instance Foldable (Map k) where
    foldr f z m = List.foldr f z (Map.values m)
    toList m = Map.values m

  instance Foldable Set where
    foldr f z s = List.foldr f z (Set.toList s)
    toList s = Set.toList s
  -- ANCHOR_END: foldable

  -- ANCHOR: foldableTools
  -- Any container: make it a list with toList, then ask the list
  -- versions above.  sum (Just 3) is 3, and maximum m is the biggest
  -- value of a map.
  -- どんな入れ物でも、toList でリストにしてから、上のリストの版に頼む
  let foldl f z t = List.foldl f z (toList t)
  let null t = List.null (toList t)
  let any p t = List.any p (toList t)
  let all p t = List.all p (toList t)
  let elem y t = List.any (\\x -> x == y) (toList t)
  let sum t = List.sum (toList t)
  let product t = List.product (toList t)
  let maximum t = List.maximum (toList t)
  let minimum t = List.minimum (toList t)
  let find p t = List.find p (toList t)
  let concatMap f t = List.foldr (\\x rest -> f x ++ rest) [] (toList t)
  -- ANCHOR_END: foldableTools

  -- ANCHOR: functor
  -- 🎁 Functor: map changes each item, and keeps the shape of the
  --    container (the keys of a map, the Err of a Result).
  -- Functor：map は中身を1つずつ変え、入れ物の形はそのまま（Map の
  -- キー、Result の Err）
  class Functor f where
    map : (a -> b) -> f a -> f b

  instance Functor List where
    map f xs = List.map f xs

  instance Functor Maybe where
    map f m =
      case m of
        | Nothing -> Nothing
        | Just x -> Just (f x)

  instance Functor (Result e) where
    map f r =
      case r of
        | Ok x -> Ok (f x)
        | Err e -> Err e

  instance Functor (Map k) where
    map f m = Map.map f m
  -- ANCHOR_END: functor

  -- ANCHOR: monad
  -- ANCHOR: applicative
  -- 🧩 Applicative: pure puts a value in, with nothing else; ap gives
  --    the value in one box to the function in another. Neither box
  --    looks at what the other holds, so an ado is opened into these.
  -- Applicative：pure は値を、ほかに何もせずに入れる。ap は、箱の中の
  -- 関数に、別の箱の中の値を渡す。どちらの箱も、相手の中身を見ないので、
  -- ado はこの2つに開く
  class Functor f => Applicative f where
    pure : a -> f a
    ap : f (a -> b) -> f a -> f b
  -- ANCHOR_END: applicative

  -- ANCHOR: applicativeOperators
  -- f <$> a is map f a, and ff <*> a is ap ff a. Both lean left, at
  -- level 4: looser than ?:, tighter than ==.
  -- f <$> a は map f a、ff <*> a は ap ff a。どちらも左に寄り、強さは 4
  -- （?: より弱く、== より強い）
  infixl 4 map as <$>
  infixl 4 ap as <*>
  -- ANCHOR_END: applicativeOperators

  -- 🔗 Monad: bind takes the value out, and gives it to what comes
  --    next. A do is opened into bind, so a do works for each of
  --    these.
  -- Monad：bind は値を取り出し、続きに渡す。do は bind に開くので、
  -- どの仲間にも do が使える
  class Applicative m => Monad m where
    bind : m a -> (a -> m b) -> m b

  -- ANCHOR: applicativeInstances
  instance Applicative Maybe where
    pure x = Just x
    ap mf mx =
      case mf of
        | Nothing -> Nothing
        | Just f -> map f mx

  instance Applicative (Result e) where
    pure x = Ok x
    ap rf rx =
      case rf of
        | Err e -> Err e
        | Ok f -> map f rx

  -- Each function, with each value, in order.
  -- 関数ごとに、値を全部、順に
  instance Applicative List where
    pure x = [x]
    ap fs xs = concatMap (\\f -> List.map f xs) fs
  -- ANCHOR_END: applicativeInstances

  instance Monad Maybe where
    bind m f = Maybe.bind m f

  instance Monad (Result e) where
    bind r f = Result.bind r f

  -- Each item, then each of its answers, in order.
  -- 項目ごとに、その答えを全部、順に
  instance Monad List where
    bind xs f = concatMap f xs
  -- ANCHOR_END: monad

  -- ANCHOR: forEach
  -- 🔁 Do f for each item, one after another, and give (). The list
  --    comes first, as Haskell's forM_.
  -- 項目ごとに f を、順にやる。答えは ()。Haskell の forM_ と同じく、
  -- リストが先
  let forEach xs f =
    case xs of
      | [] -> pure ()
      | x : rest -> bind (f x) (\\_ -> forEach rest f)

  -- 🔁 The same, but keep the answers, in a list in the monad. For
  --    Maybe, one Nothing makes it all Nothing (Haskell's mapM).
  -- 同じだが、答えをモナドの中のリストにして残す。Maybe なら、1つでも
  -- Nothing なら全体が Nothing（Haskell の mapM）
  let mapM f xs =
    case xs of
      | [] -> pure []
      | x : rest ->
          bind (f x) (\\y -> bind (mapM f rest) (\\ys -> pure (y : ys)))
  -- ANCHOR_END: forEach

  -- ANCHOR: fallible
  -- 🛟 Fallible: a container that may be missing its value. toMaybe
  --    says what is inside, as a Maybe. m ?: d asks it: the value
  --    inside, or d (computed only then).
  -- Fallible：値がないかもしれない入れ物。toMaybe は、中にあるものを
  -- Maybe で言う。m ?: d はそれに聞く：中の値か、なければ d（そのとき
  -- だけ計算する）
  class Fallible f where
    toMaybe : f a -> Maybe a
    -- ANCHOR: branchMethod
    -- The value inside (Ok), or the failure, as a container of any
    -- type b (Err): a failure has no value, so it fits any b.
    -- 中の値（Ok）か、失敗を、どんな型 b の入れ物にもして（Err）。
    -- 失敗には値がないので、どの b にも合う
    branch : f a -> Result (f b) a
    -- ANCHOR_END: branchMethod

  instance Fallible Maybe where
    toMaybe m = m
    branch m =
      case m of
        | Just x -> Ok x
        | Nothing -> Err Nothing

  instance Fallible (Result e) where
    toMaybe r =
      case r of
        | Ok x -> Just x
        | Err _ -> Nothing
    branch r =
      case r of
        | Ok x -> Ok x
        | Err e -> Err (Err e)
  -- ANCHOR_END: fallible

  -- ANCHOR: sequence
  -- 🔤 Sequence: the same names for a string and a list. A string has
  --    no hole (it holds letters, not any a), so this class is about s
  --    itself, like Describe. slice is a default made of the others:
  --    from "from" up to (not with) "to"; a negative number counts
  --    from the end, as in Python.  slice 1 (-1) "Lam!" is "am".
  -- Sequence：文字列にもリストにも、同じ名前で。文字列には穴がない
  -- （どんな a でもなく文字を持つ）ので、Describe と同じく s そのものの
  -- クラス。slice はほかのメソッドで作る既定のメソッド。from から to の
  -- 手前まで。負の数は後ろから数える（Python と同じ）
  class HasLength s => Sequence s where
    reverse : s -> s
    take : Int -> s -> s
    drop : Int -> s -> s
    slice : Int -> Int -> s -> s
    slice from to s =
      let n = length s in
      let place k = max 0 (min n (if k < 0 then n + k else k)) in
      drop (place from) (take (place to) s)

  instance Sequence String where
    reverse s = String.reverse s
    take n s = String.take n s
    drop n s = String.drop n s

  instance Sequence [a] where
    reverse xs = List.reverse xs
    take n xs = List.take n xs
    drop n xs = List.drop n xs
  -- ANCHOR_END: sequence

  -- ANCHOR: splitAt
  -- Cut in two at n, a string or a list (Haskell's splitAt):
  --   splitAt 3 "Lam!" is ("Lam", "!")
  -- n のところで2つに切る。文字列でもリストでも（Haskell の splitAt）
  let splitAt n s = (take n s, drop n s)
  -- ANCHOR_END: splitAt

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
    List.foldl (\\seen x -> if elem x seen then seen else seen ++ [x])
      [] xs
  -- ANCHOR_END: lists

  -- ANCHOR: toolboxLists
  -- The items from the head, while they pass (Haskell's takeWhile and
  -- dropWhile).
  -- 頭から、合うあいだの要素（Haskell の takeWhile・dropWhile）
  let takeWhile p xs =
    case xs of
      | x : rest if p x -> x : takeWhile p rest
      | _ -> []
  let dropWhile p xs =
    case xs of
      | x : rest if p x -> dropWhile p rest
      | _ -> xs
  -- Next items that go with the first one of the group, together
  -- (Haskell's groupBy):
  --   groupBy (\\a b -> a == b) [1, 1, 2, 1] is [[1, 1], [2], [1]]
  -- 先頭と同じ組になる、となりの要素をまとめる（Haskell の groupBy）
  let groupBy same xs =
    case xs of
      | [] -> []
      | x : rest ->
          (x : takeWhile (same x) rest)
            : groupBy same (dropWhile (same x) rest)
  -- ANCHOR_END: toolboxLists

  -- ANCHOR: sort
  -- Sort by a key, the smallest first (List.sortBy, a merge sort). Two
  -- items with the same key keep their order.
  --   sortOn negate [3, 1, 2] is [3, 2, 1].
  -- キーで並べる。小さいものから（List.sortBy、マージソート）。同じキーの
  -- 2つは、元の順のまま
  let sortOn key xs = List.sortBy (\\a b -> key a < key b) xs
  let sort xs = sortOn id xs
  -- ANCHOR_END: sort

  let fromMaybe d m =
    case m of
      | Nothing -> d
      | Just x -> x
  let lookup k pairs =
    case pairs of
      | [] -> Nothing
      | (k2, v) : rest -> if k == k2 then Just v else lookup k rest

  -- ANCHOR: display
  -- 🖨️ Display: a value as text for people. {x} of f"..." calls it.
  --    Any type in Showable can join with one line (instance Display
  --    Money), and then it is shown as show shows it. A type can say
  --    how it wants to be shown, too: a string without its quotes,
  --    and JSON as JSON
  --    (in the module Json).
  -- Display：人に見せる文字。f"..." の {x} が呼ぶ。Showable の型なら、
  -- 1行（instance Display Money）で入れて、show と同じ形で見せる。
  -- 見せ方を自分で決めてもよい：文字列は引用符なしに、JSON は JSON に
  -- （モジュール Json で）
  class Showable a => Display a where
    display : a -> String
    display x = show x

  instance Display Int
  instance Display Float
  instance Display Bool
  instance Display String where
    display s = s
  instance Showable a => Display [a]
  instance (Showable a, Showable b) => Display (a, b)
  instance Showable a => Display (Maybe a)
  instance (Showable e, Showable a) => Display (Result e a)
  instance (Showable k, Showable v) => Display (Map k v)
  instance Showable a => Display (Set a)
  -- ANCHOR_END: display
  """
-- ANCHOR_END: basics

-- ANCHOR: string
stringSource :: Text
stringSource = """
  module String exposing (..)

  -- Strings: trim, split, reverse, ... are builtins (Haskell's
  -- Data.Text does the work). Here is one made of them.
  -- 文字列：trim・split・reverse などは組み込み（中身は Haskell の
  -- Data.Text）。ここには、それを組み合わせたものを書く
  let isEmpty s = s == ""
  """
-- ANCHOR_END: string

-- ANCHOR: map
mapSource :: Text
mapSource = """
  module Map exposing (..)

  -- ANCHOR: insertWith
  -- Maps: functions that take a function, made of the builtins
  -- Map：関数を受け取る関数を、組み込みの関数から作る
  let insertWith f k v m =
    case lookup k m of
      | Nothing -> insert k v m
      | Just old -> insert k (f v old) m
  -- ANCHOR_END: insertWith

  -- ANCHOR: mapMore
  -- Keep the values that pass (filterWithKey also sees the key)
  -- 合う値だけを残す（filterWithKey はキーも見る）
  let filterWithKey p m =
    fromList (Prelude.filter (\\(k, v) -> p k v) (toList m))
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
    List.foldr (\\(k, v) acc -> f k v acc) z (toList m)
  -- Two maps into one; f decides when both have the key.
  -- 2つの Map を1つに。両方にあるキーは f で決める
  let unionWith f a b =
    foldWithKey
      (\\k v acc -> insertWith (\\new old -> f old new) k v acc) a b
  -- ANCHOR_END: mapMore
  """
-- ANCHOR_END: map

-- ANCHOR: setFloat
-- Set and Float are all builtins: their modules show them, and that
-- is all.
-- Set と Float は、どれも組み込み。モジュールは、それを見せるだけ
setSource :: Text
setSource = """
  module Set exposing (..)
  """

floatSource :: Text
floatSource = """
  module Float exposing (..)
  """
-- ANCHOR_END: setFloat

-- ANCHOR: json
jsonSource :: Text
jsonSource = """
  module Json exposing
    ( Json (JNull, JBool, JInt, JStr, JArr, JObj)
    , get, at, int, string, bool, array, mkDec, decimal
    , parse, show, pretty, toFloat, fromFloat
    , ToJson (..), ToKey (..), encode )

  -- ANCHOR: json
  -- JSON, as a data type. A number with a fraction is JDec:
  -- JDec 1999 2 is 1999 divided by 10 to the 2, that is 19.99.
  -- JSON を data の型で。小数は JDec：JDec 1999 2 は 19.99
  data Json
    = JNull
    | JBool Bool
    | JInt Int
    | JDec Int Int
    | JStr String
    | JArr [Json]
    | JObj (Map String Json)
  -- ANCHOR_END: json

  -- ANCHOR: jsonTools
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
  -- ANCHOR_END: jsonTools

  -- ANCHOR: mkDec
  -- Make a number with a fraction, in the same shape as parse
  -- makes it: the 0s at the end of c move into d, and a whole number
  -- is JInt.  mkDec 19990 3 is JDec 1999 2, and
  -- mkDec 1900 2 is JInt 19.
  -- 小数を、parse と同じ形で作る。c の後ろの 0 は d へ移し、
  -- 整数なら JInt にする
  let mkDec c d =
    if c == 0 then JInt 0
    else if mod c 10 == 0 then mkDec (c // 10) (d - 1)
    else if d > 0 || d < -1024 then JDec c d
    else JInt (c * pow 10 (-d))
  -- ANCHOR_END: mkDec

  -- ANCHOR: decimal
  -- Look inside a number with a fraction:  decimal (mkDec 1999 2)  is
  -- Just (1999, 2). Anything else (a whole number too) is Nothing.
  -- 小数の中を見る。ほかのもの（整数も）は Nothing
  let decimal j = case j of | JDec c d -> Just (c, d) | _ -> Nothing
  -- ANCHOR_END: decimal

  -- ANCHOR: displayJson
  -- JSON is shown as JSON. (Here, show is the builtin Json.show: the
  -- builtins of Json have no Json. in front inside it.)
  -- JSON は JSON として見せる（ここの show は、組み込みの Json.show。
  -- Json の組み込みは、中では Json. を付けない）
  instance Display Json where
    display j = show j
  -- ANCHOR_END: displayJson

  -- ANCHOR: toJsonClass
  -- 📤 Any value to JSON with one name, toJson. The builtin types, the
  --    lists, Maybe (Nothing is null), the pairs (an array of two),
  --    Json itself, and a Map or a Set have instances; a type of your
  --    own joins with an instance.
  -- どんな値も、1つの名前 toJson で JSON に。組み込みの型、リスト、Maybe
  -- （Nothing は null）、組（2つの配列）、Json そのもの、Map と Set に
  -- インスタンスがある。自分の型は、インスタンスを書けば仲間になる
  class ToJson a where
    toJson : a -> Json

  instance ToJson Json where
    toJson j = j

  instance ToJson Int where
    toJson n = JInt n

  instance ToJson Float where
    toJson x = fromFloat x

  instance ToJson String where
    toJson s = JStr s

  instance ToJson Bool where
    toJson b = JBool b

  instance ToJson a => ToJson [a] where
    toJson xs = JArr (map toJson xs)

  instance ToJson a => ToJson (Maybe a) where
    toJson m =
      case m of
        | Just x -> toJson x
        | Nothing -> JNull

  instance (ToJson a, ToJson b) => ToJson (a, b) where
    toJson pair = JArr [toJson (fst pair), toJson (snd pair)]

  instance ToJson a => ToJson (Set a) where
    toJson xs = JArr (map toJson (Set.toList xs))
  -- ANCHOR_END: toJsonClass

  -- ANCHOR: toKeyClass
  -- 🔑 A key of a JSON object is always a string. A Map can be written
  --    as JSON when its keys can be strings, and its values JSON.
  -- JSON のオブジェクトのキーは、いつも文字列。キーを文字列にでき、
  -- 値を JSON にできる Map なら、JSON に書ける
  class ToKey a where
    toKey : a -> String

  instance ToKey String where
    toKey s = s

  instance ToKey Int where
    toKey n = showInt n

  instance (ToKey k, ToJson v) => ToJson (Map k v) where
    toJson m =
      JObj (Map.fromList [(toKey k, toJson v) | (k, v) <- Map.toList m])
  -- ANCHOR_END: toKeyClass

  -- ANCHOR: encode
  -- 🖨️ A value as the text of JSON, at once (Python's json.dumps).
  -- 値を、すぐに JSON の文字に（Python の json.dumps）
  encode : ToJson a => a -> String
  encode x = show (toJson x)
  -- ANCHOR_END: encode
  """
-- ANCHOR_END: json

-- ANCHOR: io
ioSource :: Text
ioSource = """
  module IO exposing
    ( putStr, putStrLn, getLine, readFile, writeFile, andThen
    , finally, bracket, try
    , getArgs, lookupEnv, getEnvironment, getContents
    , ePutStr, ePutStrLn, exit
    , forEachLine, foldLines, print, when
    , all, allN, both, race, allOk, oks, settle, timeout, timed )

  -- ANCHOR: monadIO
  -- IO joins Functor, Applicative and Monad with its two builtins
  -- (here, without IO. in front: returnIO and bindIO).
  -- IO は、2つの組み込みで Functor・Applicative・Monad の仲間になる
  -- （ここでは IO. を付けずに returnIO と bindIO）
  instance Functor IO where
    map f m = bindIO m (\\x -> returnIO (f x))

  instance Applicative IO where
    pure x = returnIO x
    ap mf mx = bindIO mf (\\f -> bindIO mx (\\x -> returnIO (f x)))

  instance Monad IO where
    bind m f = bindIO m f
  -- ANCHOR_END: monadIO

  -- ANCHOR: ioAndThen
  -- The function first, as Maybe.andThen:  m |> IO.andThen f
  -- 関数が先。Maybe.andThen と同じ
  andThen : (a -> IO b) -> IO a -> IO b
  andThen f m = bind m f
  -- ANCHOR_END: ioAndThen

  -- ANCHOR: bracket
  -- 🧹 Get a thing, use it, and give it back: the giving back is
  --    done even when the use stops (as Haskell's bracket). use r is
  --    made only when it is done (after returnIO ()), so a stop while
  --    making it is inside finally too.
  -- 借りて、使って、返す。使うところで止まっても、返すのはやる
  -- （Haskell の bracket と同じ）。use r は、やるときに作る
  -- （returnIO () のあと）ので、作る途中で止まっても finally の内側
  bracket : IO r -> (r -> IO b) -> (r -> IO c) -> IO c
  bracket acquire release use =
    bindIO acquire (\\r ->
      finally (bindIO (returnIO ()) (\\_ -> use r)) (release r))
  -- ANCHOR_END: bracket

  -- ANCHOR: printWhen
  -- 🖨️ Show any value on a line (a String without its quotes), and
  --    do an IO only when the condition holds. One-liners use them a
  --    lot:  when (line =~ m/ERROR/) (print line)
  -- どんな値も1行で見せる（String は " なしで）。条件が合うときだけ IO を
  -- やる。ワンライナーでよく使う
  print : Display a => a -> IO ()
  print x = putStrLn (display x)

  when : Bool -> IO () -> IO ()
  when ok io = if ok then io else pure ()
  -- ANCHOR_END: printWhen

  -- ANCHOR: stdinLines
  -- 📜 The lines of the standard input, one at a time, until the end:
  --    each one is let go before the next comes (unlike getContents).
  -- 標準入力の行を、終わりまで1行ずつ。次の行が来る前に、前の行は
  -- 手放す（getContents とちがう）
  forEachLine : (String -> IO ()) -> IO ()
  forEachLine act = do
    next <- getLine
    case next of
      | Nothing -> pure ()
      | Just line -> do
          act line
          forEachLine act

  foldLines : (b -> String -> b) -> b -> IO b
  foldLines f acc = do
    next <- getLine
    case next of
      | Nothing -> pure acc
      | Just line -> foldLines f (f acc line)
  -- ANCHOR_END: stdinLines

  -- ANCHOR: oks
  -- Wait for all, and keep only the answers that are Ok.
  -- 全部待って、Ok の答えだけ残す
  oks : [IO (Result e a)] -> IO [a]
  oks ios = do
    results <- all ios
    pure (concatMap okOnly results)
    where
      okOnly r = case r of
        | Ok x -> [x]
        | Err _ -> []

  -- Wait for all, and put the answers in two piles.
  -- 全部待って、答えを2つの山に分ける
  settle : [IO (Result e a)] -> IO { oks : [a], errs : [e] }
  settle ios = do
    results <- all ios
    pure (List.foldr pile { oks = [], errs = [] } results)
    where
      pile r piles = case r of
        | Ok x -> { piles | oks = x : piles.oks }
        | Err e -> { piles | errs = e : piles.errs }
  -- ANCHOR_END: oks

  -- ANCHOR: timeout
  -- The answer of io, or Nothing if it takes more than ms: a race
  -- with a clock.
  -- io の答え。ms より長くかかれば Nothing：時計との競走
  timeout : Int -> IO a -> IO (Maybe a)
  timeout ms io =
    race [ map Just io
         , map (\\_ -> Nothing) (sleep ms) ]
  -- ANCHOR_END: timeout

  -- ANCHOR: timed
  -- The answer of io, and how long it took, in seconds.
  -- io の答えと、かかった秒数
  timed : IO a -> IO { value : a, seconds : Float }
  timed io = do
    start <- Time.now
    x <- io
    end <- Time.now
    let ms = Time.toMillis end - Time.toMillis start
    pure { value = x, seconds = toFloat ms / 1000 }
  -- ANCHOR_END: timed
  """
-- ANCHOR_END: io

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

  utc : Zone
  utc = Zone 0

  offset : Zone -> Int
  offset z = case z of | Zone m -> m

  toMillis : Time -> Int
  toMillis t = case t of | Time ms -> ms

  fromMillis : Int -> Time
  fromMillis ms = Time ms
  -- ANCHOR_END: timeData

  -- ANCHOR: timeMath
  -- Move a time, and the seconds or days from b to a (a - b, cut
  -- down: 1.9 days is 1).
  -- 時刻をずらす。b から a まで何秒・何日か（a - b。端数は切り捨て）
  addSeconds : Int -> Time -> Time
  addSeconds n t = Time (toMillis t + n * 1000)

  addDays : Int -> Time -> Time
  addDays n t = addSeconds (n * 86400) t

  diffSeconds : Time -> Time -> Int
  diffSeconds a b = (toMillis a - toMillis b) // 1000

  diffDays : Time -> Time -> Int
  diffDays a b = diffSeconds a b // 86400
  -- ANCHOR_END: timeMath

  -- ANCHOR: displayTime
  -- {t} in f"..." shows a Time as the REPL does: 2026-11-17T03:00:00Z
  -- f"..." の {t} も、REPL と同じ形で見せる
  instance Display Time
  instance Display Zone
  -- ANCHOR_END: displayTime
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
    ( FileInfo, list, glob, exists, isDir, info
    , makeDir, remove, rename, copy, append
    , Handle, open, openWith, readLine, readChunk, close
    , withFile, withFileWith, foldLines, foldLinesWith, forEachLine
    , forLines, readBytes, writeBytes )

  -- The size in bytes, and when the file was last changed: what
  -- File.info gives.
  -- 大きさ（バイト）と、最後に変えた時刻。File.info がくれるもの
  type alias FileInfo = { size : Int, modified : Time }

  -- ANCHOR: handle
  -- 🎫 An open file: only a number the world gave. The constructor is
  --    hidden, so a Handle is never made from any number.
  -- 開いたファイル：世界がくれた番号だけ。構成子は隠すので、どんな数
  -- からでも Handle を作れる、ということはない
  data Handle = Handle Int

  open : String -> IO (Result String Handle)
  open path = openWith Strict path

  -- The next line, or Nothing at the end. A line that is not UTF-8
  -- stops the program when the file was opened Strict (Python's
  -- UnicodeDecodeError); Replace and Ignore never stop.
  -- 次の行。終わりなら Nothing。Strict で開いたファイルの、UTF-8 でない
  -- 行は、プログラムを止める（Python の UnicodeDecodeError）。Replace と
  -- Ignore は止まらない
  readLine : Handle -> IO (Maybe String)
  readLine h = do
    next <- nextLine h
    case next of
      | Ok line -> pure line
      | Err why -> error why

  -- At most n bytes, or Nothing at the end.
  -- 多くても n バイト。終わりなら Nothing
  readChunk : Int -> Handle -> IO (Maybe Bytes)
  readChunk n h = do
    next <- nextChunk n h
    case next of
      | Ok piece -> pure piece
      | Err why -> error why
  -- ANCHOR_END: handle

  -- ANCHOR: withFile
  -- 🧹 Open, use, and close, even when the use stops: ? and defer of
  --    the chapter on errors.
  -- 開いて、使って、閉じる。使うところで止まっても、閉じる。失敗を
  -- 早めに返す章の ? と defer で
  withFileWith
    : Decoding -> String -> (Handle -> IO a) -> IO (Result String a)
  withFileWith how path use = do
    h <- openWith how path ?
    defer close h
    answer <- use h
    pure (Ok answer)

  withFile : String -> (Handle -> IO a) -> IO (Result String a)
  withFile path use = withFileWith Strict path use
  -- ANCHOR_END: withFile

  -- ANCHOR: foldLines
  -- 🔁 Go down the lines with a value: step gets the value so far and
  --    a line, and gives the next value. Only one line is held at a
  --    time.
  -- 値を持ちながら、行を下っていく。step は、ここまでの値と1行を受け取り、
  -- 次の値を返す。持つのは、いつも1行だけ
  eachLine : Handle -> b -> (b -> String -> IO b) -> IO b
  eachLine h acc step = do
    next <- readLine h
    case next of
      | Nothing -> pure acc
      | Just line -> do
          acc2 <- step acc line
          eachLine h acc2 step

  foldLinesWith
    : Decoding -> (b -> String -> b) -> b -> String -> IO (Result String b)
  foldLinesWith how f start path =
    withFileWith how path (\\h ->
      eachLine h start (\\acc line -> pure (f acc line)))

  foldLines : (b -> String -> b) -> b -> String -> IO (Result String b)
  foldLines f start path = foldLinesWith Strict f start path

  forEachLine : String -> (String -> IO ()) -> IO (Result String ())
  forEachLine path act =
    withFile path (\\h -> eachLine h () (\\_ line -> act line))
  -- ANCHOR_END: foldLines

  -- ANCHOR: forLines
  -- Go down the lines of h, from the line number n.
  -- h の行を、番号 n の行から下っていく
  numbered : Handle -> Int -> b -> (b -> Int -> String -> IO b) -> IO b
  numbered h n acc step = do
    next <- readLine h
    case next of
      | Nothing -> pure acc
      | Just line -> do
          acc2 <- step acc n line
          numbered h (n + 1) acc2 step

  -- 🔢 The same, with the number of each line (from 1, as Perl's $.),
  --    and broken bytes read as how says. "-" is the standard input.
  --    One-liners are made of it (lam -n, -p).
  -- 同じだが、行の番号（1 から。Perl の $. と同じ）も渡し、壊れた
  -- バイトは how のとおりに読む。"-" は標準入力。ワンライナー（lam -n・-p）
  -- は、これでできている
  forLines
    : Decoding -> String -> b -> (b -> Int -> String -> IO b)
    -> IO (Result String b)
  forLines how path start step =
    withFileWith how path (\\h -> numbered h 1 start step)
  -- ANCHOR_END: forLines
  """
-- ANCHOR_END: file

-- ANCHOR: process
processSource :: Text
processSource = """
  module Process exposing (Outcome (..), run, pipe, output)

  -- ANCHOR: outcome
  -- How a command ended: it could not start (no such command), it
  -- ended with an exit code that is not 0, or it ended with 0. out
  -- and err are the standard output and the standard error.
  -- コマンドの終わり方：始まれなかった（そんなコマンドはない）、0 でない
  -- 終了コードで終わった、0 で終わった。out と err は、標準出力と
  -- 標準エラー
  data Outcome
    = CannotStart String
    | Failed { code : Int, out : String, err : String }
    | Succeeded { out : String, err : String }
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
      | Succeeded { out } -> Ok out
      | Failed { code, err } ->
          Err ("exit " ++ show code ++ ": " ++ String.trim err)
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
  pick : [a] -> IO (Maybe a)
  pick xs =
    case xs of
      | [] -> pure Nothing
      | _ -> do
          i <- int 0 (length xs - 1)
          pure (case drop i xs of
            | y : _ -> Just y
            | [] -> Nothing)

  -- The same list in a new order: pick one, then shuffle the rest.
  -- 同じリストを、新しい順に。1つ選び、残りを混ぜる
  shuffle : [a] -> IO [a]
  shuffle xs =
    case xs of
      | [] -> pure []
      | _ -> do
          i <- int 0 (length xs - 1)
          case drop i xs of
            | [] -> pure []
            | y : others -> do
                rest <- shuffle (take i xs ++ others)
                pure (y : rest)
  """
-- ANCHOR_END: random

-- ANCHOR: bytes
bytesSource :: Text
bytesSource = """
  module Bytes exposing
    ( Decoding (..), fromString, toString, decode
    , toList, fromList, toHex, fromHex, slice, concat )

  -- ANCHOR: decodingData
  -- What to do with bytes that are not UTF-8: stop (Strict), put a
  -- U+FFFD there (Replace), or leave them out (Ignore). Python's
  -- errors="strict", "replace" and "ignore".
  -- UTF-8 でないバイトをどうするか：止まる（Strict）、U+FFFD を置く
  -- （Replace）、とばす（Ignore）。Python の errors= の3つ
  data Decoding = Strict | Replace | Ignore
  -- ANCHOR_END: decodingData

  -- {b} in f"..." shows it as the REPL does: <3 bytes: e38182>
  -- f"..." の {b} も、REPL と同じ形で見せる
  instance Display Bytes
  """
-- ANCHOR_END: bytes

-- ANCHOR: hashModule
hashSource :: Text
hashSource = """
  module Hash exposing (Hasher, sha256, start, add, finish, file)

  -- The work of SHA-256 so far (hidden: only start and add make one).
  -- ここまでの SHA-256 の途中（隠す。作れるのは start と add だけ）
  data Hasher = Hasher Bytes

  -- ANCHOR: hashFile
  -- 🔏 The SHA-256 of a file, 64 KiB at a time: a file of any size
  --    needs only 64 KiB of memory.
  -- ファイルの SHA-256 を、64 KiB ずつ。どんなに大きいファイルでも、
  -- メモリは 64 KiB ですむ
  chunks : Handle -> Hasher -> IO Bytes
  chunks h hasher = do
    next <- File.readChunk 65536 h
    case next of
      | Nothing -> pure (finish hasher)
      | Just piece -> chunks h (add piece hasher)

  file : String -> IO (Result String Bytes)
  file path = File.withFile path (\\h -> chunks h start)
  -- ANCHOR_END: hashFile
  """
-- ANCHOR_END: hashModule

-- ANCHOR: base64Module
base64Source :: Text
base64Source = """
  module Base64 exposing (..)

  -- encode and decode are builtins (Haskell's base64-bytestring).
  -- どちらも組み込み（Haskell の base64-bytestring）
  """
-- ANCHOR_END: base64Module

-- ANCHOR: uuid
uuidSource :: Text
uuidSource = """
  module Uuid exposing (v4)

  -- 0x40 to 0x4f is 0100xxxx; 0x80 to 0xbf is 10xxxxxx.
  -- 0x40〜0x4f は 0100xxxx、0x80〜0xbf は 10xxxxxx
  byteAt : Int -> IO Int
  byteAt i =
    if i == 6 then Random.int 64 79
    else if i == 8 then Random.int 128 191
    else Random.int 0 255

  -- 🆔 A random UUID (version 4, RFC 9562): 16 random bytes, but the
  --    7th starts with 4 (the version), and the 9th with 8, 9, a or b
  --    (the variant). Its dice are Random's, so --seed=618 gives the
  --    same one every time.
  -- ランダムな UUID（バージョン4、RFC 9562）：16個のランダムなバイト。
  -- ただし7つ目は 4 で始まり（バージョン）、9つ目は 8・9・a・b の
  -- どれかで始まる（バリアント）。さいころは Random のものなので、
  -- --seed=618 なら毎回同じ
  v4 : IO String
  v4 = do
    numbers <- mapM byteAt (range 0 15)
    let hex = Bytes.toHex (Bytes.fromList numbers)
    pure (join "-"
      [ slice 0 8 hex, slice 8 12 hex, slice 12 16 hex
      , slice 16 20 hex, slice 20 32 hex ])
  """
-- ANCHOR_END: uuid

-- ANCHOR: regexModule
regexSource :: Text
regexSource = """
  module Regex exposing
    ( Match, compile, matches, find, findAll, split
    , replace, replaceAll, replaceWith, replaceFirstWith
    , group, named, matchedBy, (=~) )

  -- ANCHOR: matchAlias
  -- 🎁 What a regex found: the text, where it starts (in letters, from
  --    0), the groups 1, 2, ... ("" for a group that took no part),
  --    and the named groups.
  -- 見つけたもの：その文字、始まる場所（0 から数えた文字の数）、
  -- グループ 1, 2, …（使われなかったグループは ""）、名前付きのグループ
  type alias Match =
    { text : String
    , start : Int
    , groups : [String]
    , named : Map String String }
  -- ANCHOR_END: matchAlias

  -- ANCHOR: group
  -- The i-th item of a list, from 0 (not shown outside).
  -- リストの i 番目（0 から。外には見せない）
  nth : Int -> [a] -> Maybe a
  nth i xs =
    case xs of
      | [] -> Nothing
      | x : rest -> if i == 0 then Just x else nth (i - 1) rest

  -- 🔢 Group n: 0 is the whole match, as m.group(0) in Python.
  --    Nothing for a group the regex does not have.
  -- n 番のグループ。0 は全体（Python の m.group(0) と同じ）。
  -- 正規表現にないグループは Nothing
  group : Int -> Match -> Maybe String
  group n m =
    if n == 0 then Just m.text else nth (n - 1) m.groups

  -- 🏷️ The group named k:  (?<year>[0-9]+)  is  named "year" m.
  -- k という名前のグループ
  named : String -> Match -> Maybe String
  named k m = Map.lookup k m.named
  -- ANCHOR_END: group

  -- ANCHOR: matchedBy
  -- 🐪 line =~ re: does re match somewhere in line? The same as
  --    matches re line, with the text first, as Perl writes it. =~ is
  --    as strong as ==, and comes in by the default import.
  -- line =~ re：re が line のどこかに合うか。matches re line と同じだが、
  -- Perl と同じく文字が先。=~ は == と同じ強さで、はじめの import で来る
  matchedBy : String -> Regex -> Bool
  matchedBy text regex = matches regex text

  infix 3 matchedBy as =~
  -- ANCHOR_END: matchedBy
  """
-- ANCHOR_END: regexModule

-- ANCHOR: parserModule
parserSource :: Text
parserSource = """
  module Parser exposing
    ( ParseError, run, string, satisfy, int, spaces, end, fail
    , oneOf, many, some, sepBy, optional, lazy, label, regex
    , andThen, token, symbol, between )

  -- ANCHOR: parseErrorAlias
  -- 🧾 Why a string did not fit: the line and column (from 1), what
  --    was found and wanted, and all of it with the line (to show).
  -- 文字列が合わなかったわけ：行と列（1 から）、見つけたものと欲しかった
  -- もの、それを行と一緒に見せる全体
  type alias ParseError =
    { line : Int
    , column : Int
    , message : String
    , pretty : String }
  -- ANCHOR_END: parseErrorAlias

  -- ANCHOR: monadParser
  -- A Parser joins Functor, Applicative and Monad with its three
  -- builtins (here, without Parser. in front). Then a do reads one
  -- line after another.
  -- Parser は、3つの組み込みで Functor・Applicative・Monad の仲間に
  -- なる（ここでは Parser. を付けずに）。これで、do が1行ずつ読んでいく
  instance Functor Parser where
    map f p = mapParser f p

  -- ap reads the function with pf, then the value with px after it.
  -- ap は、pf で関数を読み、そのあとを px で読んで値を渡す
  instance Applicative Parser where
    pure x = pureParser x
    ap pf px = bindParser pf (\\f -> mapParser f px)

  instance Monad Parser where
    bind p f = bindParser p f
  -- ANCHOR_END: monadParser

  -- The function first, as Maybe.andThen:  p |> Parser.andThen f
  -- 関数が先。Maybe.andThen と同じ
  andThen : (a -> Parser b) -> Parser a -> Parser b
  andThen f p = bind p f

  -- ANCHOR: parserTools
  -- 🧰 Small tools, made of the builtins with do.
  -- do で、組み込みのパーサーから作る小さな道具

  -- p, then the spaces after it:  Parser.token Parser.int
  -- p と、その後ろの空白
  token : Parser a -> Parser a
  token p = do
    x <- p
    spaces
    pure x

  -- This text, then the spaces after it:  Parser.symbol "("
  -- この文字と、その後ろの空白
  symbol : String -> Parser String
  symbol s = token (string s)

  -- p between two others:  Parser.between "(" ")" p
  -- 2つの文字ではさまれた p
  between : String -> String -> Parser a -> Parser a
  between open close p = do
    symbol open
    x <- p
    symbol close
    pure x
  -- ANCHOR_END: parserTools
  """
-- ANCHOR_END: parserModule

-- ANCHOR: decodeModule
decodeSource :: Text
decodeSource = """
  -- ANCHOR: decodeExposing
  module Decode exposing
    ( Decoder, Problem, run, fromString, explain
    , succeed, fail, int, float, string, bool, null, value
    , field, at, index, list, dict, nullable, optionalField
    , oneOf, lazy, map2, map3, map4, andMap, andThen
    , collect, keyStep, Decodable (..) )
  -- ANCHOR_END: decodeExposing

  -- ANCHOR: problemAlias
  -- 🩹 One thing that did not fit: where (the path, as jq writes it:
  --    .orders[2].price) and what was wrong.
  -- 合わなかったところ1つ：どこで（jq の書き方の道筋）、何がだめか
  type alias Problem = { path : String, message : String }
  -- ANCHOR_END: problemAlias

  -- ANCHOR: decoderData
  -- 🔍 A decoder: a function from Json to a value, or to all the
  --    problems it found. The constructor is not shown outside, so a
  --    decoder is made only of the tools below.
  -- Decoder：Json から値へ（だめなら、見つけた問題ぜんぶへ）の関数。
  -- 構成子は外に見せないので、下の道具からしか作れない
  data Decoder a = Decoder (Json -> Result [Problem] a)

  -- Use a decoder on a Json.
  -- Json に Decoder を使う
  run : Decoder a -> Json -> Result [Problem] a
  run (Decoder f) j = f j
  -- ANCHOR_END: decoderData

  -- ANCHOR: decodeBasics
  -- One problem, here (the path is put in front on the way out).
  -- ここでの問題1つ（道筋は、外へ出ていくときに前に足していく）
  problem : String -> Result [Problem] a
  problem m = Err [{ path = "", message = m }]

  -- What was found, made short:  expected an Int, got "12"
  -- 見つけたもの（長ければ短く）
  got : Json -> String
  got j =
    if length s > 30 then String.take 27 s ++ "..." else s
    where
      s = Json.show j

  expected : String -> Json -> Result [Problem] a
  expected what j = problem f"expected {what}, got {got j}"

  -- 🧱 The smallest decoders: one for each kind of JSON value.
  -- いちばん小さな Decoder。JSON の値の種類ごとに1つ
  int : Decoder Int
  int = Decoder (\\j -> case j of
                         | JInt n -> Ok n
                         | _ -> expected "an Int" j)

  float : Decoder Float
  float = Decoder (\\j -> case Json.toFloat j of
                           | Just x -> Ok x
                           | Nothing -> expected "a number" j)

  string : Decoder String
  string = Decoder (\\j -> case j of
                            | JStr s -> Ok s
                            | _ -> expected "a String" j)

  bool : Decoder Bool
  bool = Decoder (\\j -> case j of
                          | JBool b -> Ok b
                          | _ -> expected "a Bool" j)

  -- null gives x:  Decode.null 0
  -- null なら x
  null : a -> Decoder a
  null x = Decoder (\\j -> case j of
                            | JNull -> Ok x
                            | _ -> expected "null" j)

  -- The Json as it is.
  -- Json をそのまま
  value : Decoder Json
  value = Decoder (\\j -> Ok j)

  -- Always x, and always this problem.
  -- いつも x。いつもこの問題
  succeed : a -> Decoder a
  succeed x = Decoder (\\_ -> Ok x)

  fail : String -> Decoder a
  fail m = Decoder (\\_ -> problem m)
  -- ANCHOR_END: decodeBasics

  -- ANCHOR: decodeField
  -- 🧭 A step of the path: .price, or ["a key"] when the key is not
  --    a plain name.
  -- 道筋の1歩。ふつうの名前でないキーは ["a key"]
  keyStep : String -> String
  keyStep k =
    if k /= "" && all plain (chars k) then "." ++ k else f"[{show k}]"
    where
      plain c =
        (c >= "a" && c <= "z") || (c >= "A" && c <= "Z")
          || (c >= "0" && c <= "9") || c == "_"

  -- Put a step in front of the path of every problem.
  -- 問題ぜんぶの道筋の前に、1歩を足す
  under : String -> Result [Problem] a -> Result [Problem] a
  under s r =
    Result.mapError (map (\\p -> { p | path = s ++ p.path })) r

  -- 🔑 The field k of an object, read with d.
  -- オブジェクトの項目 k を、d で読む
  field : String -> Decoder a -> Decoder a
  field k d =
    Decoder (\\j -> case j of
      | JObj m -> inside m
      | _ -> expected "an object" j)
    where
      inside m =
        case Map.lookup k m of
          | Just x -> under (keyStep k) (run d x)
          | Nothing -> problem f"missing field {show k}"

  -- Fields inside fields:  Decode.at ["shop", "tax"] Decode.int
  -- 項目の中の項目
  at : [String] -> Decoder a -> Decoder a
  at ks d = List.foldr field d ks

  -- The i-th item of an array, from 0.
  -- 配列の i 番目（0 から）
  index : Int -> Decoder a -> Decoder a
  index i d =
    Decoder (\\j -> case j of
      | JArr xs -> inside xs
      | _ -> expected "an array" j)
    where
      inside xs =
        case drop i xs of
          | x : _ if i >= 0 -> under f"[{i}]" (run d x)
          | _ -> problem f"no item [{i}] in {length xs} items"
  -- ANCHOR_END: decodeField

  -- ANCHOR: decodeCollect
  -- 🧺 All the values, or all the problems: one bad item does not hide
  --    the others.
  -- 値ぜんぶか、問題ぜんぶ。1つだめでも、ほかのものを隠さない
  -- The problems of one result (none for an Ok).
  -- 結果1つの問題（Ok なら、なし）
  problems : Result [Problem] a -> [Problem]
  problems r = case r of
                 | Err ps -> ps
                 | Ok _ -> []

  collect : [Result [Problem] a] -> Result [Problem] [a]
  collect rs =
    List.foldr
      (\\r rest -> case (r, rest) of
                    | (Ok x, Ok xs) -> Ok (x : xs)
                    | _ -> Err (problems r ++ problems rest))
      (Ok [])
      rs

  -- 📚 Every item of an array, read with d.
  -- 配列の要素ぜんぶを、d で読む
  list : Decoder a -> Decoder [a]
  list d =
    Decoder (\\j -> case j of
      | JArr xs ->
          collect [ under f"[{i}]" (run d x) | (i, x) <- indexed xs ]
      | _ -> expected "an array" j)

  -- 🗂️ Every value of an object, read with d, by its key.
  -- オブジェクトの値ぜんぶを、キーごとに d で読む
  dict : Decoder a -> Decoder (Map String a)
  dict d =
    Decoder (\\j -> case j of
      | JObj m ->
          Result.map Map.fromList (collect
            [ Result.map (\\x -> (k, x)) (under (keyStep k) (run d x))
            | (k, x) <- Map.toList m ])
      | _ -> expected "an object" j)
  -- ANCHOR_END: decodeCollect

  -- ANCHOR: decodeMaybe
  -- 🕳️ null is Nothing, anything else is read with d.
  -- null は Nothing。ほかは d で読む
  nullable : Decoder a -> Decoder (Maybe a)
  nullable d =
    Decoder (\\j -> case j of
      | JNull -> Ok Nothing
      | _ -> Result.map Just (run d j))

  -- A field that may be missing (or null): Nothing then.
  -- なくてもよい（null でもよい）項目。そのときは Nothing
  optionalField : String -> Decoder a -> Decoder (Maybe a)
  optionalField k d =
    Decoder (\\j -> case j of
      | JObj m -> inside m
      | _ -> expected "an object" j)
    where
      inside m =
        case Map.lookup k m of
          | Just x -> under (keyStep k) (run (nullable d) x)
          | Nothing -> Ok Nothing

  -- 🔀 The first decoder that fits. If none does, the problems of
  --    all of them.
  -- 最初に合う Decoder。どれも合わなければ、全部の問題
  firstOf : [Decoder a] -> Json -> [Problem] -> Result [Problem] a
  firstOf ds j seen =
    case ds of
      | [] ->
          if length seen == 0 then problem "oneOf of no decoders"
          else Err seen
      | d : rest ->
          case run d j of
            | Ok x -> Ok x
            | Err ps -> firstOf rest j (seen ++ ps)

  oneOf : [Decoder a] -> Decoder a
  oneOf ds = Decoder (\\j -> firstOf ds j [])

  -- A decoder that points at itself:  Decode.lazy (\\_ -> tree)
  -- 自分を指す Decoder
  lazy : (() -> Decoder a) -> Decoder a
  lazy f = Decoder (\\j -> run (f ()) j)
  -- ANCHOR_END: decodeMaybe

  -- ANCHOR: monadDecoder
  -- 🔗 Functor, Applicative and Monad: then do reads one field after
  --    another. A do stops at the first problem: the next line may
  --    need the value. But ap (and so <*> and ado) runs both
  --    decoders, and gives back the problems of both.
  -- Functor・Applicative・Monad の仲間。do で、項目を1つずつ読める。
  -- do は、最初の問題で止まる（次の行が、その値を使うかもしれないから）。
  -- でも ap（だから <*> と ado も）は、両方の Decoder を動かし、両方の
  -- 問題を返す
  instance Functor Decoder where
    map f d = Decoder (\\j -> Result.map f (run d j))

  instance Applicative Decoder where
    pure x = succeed x
    ap df dx =
      Decoder (\\j -> case (run df j, run dx j) of
                       | (Ok f, Ok x) -> Ok (f x)
                       | (rf, rx) -> Err (problems rf ++ problems rx))

  instance Monad Decoder where
    bind d f =
      Decoder (\\j -> case run d j of
                       | Ok x -> run (f x) j
                       | Err ps -> Err ps)

  -- The function first, as Maybe.andThen.
  -- 関数が先。Maybe.andThen と同じ
  andThen : (a -> Decoder b) -> Decoder a -> Decoder b
  andThen f d = bind d f
  -- ANCHOR_END: monadDecoder

  -- ANCHOR: decodeMap2
  -- 🤝 Two decoders, side by side: both are always run, so the
  --    problems of both come back. The second does not need the
  --    value of the first, so it need not wait for it.
  -- 2つの Decoder を並べる。いつも両方を動かすので、両方の問題が
  -- 返ってくる。2つ目は1つ目の値を使わないので、待たなくてよい
  both : (a -> b -> c) -> Result [Problem] a -> Result [Problem] b
         -> Result [Problem] c
  both f ra rb =
    case (ra, rb) of
      | (Ok a, Ok b) -> Ok (f a b)
      | _ -> Err (problems ra ++ problems rb)

  map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
  map2 f da db = Decoder (\\j -> both f (run da j) (run db j))

  -- One more argument for the function in the decoder (Elm's
  -- andMap):  Decode.succeed f |> Decode.andMap d1 |> Decode.andMap d2
  -- Decoder の中の関数に、引数をもう1つ（Elm の andMap）
  andMap : Decoder a -> Decoder (a -> b) -> Decoder b
  andMap da df = map2 (\\f a -> f a) df da

  map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c
         -> Decoder d
  map3 f da db dc = map2 f da db |> andMap dc

  map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b
         -> Decoder c -> Decoder d -> Decoder e
  map4 f da db dc dd = map3 f da db dc |> andMap dd
  -- ANCHOR_END: decodeMap2

  -- ANCHOR: decodeFromString
  -- 📖 Read JSON text with a decoder. Text that is not JSON is one
  --    problem at the top.
  -- JSON の文字を、Decoder で読む。JSON でなければ、てっぺんの問題1つ
  fromString : Decoder a -> String -> Result [Problem] a
  fromString d text =
    case Json.parse text of
      | Ok j -> run d j
      | Err e -> problem f"not JSON: {e}"

  -- 🗒️ The problems, one in a line, as jq writes the paths (. is the
  --    top).
  -- 問題を1行に1つ。道筋は jq の書き方（. がてっぺん）
  explain : [Problem] -> String
  explain ps = join "\\n" (map line ps)
    where
      place path =
        if path == "" then "."
        else if String.startsWith "[" path then "." ++ path
        else path
      line p = f"{place p.path}: {p.message}"
  -- ANCHOR_END: decodeFromString

  -- ANCHOR: decodable
  -- 🎯 A type that can be read from JSON has a decoder: the type
  --    chooses which one (lam --as=Order uses it). The same as the
  --    FromJson we made in the chapter on decoders, under the name
  --    Swift gives it. A type of our own joins with an instance.
  -- JSON から読める型は、Decoder を持つ。どれを使うかは型が選ぶ
  -- （lam --as=Order が使う）。Decoder の章で作った FromJson と同じもの
  -- を、Swift の名前で。自分の型は instance で仲間に
  class Decodable a where
    decoder : Decoder a

  instance Decodable Int where
    decoder = int

  instance Decodable Float where
    decoder = float

  instance Decodable String where
    decoder = string

  instance Decodable Bool where
    decoder = bool

  instance Decodable Json where
    decoder = value

  instance Decodable a => Decodable [a] where
    decoder = list decoder

  instance Decodable a => Decodable (Maybe a) where
    decoder = nullable decoder
  -- ANCHOR_END: decodable
  """
-- ANCHOR_END: decodeModule

-- ANCHOR: csvModule
csvSource :: Text
csvSource = """
  module Csv exposing
    ( Column, parse, read, decode, text, int, number, bool, optional )

  -- ANCHOR: csvColumn
  -- 🔧 How to read the string of one column. A column that is not in
  --    the table stays a string (text).
  -- 1つの列の文字列の読み方。表にない列は、文字のまま（text）
  type alias Column = String -> Result String Json

  text : Column
  text s = Ok (JStr s)

  -- Whole numbers:  " 12 " is 12, "007" is 7
  -- 整数
  int : Column
  int s =
    case String.toInt (String.trim s) of
      | Just n -> Ok (JInt n)
      | Nothing -> Err f"not an Int: {show s}"

  -- Any number, kept exact as JSON keeps it:  "19.99" is JDec 1999 2
  -- どんな数でも。JSON と同じく、ぴったり持つ
  number : Column
  number s =
    case Json.parse s of
      | Ok j if Json.toFloat j /= Nothing -> Ok j
      | _ -> Err f"not a number: {show s}"

  bool : Column
  bool s =
    case String.trim s of
      | "true" -> Ok (JBool true)
      | "false" -> Ok (JBool false)
      | _ -> Err f"not true or false: {show s}"

  -- An empty field is null; anything else is read with c.
  -- 空のフィールドは null。ほかは c で読む
  optional : Column -> Column
  optional c s = if String.trim s == "" then Ok JNull else c s
  -- ANCHOR_END: csvColumn

  -- ANCHOR: csvTable
  -- 📍 Where a problem is: the line of the file, and the column.
  -- 問題の場所：ファイルの行と、列
  problemAt : Int -> String -> String -> Problem
  problemAt line column m =
    { path = f"line {line}, column {column}", message = m }

  -- The line of a row: the line its first field starts on.
  -- 行の場所：最初のフィールドが始まる行
  lineOf : [(Int, String)] -> Int
  lineOf cells =
    case cells of
      | (n, _) : _ -> n
      | [] -> 0

  -- A row as long as the header.
  -- 見出しと同じ長さの行
  fits : [String] -> [(Int, String)] -> Result [Problem] [(Int, String)]
  fits header cells =
    if length cells == length header then Ok cells
    else Err [{ path = f"line {lineOf cells}", message = m }]
    where
      m = f"{length cells} fields, but the header has {length header}"

  -- 📄 The rows of CSV: the first one is the header, and each field of
  --    the others has the line of the file it starts on. numberedRows
  --    is the builtin (cassava reads; the lines are counted by the same
  --    rules, quotes with line breaks too).
  -- CSV の行。1行目は見出し。ほかの行のフィールドには、それが始まる
  -- ファイルの行を付ける。numberedRows は組み込み（読むのは cassava。
  -- 行は同じ決まりで数える。改行の入った引用符も）
  table : String -> Result [Problem] ([String], [[(Int, String)]])
  table s =
    case numberedRows s of
      | Err e -> Err [{ path = "", message = f"not CSV: {e}" }]
      | Ok [] -> Ok ([], [])
      | Ok (header : body) -> Ok (map snd header, body)
  -- ANCHOR_END: csvTable

  -- ANCHOR: csvParse
  -- 🗒️ Every row as a Map from the header to the field (a string).
  -- どの行も、見出しからフィールド（文字）への Map
  parse : String -> Result [Problem] [Map String String]
  parse s = do
    (header, body) <- table s
    Decode.collect
      [ Result.map (\\cells -> Map.fromList (zip header (map snd cells)))
          (fits header row)
      | row <- body ]
  -- ANCHOR_END: csvParse

  -- ANCHOR: csvRead
  -- Every column of the table is in the header.
  -- 表の列が、どれも見出しにあるか
  noneMissing : Map String Column -> [String] -> Result [Problem] ()
  noneMissing columns header =
    case [ k | (k, _) <- Map.toList columns, not (elem k header) ] of
      | [] -> Ok ()
      | missing ->
          Err [ { path = "", message = f"missing column {show k}" }
              | k <- missing ]

  -- One row as an object. A field that its Column cannot read is a
  -- problem at its own line and column.
  -- 1行を、オブジェクトに。Column で読めないフィールドは、その行と列の
  -- 問題
  object : Map String Column -> [String] -> [(Int, String)]
           -> Result [Problem] Json
  object columns header cells =
    Result.map (\\kvs -> JObj (Map.fromList kvs))
      (Decode.collect [ cell k field | (k, field) <- zip header cells ])
    where
      cell k field =
        case (Map.lookup k columns, field) of
          | (Nothing, (_, s)) -> Ok (k, JStr s)
          | (Just c, (n, s)) ->
              Result.map (\\j -> (k, j))
                (Result.mapError (\\m -> [problemAt n k m]) (c s))

  -- Every row as an object, then used by use (with the line of the
  -- row). The problems of every row, all at once.
  -- どの行もオブジェクトにして、use で使う（行の番号も渡す）。どの行の
  -- 問題も、まとめて全部
  eachRow : Map String Column -> (Int -> Json -> Result [Problem] a)
            -> String -> Result [Problem] [a]
  eachRow columns use s = do
    (header, body) <- table s
    noneMissing columns header
    Decode.collect
      [ fits header row
          |> Result.andThen (object columns header)
          |> Result.andThen (use (lineOf row))
      | row <- body ]

  -- 🔧 Every row as a JSON object, each field read by the Column of
  --    its header (a string if it has none). All the problems at once.
  -- どの行も JSON のオブジェクトに。フィールドは、見出しの Column で
  -- 読む（なければ文字のまま）。問題は、まとめて全部
  read : Map String Column -> String -> Result [Problem] Json
  read columns s = Result.map JArr (eachRow columns (\\_ j -> Ok j) s)

  -- A problem a decoder found in the row on this line: .price is the
  -- column price.
  -- この行で、Decoder が見つけた問題。.price は、列 price
  placed : Int -> Problem -> Problem
  placed line p =
    if p.path == "" then { p | path = f"line {line}" }
    else if String.startsWith "." p.path then
      { p | path = f"line {line}, column {String.drop 1 p.path}" }
    else { p | path = f"line {line}, column {p.path}" }

  -- 🎯 Read every row with a decoder, after the columns. A problem
  --    says the line of the file, not the number of the row.
  -- 列を読んだあと、どの行も Decoder で読む。問題は、行の番号ではなく
  -- ファイルの行を言う
  decode : Map String Column -> Decoder a -> String
           -> Result [Problem] [a]
  decode columns d s =
    eachRow columns
      (\\line j -> Result.mapError (map (placed line)) (Decode.run d j)) s
  -- ANCHOR_END: csvRead
  """
-- ANCHOR_END: csvModule

-- ANCHOR: tomlModule
tomlSource :: Text
tomlSource = """
  module Toml exposing (parse, decode)

  -- 🎯 Read TOML text with a decoder. parse is the builtin: the
  --    document as Json.
  -- TOML の文字を Decoder で読む。parse は組み込み（文書を Json に）
  decode : Decoder a -> String -> Result [Problem] a
  decode d s =
    case parse s of
      | Ok j -> Decode.run d j
      | Err e -> Err [{ path = "", message = f"not TOML: {e}" }]
  """
-- ANCHOR_END: tomlModule

-- ANCHOR: http
-- ANCHOR: httpHead
httpSource :: Text
httpSource = """
  module Http exposing
    ( Response, HttpError (..)
    , send, request, get, post, getAs
    , header, string, json, expectJson, response, errorText
    , Request, serve, segments, bodyAs )
  -- ANCHOR_END: httpHead

  -- ANCHOR: response
  -- 📬 An answer: the status (200, 404, ...), the headers, and the
  --    body. Http.send (a builtin) makes it. The names of the headers
  --    are in small letters. The body is bytes: an image is not text.
  --    Http.string reads it as text (UTF-8).
  -- 答え：状態コード（200、404 など）、ヘッダー、本文。作るのは組み込みの
  -- Http.send。ヘッダーの名前は小文字。本文はバイト列（画像は文字では
  -- ない）。文字として読むのは Http.string（UTF-8）
  type alias Response =
    { status : Int, headers : Map String String, body : Bytes }

  header : String -> Response -> Maybe String
  header name r = Map.lookup (toLower name) r.headers
  -- ANCHOR_END: response

  -- ANCHOR: httpError
  -- 🚧 What went wrong. e is the body of a failed answer (404, 500,
  --    ...), read by the second reader of request.
  -- うまくいかなかったわけ。e は、失敗した答え（404、500 など）の本文で、
  -- request の2つ目の読み方で読む
  data HttpError e
    = NoAnswer String        -- no answer came (no network, ...)
    | BadStatus Int e        -- the answer was 404, 500, ...
    | BadBody String         -- the body could not be read (bad JSON)
  -- ANCHOR_END: httpError

  -- ANCHOR: readers
  -- 📖 Readers: how to read the body of an answer.
  -- 読み方：答えの本文を、どう読むか
  string : Response -> Result String String
  string r = Bytes.toString r.body

  json : Response -> Result String Json
  json r = string r |> Result.andThen Json.parse

  response : Response -> Result String Response
  response r = Ok r
  -- ANCHOR_END: readers

  -- ANCHOR: expectJson
  -- 🔍 Read the body with a decoder (Elm's Http.expectJson). The
  --    problems become one string: Decode.explain, a line for each.
  -- 本文を Decoder で読む（Elm の Http.expectJson）。問題は1つの文字に
  -- する：Decode.explain で、1つにつき1行
  expectJson : Decoder a -> Response -> Result String a
  expectJson d r =
    string r |> Result.andThen (\\s ->
      Decode.fromString d s |> Result.mapError Decode.explain)
  -- ANCHOR_END: expectJson

  -- ANCHOR: request
  -- 🧭 The long one: choose how to read a good answer (2xx), and how
  --    to read a bad one (4xx, 5xx).
  -- 長いほう：よい答え（2xx）の読み方と、悪い答え（4xx・5xx）の読み方を
  -- 選ぶ
  request
    : (Response -> Result String a) -> (Response -> Result String e)
      -> String -> String -> Map String String -> String
      -> IO (Result (HttpError e) a)
  request readOk readBad method url hs text = do
    answer <- send method url hs text
    r <- pure (Result.mapError NoAnswer answer) ?
    if r.status >= 200 && r.status < 300 then
      pure (Result.mapError BadBody (readOk r))
    else
      pure (case readBad r of
        | Ok e -> Err (BadStatus r.status e)
        | Err e -> Err (BadBody e))
  -- ANCHOR_END: request

  -- ANCHOR: short
  -- ✂️ The short ones: the error is a String. get gives a Json, getAs
  --    reads it with a decoder.
  -- 短いほう：エラーは String。get は Json を、getAs は Decoder で
  -- 読んだものをくれる
  errorText : HttpError String -> String
  errorText e =
    case e of
      | NoAnswer m -> m
      | BadStatus s text -> f"status {s}: {text}"
      | BadBody m -> m

  get : String -> IO (Result String Json)
  get url = do
    answer <- request json string "GET" url {} ""
    pure (Result.mapError errorText answer)

  getAs : Decoder a -> String -> IO (Result String a)
  getAs d url = do
    answer <- request (expectJson d) string "GET" url {} ""
    pure (Result.mapError errorText answer)

  post : String -> Json -> IO (Result String Json)
  post url j = do
    answer <-
      request json string "POST" url
        {"content-type": "application/json"} (Json.show j)
    pure (Result.mapError errorText answer)
  -- ANCHOR_END: short

  -- ANCHOR: requestAlias
  -- 📨 A request from the web, given to the handler of Http.serve.
  --    serve (a builtin) waits for them, and does the handler for each
  --    one as a job of its own.
  -- Web からの頼み。Http.serve の handler に渡される。待つのは組み込みの
  -- serve で、1つごとに handler を自分の仕事としてやる
  type alias Request =
    { method : String
    , path : String
    , query : Map String String
    , headers : Map String String
    , body : String }
  -- ANCHOR_END: requestAlias

  -- ANCHOR: segments
  -- ✂️ The pieces of a path: "/todos/3" is ["todos", "3"].
  -- 道のかけら："/todos/3" は ["todos", "3"]
  segments : String -> [String]
  segments path = [s | s <- String.split "/" path, s /= ""]

  -- 📥 Read the body of a request with a decoder (the same as the
  --    body of an answer, with expectJson).
  -- 頼みの本文を Decoder で読む（答えの本文を expectJson で読むのと
  -- 同じ）
  bodyAs : Decoder a -> Request -> Result String a
  bodyAs d req =
    Decode.fromString d req.body |> Result.mapError Decode.explain
  -- ANCHOR_END: segments
  """
-- ANCHOR_END: http

-- ANCHOR: sql
sqlSource :: Text
sqlSource = """
  module Sql exposing
    ( Db, open, openReadOnly, query, exec, close
    , queryAs, withDb, transaction, column )

  -- ANCHOR: db
  -- 🗄️ An open database. The constructor is hidden: only Sql.open
  --    makes it.
  -- 開いたデータベース。構成子は隠す：作れるのは Sql.open だけ
  data Db = Db Int

  -- One column of a row, if it is there.
  -- 行の、1つの列（あれば）
  column : String -> Map String Json -> Maybe Json
  column name row = Map.lookup name row
  -- ANCHOR_END: db

  -- ANCHOR: queryAs
  -- 🔍 The rows of a SELECT, read with a decoder: each row is a JSON
  --    object, so the decoders of the chapter on decoding work as they
  --    are. A problem says the row: [2].tmax.
  -- SELECT の行を Decoder で読む。行は JSON のオブジェクトなので、
  -- データを読む章の Decoder が、そのまま使える。問題は行も言う：
  -- [2].tmax
  queryAs
    : Decoder a -> Db -> String -> [Json] -> IO (Result String [a])
  queryAs d db sql args = do
    rows <- query db sql args ?
    pure (Decode.run (Decode.list d) (JArr (map JObj rows))
            |> Result.mapError Decode.explain)
  -- ANCHOR_END: queryAs

  -- ANCHOR: withDb
  -- 🧹 Open, use, and close, even when the use stops. Unlike
  --    File.withFile, the use gives a Result too, so that it can use ?
  --    inside: its Err comes out as it is.
  -- 開いて、使って、閉じる。使うところで止まっても閉じる。File.withFile
  -- とちがって、use も Result を返す。中で ? を使えるように：use の Err
  -- は、そのまま外に出る
  withDb : String -> (Db -> IO (Result String a)) -> IO (Result String a)
  withDb file use = do
    db <- open file ?
    defer close db
    use db
  -- ANCHOR_END: withDb

  -- ANCHOR: transaction
  -- 🤝 All of it, or none of it: BEGIN, then COMMIT when the work gives
  --    Ok. When it gives Err, or stops, ROLLBACK: the table is as it
  --    was before BEGIN.
  -- 全部やるか、何もしないか。BEGIN のあと、仕事が Ok なら COMMIT。
  -- Err か、止まったら ROLLBACK：表は BEGIN の前のまま
  transaction : Db -> IO (Result String a) -> IO (Result String a)
  transaction db work = do
    exec db "BEGIN" [] ?
    outcome <- IO.try work
    case outcome of
      | Ok (Ok x) -> do
          exec db "COMMIT" [] ?
          pure (Ok x)
      | Ok (Err e) -> do
          exec db "ROLLBACK" [] ?
          pure (Err e)
      | Err stop -> do
          exec db "ROLLBACK" [] ?
          pure (Err stop)
  -- ANCHOR_END: transaction
  """
-- ANCHOR_END: sql

-- ANCHOR: cli
cliSource :: Text
cliSource = """
  module Cli exposing (FromArg (..))

  -- ANCHOR: fromArg
  -- 🔤 How to read one word of the command line as an a. An Err says
  --    what it needs:  --count needs a whole number, not "abc"
  --    A type with an instance can be the type of an option.
  -- コマンドラインの語1つを、a として読む方法。Err は、何が要るかを言う。
  -- インスタンスのある型は、オプションの型にできる
  class FromArg a where
    fromArg : String -> Result String a

  instance FromArg String where
    fromArg s = Ok s

  instance FromArg Int where
    fromArg s = Result.fromMaybe "a whole number" (String.toInt s)

  instance FromArg Float where
    fromArg s = Result.fromMaybe "a number" (String.toFloat s)
  -- ANCHOR_END: fromArg
  """
-- ANCHOR_END: cli

-- ANCHOR: shell
shellSource :: Text
shellSource = """
  module Shell exposing
    ( Cmd, ToArgs (..), command, glue, pipeTo, (.|)
    , within, withEnv, withInput, run, out, lines, ok, render )

  -- ANCHOR: pipeOperator
  -- a .| b: the output of a is the input of b.
  -- a の出力を、b の入力に
  infixl 2 pipeTo as .|
  -- ANCHOR_END: pipeOperator

  -- ANCHOR: cmd
  -- 🐚 A command not run yet: only a value. One is one command, with
  --    its folder ("" is here), the environment variables to add, and
  --    its input. Pipe joins two. The constructors are hidden: $"..."
  --    and the functions below make it.
  -- まだ動かしていないコマンド：ただの値。One はコマンド1つで、フォルダー
  -- （"" はここ）、足す環境変数、入力を持つ。Pipe は2つをつなぐ。構成子は
  -- 隠す。作るのは $"…" と、下の関数
  data Cmd
    = One { program : String, args : [String], dir : String
          , env : Map String String, input : String }
    | Pipe Cmd Cmd
  -- ANCHOR_END: cmd

  -- ANCHOR: toArgs
  -- 🔤 The words a value is in $"...": one for a string or a number,
  --    one for each item of a list, none for Nothing.
  -- 値が $"…" の中でなる語：文字列と数は1つ、リストは要素ごとに1つ、
  -- Nothing は0
  class ToArgs a where
    toArgs : a -> [String]

  instance ToArgs String where
    toArgs s = [s]

  instance ToArgs Int where
    toArgs n = [show n]

  instance ToArgs Float where
    toArgs x = [show x]

  instance ToArgs a => ToArgs [a] where
    toArgs xs = concatMap toArgs xs

  instance ToArgs a => ToArgs (Maybe a) where
    toArgs m =
      case m of
        | Just x -> toArgs x
        | Nothing -> []
  -- ANCHOR_END: toArgs

  -- ANCHOR: command
  -- 🧱 The words, as a command: the first is its name.
  -- 語を、コマンドに。最初の語が名前
  command : [String] -> Cmd
  command words =
    case words of
      | name : rest ->
          One { program = name, args = rest, dir = "", env = Map.empty
              , input = "" }
      | [] -> One { program = "", args = [], dir = "", env = Map.empty
                  , input = "" }

  -- 🧩 The pieces of one word, glued: each word of a piece with each
  --    of the next.  glue [["--tag="], ["a", "b"]]  is
  --    ["--tag=a", "--tag=b"].
  -- 1つの語の部品をくっつける。部品の語どうしを、全部の組で
  glue : [[String]] -> [String]
  glue pieces =
    foldr (\\piece rest -> [p ++ r | p <- piece, r <- rest]) [""] pieces
  -- ANCHOR_END: command

  -- ANCHOR: change
  -- 📁 Run it in a folder (inside the folder it had, if it had one).
  -- フォルダーの中で動かす（もうフォルダーがあれば、その中で）
  within : String -> Cmd -> Cmd
  within folder c =
    case c of
      | One s -> One { s | dir = Path.join folder s.dir }
      | Pipe a b -> Pipe (within folder a) (within folder b)

  -- 🌱 Add environment variables (the new ones win).
  -- 環境変数を足す（新しいものが勝つ）
  withEnv : Map String String -> Cmd -> Cmd
  withEnv more c =
    case c of
      | One s -> One { s | env = Map.union more s.env }
      | Pipe a b -> Pipe (withEnv more a) (withEnv more b)

  -- 📥 Give a text to its input (to the first, for a pipe).
  -- 入力に文字を渡す（パイプなら、最初のコマンドに）
  withInput : String -> Cmd -> Cmd
  withInput text c =
    case c of
      | One s -> One { s | input = text }
      | Pipe a b -> Pipe (withInput text a) b

  -- a .| b
  pipeTo : Cmd -> Cmd -> Cmd
  pipeTo a b = Pipe a b
  -- ANCHOR_END: change

  -- ANCHOR: run
  -- ▶️ Run it, and say how it ended. A pipe runs the first, then gives
  --    its output to the next; if the first does not succeed, that is
  --    how the pipe ended (as set -o pipefail).
  -- 動かして、終わり方を言う。パイプは最初を動かし、その出力を次に渡す。
  -- 最初がうまくいかなければ、それがパイプの終わり方（set -o pipefail と
  -- 同じ）
  run : Cmd -> IO Outcome
  run c =
    case c of
      | One s -> if s.program == ""
                   then pure (CannotStart "a command needs a name")
                   else spawn s
      | Pipe a b -> do
          first <- run a
          case first of
            | Succeeded { out } -> run (withInput out b)
            | _ -> pure first
  -- ANCHOR_END: run

  -- ANCHOR: render
  -- 🖨️ The command as a person reads it:  git branch -d 'old fix'
  -- コマンドを、人が読む形で
  render : Cmd -> String
  render c =
    case c of
      | One s -> join " " (List.map quote (s.program : s.args))
      | Pipe a b -> render a ++ " | " ++ render b
  -- ANCHOR_END: render

  -- ANCHOR: out
  -- 📤 Only the output, without the line breaks at its end (as $(...)
  --    of a shell), or an Err that says which command and why.
  -- 出力だけ。最後の改行は取る（シェルの $(…) と同じ）。だめなら、どの
  -- コマンドが、なぜかを言う Err
  out : Cmd -> IO (Result String String)
  out c = do
    o <- run c
    pure (case o of
      | Succeeded { out } -> Ok (String.trimEnd out)
      | Failed { code, err } ->
          Err (render c ++ ": exit " ++ show code ++ ": " ++ String.trim err)
      | CannotStart why -> Err why)

  -- 📃 The lines of the output, without the empty ones.
  -- 出力の行。空の行は除く
  lines : Cmd -> IO (Result String [String])
  lines c = do
    text <- out c ?
    pure (Ok (filter (\\l -> String.trim l /= "") (String.split "\\n" text)))

  -- ✅ Only whether it went well.
  -- うまくいったかどうかだけ
  ok : Cmd -> IO (Result String ())
  ok c = do
    _ <- out c ?
    pure (Ok ())
  -- ANCHOR_END: out

  """
-- ANCHOR_END: shell

-- ANCHOR: term
termSource :: Text
termSource = """
  module Term exposing (..)

  -- ANCHOR: colors
  -- 🎨 Colors and styles: pure functions that put marks around a text.
  --    say takes them off again when they would only be noise.
  -- 色と見た目：文字の前後に印を付けるだけの純粋な関数。印がじゃまなだけの
  -- ときは、say が外す
  red s = style "31" s
  green s = style "32" s
  yellow s = style "33" s
  blue s = style "34" s
  bold s = style "1" s
  dim s = style "2" s
  -- ANCHOR_END: colors

  -- ANCHOR: say
  -- 🖍️ Colors only on a terminal, and not when NO_COLOR is set (and is
  --    not empty): a file or a pipe gets the plain text.
  -- 色は端末だけ。NO_COLOR があれば（空でなければ）出さない。ファイルや
  -- パイプには、印のない文字を
  colorful : IO Bool
  colorful = do
    tty <- isTerminal
    noColor <- lookupEnv "NO_COLOR"
    pure (tty && (noColor ?: "") == "")

  say : String -> IO ()
  say s = do
    c <- colorful
    putStrLn (if c then s else strip s)
  -- ANCHOR_END: say

  -- ANCHOR: table
  -- 📏 A text made as wide as n columns (wide letters count two).
  -- 文字を n 桁の幅にする（幅の広い文字は2桁と数える）
  padTo : Int -> String -> String
  padTo n s = s ++ String.repeat (n - width s) " "

  -- 📐 The widest cell of each column.
  -- 列ごとに、いちばん広いマス
  wider : [Int] -> [Int] -> [Int]
  wider a b =
    case (a, b) of
      | (x : xs, y : ys) -> max x y : wider xs ys
      | ([], ys) -> ys
      | (xs, []) -> xs

  widthsOf : [[String]] -> [Int]
  widthsOf rows =
    case rows of
      | [] -> []
      | row : rest -> wider (List.map width row) (widthsOf rest)

  -- 🗂️ Rows lined up in columns, under a bold header and a line.
  -- 行を列にそろえる。太字の見出しと線の下に
  table : [String] -> [[String]] -> String
  table header rows =
    join "\\n" (bold (line header) : rule : List.map line rows)
    where
      widths = widthsOf (header : rows)
      line row = String.trimEnd (join "  " (zipWith padTo widths row))
      rule = join "  " (List.map (\\w -> String.repeat w "-") widths)
  -- ANCHOR_END: table

  -- ANCHOR: ask
  -- ❓ Ask yes or no. Only y or yes is yes; the end of the input is no.
  --    The questions go to the standard error, as read -p of bash does:
  --    the output stays only the answer of the script.
  -- はいかいいえを聞く。y か yes だけが、はい。入力の終わりは、いいえ。
  -- 問いは標準エラーへ（bash の read -p と同じ）。出力は、スクリプトの
  -- 答えだけのまま
  confirm : String -> IO Bool
  confirm question = do
    ePutStr (question ++ " [y/N] ")
    answer <- getLine
    pure (elem (toLower (String.trim (answer ?: ""))) ["y", "yes"])

  -- The first item, if there is one.
  -- 最初の要素（あれば）
  first : [a] -> Maybe a
  first xs =
    case xs of
      | x : _ -> Just x
      | [] -> Nothing

  -- The choice with that number, if there is one.
  -- その番号の選ぶもの（あれば）
  chosen : [String] -> String -> Maybe String
  chosen choices a =
    case String.toInt (String.trim a) of
      | Just n -> if n >= 1 then first (drop (n - 1) choices) else Nothing
      | Nothing -> Nothing

  -- Ask for a number until it is one of the choices.
  -- 選ぶものの番号になるまで、番号を聞く
  pickFrom : String -> [String] -> IO (Maybe String)
  pickFrom question choices = do
    ePutStr (question ++ " ")
    answer <- getLine
    case answer of
      | Nothing -> pure Nothing
      | Just a -> case chosen choices a of
          | Just c -> pure (Just c)
          | Nothing -> do
              ePutStrLn f"(a number from 1 to {length choices})"
              pickFrom question choices

  -- 🔢 Show the choices with numbers, and read a number (again, until
  --    it is one of them). Nothing at the end of the input.
  -- 選ぶものを番号つきで見せ、番号を読む（選ぶものの番号になるまで、
  -- もう一度）。入力の終わりなら Nothing
  select : String -> [String] -> IO (Maybe String)
  select question choices = do
    forEach (indexed choices) (\\pair -> ePutStrLn f"{fst pair + 1}) {snd pair}")
    pickFrom question choices

  -- 🤫 A line typed with its letters not shown, as for a password.
  -- 字を見せずに打つ1行（パスワードのように）
  secret : String -> IO (Maybe String)
  secret question = do
    ePutStr (question ++ " ")
    readSecret
  -- ANCHOR_END: ask

  -- ANCHOR: progress
  -- 📊 A bar of progress, drawn again in the same line with \\r. Only on
  --    a terminal: a file would get every bar, one after another.
  -- 進み具合の棒。\\r で、同じ行に描き直す。端末だけ：ファイルには、棒が
  -- 全部、次々に残ってしまう
  bar : Int -> Int -> String -> String
  bar done total label =
    "\\r[" ++ String.repeat filled "#" ++ String.repeat (20 - filled) " "
      ++ "] " ++ show done ++ "/" ++ show total ++ " " ++ label ++ clearLine
      ++ end
    where
      filled = if total > 0 then done * 20 // total else 20
      end = if done >= total then "\\n" else ""

  progress : Int -> Int -> String -> IO ()
  progress done total label = do
    tty <- isTerminal
    if tty then putStr (bar done total label) else pure ()
  -- ANCHOR_END: progress
  """
-- ANCHOR_END: term

-- ANCHOR: chan
chanSource :: Text
chanSource = """
  module Chan exposing (new, send, recv, close, forEach, toList)

  -- ANCHOR: forEach
  -- Do f for each value, until the channel is closed and empty (Go's
  -- for v := range c).
  -- チャネルが閉じて空になるまで、値ごとに f をやる（Go の for range）
  forEach : Chan a -> (a -> IO b) -> IO ()
  forEach c f = do
    next <- recv c
    case next of
      | Nothing -> pure ()
      | Just x -> do
          _ <- f x
          forEach c f

  -- All the values, until the channel is closed.
  -- チャネルが閉じるまでの値を、全部
  toList : Chan a -> IO [a]
  toList c = do
    next <- recv c
    case next of
      | Nothing -> pure []
      | Just x -> do
          rest <- toList c
          pure (x : rest)
  -- ANCHOR_END: forEach
  """
-- ANCHOR_END: chan

-- ANCHOR: actor
actorSource :: Text
actorSource = """
  module Actor exposing
    ( Down (..), new, spawn, spawnOn, send, monitor
    , receiveWith, receiveAfter, call, supervise )

  -- ANCHOR: down
  -- How an actor ended: it came to the end, or it crashed (an error,
  -- or exit with a code that is not 0).
  -- アクターの終わり方：最後まで行った、か、落ちた（エラーか、0 でない
  -- exit）
  data Down = Normal | Crashed String
  -- ANCHOR_END: down

  -- ANCHOR: spawn
  -- Start f as an actor, with a new mailbox. The answer is its Pid.
  -- f を、新しい受け箱を持つアクターとして始める。答えはその Pid
  spawn : (Pid msg -> IO a) -> IO (Pid msg)
  spawn f = do
    box <- new
    spawnOn box f
  -- ANCHOR_END: spawn

  -- ANCHOR: call
  -- Ask, and wait for the answer: a mailbox of our own goes in the
  -- message, and the answer comes back to it.
  -- 頼んで、答えを待つ：自分の受け箱をメッセージに入れると、答えが
  -- そこに返ってくる
  call : Pid msg -> (Pid r -> msg) -> IO r
  call pid ask = do
    me <- new
    send pid (ask me)
    receive me
      | answer -> pure answer
  -- ANCHOR_END: call

  -- ANCHOR: supervise
  -- A supervisor: start work on box, and start it again when it
  -- crashes, up to restarts times. The mailbox stays the same, so the
  -- Pid others hold keeps working, and the messages wait for the new
  -- one.
  -- 監督役：box の上で work を始め、落ちたら、restarts 回まで始め直す。
  -- 受け箱は同じままなので、ほかの仕事が持っている Pid はそのまま使え、
  -- メッセージは新しいほうを待っている
  watch : Int -> Pid msg -> (Pid msg -> IO a) -> Pid Down -> IO ()
  watch left box work me = do
    child <- spawnOn box work
    monitor child me (\\down -> down)
    receive me
      | Normal -> pure ()
      | Crashed why if left > 0 -> do
          -- 「監督役：始め直します（わけ）」
          ePutStrLn f"supervisor: restarting ({why})"
          watch (left - 1) box work me
      -- 「監督役：あきらめます（わけ）」
      | Crashed why -> ePutStrLn f"supervisor: giving up ({why})"

  supervise : Int -> (Pid msg -> IO a) -> IO (Pid msg)
  supervise restarts work = do
    box <- new
    _ <- spawn (watch restarts box work)
    pure box
  -- ANCHOR_END: supervise
  """
-- ANCHOR_END: actor

-- ANCHOR: responseModule
responseSource :: Text
responseSource = """
  module Response exposing
    ( text, html, json, bytes, withHeader, notFound, badRequest, file
    , typeOf )

  -- ANCHOR: makers
  -- 📮 Answers to give back: a status, the headers, and the body.
  --    The same record as the answers Http.send gets.
  -- 返す答え：状態コード、ヘッダー、本文。Http.send がもらう答えと
  -- 同じレコード
  text : Int -> String -> Response
  text status body =
    bytes status "text/plain; charset=utf-8" (Bytes.fromString body)

  -- 🖼️ Bytes as they are, with their type (image/png, ...).
  -- バイト列をそのまま、その型（image/png など）を付けて
  bytes : Int -> String -> Bytes -> Response
  bytes status kind body =
    { status = status, headers = {"content-type": kind}, body = body }

  withHeader : String -> String -> Response -> Response
  withHeader name value r =
    { r | headers = Map.insert (toLower name) value r.headers }

  html : Int -> String -> Response
  html status body =
    withHeader "content-type" "text/html; charset=utf-8" (text status body)

  json : Int -> Json -> Response
  json status j =
    withHeader "content-type" "application/json" (text status (Json.show j))

  notFound : Response
  notFound = text 404 "not found\n"

  badRequest : String -> Response
  badRequest why = text 400 f"bad request: {why}\n"
  -- ANCHOR_END: makers

  -- ANCHOR: responseFile
  -- 📄 A file as the answer, read as bytes, with its type from its
  --    extension. A file that is not there (or not allowed:
  --    --allow-read) is 404.
  -- ファイルを答えに。バイト列で読み、型は拡張子から。ないファイル
  -- （か、許されていないファイル：--allow-read）は 404
  typeOf : String -> String
  typeOf path =
    case String.toLower (Path.extension path) of
      | ".html" -> "text/html; charset=utf-8"
      | ".css" -> "text/css; charset=utf-8"
      | ".js" -> "text/javascript; charset=utf-8"
      | ".json" -> "application/json"
      | ".txt" -> "text/plain; charset=utf-8"
      | ".svg" -> "image/svg+xml"
      | ".png" -> "image/png"
      | ".jpg" -> "image/jpeg"
      | ".jpeg" -> "image/jpeg"
      | ".gif" -> "image/gif"
      | ".webp" -> "image/webp"
      | ".ico" -> "image/x-icon"
      | ".pdf" -> "application/pdf"
      | ".wasm" -> "application/wasm"
      | _ -> "application/octet-stream"
  file : String -> IO Response
  file path = do
    found <- File.readBytes path
    pure (case found of
      | Ok body -> bytes 200 (typeOf path) body
      | Err _ -> notFound)

  -- ANCHOR_END: responseFile
  """
-- ANCHOR_END: responseModule

-- ANCHOR: routeModule
routeSource :: Text
routeSource = """
  module Route exposing
    ( Route, Params, get, post, put, delete, router, matchPieces )

  -- ANCHOR: route
  -- 🧭 One route: a method, the pieces of a path to match, and what to
  --    do. A piece that starts with : (":id") matches any piece, and
  --    goes into the Params under its name ("id").
  -- 道1つ：メソッド、合わせる道のかけら、すること。: で始まるかけら
  -- （":id"）は、どのかけらにも合い、その名前（"id"）で Params に入る
  type alias Params = Map String String

  type alias Route =
    { method : String
    , pieces : [String]
    , run : Params -> Request -> IO Response }

  make : String -> String -> (Params -> Request -> IO Response) -> Route
  make method path run =
    { method = method, pieces = Http.segments path, run = run }

  get = make "GET"
  post = make "POST"
  put = make "PUT"
  delete = make "DELETE"
  -- ANCHOR_END: route

  -- ANCHOR: matchPieces
  -- 🧩 Do the pieces of a path fit the pieces of a route? Just the
  --    params if they do.
  -- 道のかけらは、道の決まりのかけらに合うか。合えば、Params を Just で
  matchPieces : [String] -> [String] -> Maybe Params
  matchPieces pattern parts =
    case (pattern, parts) of
      | ([], []) -> Just {}
      | (p : ps, x : xs) if String.startsWith ":" p ->
          Maybe.map (Map.insert (String.drop 1 p) x) (matchPieces ps xs)
      | (p : ps, x : xs) if p == x -> matchPieces ps xs
      | _ -> Nothing

  -- The routes whose pieces fit, each with its params.
  -- かけらが合う道を、それぞれの Params と一緒に
  fitting : [String] -> [Route] -> [(Route, Params)]
  fitting parts routes =
    case routes of
      | [] -> []
      | r : rest ->
          case matchPieces r.pieces parts of
            | Just ps -> (r, ps) : fitting parts rest
            | Nothing -> fitting parts rest
  -- ANCHOR_END: matchPieces

  -- ANCHOR: router
  -- 🚏 A handler made of routes: the first one whose method and path
  --    fit. A path that fits only with another method is 405 (with
  --    the methods it has, in allow); a path that fits none is 404.
  -- 道の一覧から作る handler：メソッドと道が合う最初の道。ほかの
  -- メソッドでなら合う道は 405（使えるメソッドを allow に）。どれにも
  -- 合わない道は 404
  router : [Route] -> Request -> IO Response
  router routes req =
    let fits = fitting (Http.segments req.path) routes in
    case [(r, ps) | (r, ps) <- fits, r.method == req.method] of
      | (r, ps) : _ -> r.run ps req
      | [] if null fits -> pure Response.notFound
      | [] ->
          pure (Response.withHeader "allow"
                  (join ", " [r.method | (r, _) <- fits])
                  (Response.text 405 "method not allowed\n"))
  -- ANCHOR_END: router
  """
-- ANCHOR_END: routeModule

-- ANCHOR: send
sendSource :: Text
sendSource = """
  module Send exposing (Sendable (..), (!))

  -- ANCHOR: sendable
  -- 📮 Something we can send values to: a channel, or a mailbox.
  --    box ! v  sends v, the same as Erlang's  Pid ! Msg.
  -- 値を送れるもの：チャネルか、受け箱。box ! v で v を送る（Erlang の
  -- Pid ! Msg と同じ）
  class Sendable box where
    send : box a -> a -> IO ()

  instance Sendable Chan where
    send c v = Chan.send c v

  instance Sendable Pid where
    send pid msg = Actor.send pid msg

  infixr 0 send as !
  -- ANCHOR_END: sendable
  """
-- ANCHOR_END: send

-- ANCHOR: build
-- 🏗️ Make the standard modules one by one, importing each right
--    away (as every file does), so that the next one can use it. The
--    hidden names are put in after each one, so that the modules
--    after Prelude can use do and f"..." too.
-- 標準のモジュールを1つずつ作り、すぐに（どのファイルとも同じ形で）
-- import する。そうすれば、次のモジュールが使える。隠した名前も
-- 1つごとに入れるので、Prelude より後ろのモジュールは do も f"..." も
-- 使える
buildPrelude :: Either Text Session
buildPrelude = do
  -- ANCHOR: earlyOperators
  -- The builtin operators of the Prelude (+, :, &&) are read first:
  -- Maybe and List, made before the Prelude, use them too.
  -- プレリュードの組み込みの演算子（+、:、&&）は、先に読んでおく。
  -- プレリュードより前に作る Maybe や List も使うから
  early <- fixitiesOf "<Prelude>" basicsSource
  let builtin = Map.fromList
        [ (o, f { fixityBuiltin = True }) | (_, o, f) <- early
        , isJust (builtinOperator (fixityTarget f)) ]
      start = emptySession { sessionOperators = builtin }
  -- ANCHOR_END: earlyOperators
  foldM addModule start standardSources
  where
    addModule session (m, source) = do
      made <- standardModule session m source
      imp  <- either (Left . T.pack . errorBundlePretty) Right
                (runLamParser Map.empty (spaces *> importLine <* eof)
                   "<default imports>" (defaultImports m))
      withHiddenNames <$> importInto session imp made
-- ANCHOR_END: build

-- ANCHOR: formatNames
-- 🙈 The names f"..." calls, under hidden names ($fmt.display), with
--    the prelude's types and values. A program cannot write them, so
--    no import and no definition of its own can change them.
-- f"..." が呼ぶ名前を、隠した名前（$fmt.display）で、プレリュードの型と
-- 値のまま入れておく。プログラムには書けないので、import も自分の定義も
-- 変えられない
formatNames :: [Name]
formatNames =
  [ "display", "show", "showNumber", "Float.fixed", "groupDigits"
  , "zeroPad", "padLeft", "padRight", "center" ]

-- ANCHOR: hiddenNames
-- 🙈 Each hidden name, and the name of the prelude it stands for:
--    the ones of f"...", the ones a do calls: bind, pure (for ?),
--    branch (for ?) and IO.finally (for defer), and the ones that
--    start main with its arguments.
-- 隠した名前と、それが表すプレリュードの名前：f"..." のもの、do が
-- 呼ぶもの（bind、? の pure と branch、defer の IO.finally）、引数のある
-- main を始めるもの
hiddenNames :: [(Name, Name)]
hiddenNames =
  [ (fmtName x, x) | x <- formatNames ]
    <> [ (doBind, "bind"), (doMap, "map"), (doAp, "ap"), (doPure, "pure")
       , (tryBranch, "branch"), (doFinally, "IO.finally")
       , (mainBind, "IO.bindIO"), (mainArgs, "IO.getArgs")
       , (mainEnv, "IO.getEnvironment") ]
    -- ANCHOR: regexHidden
    -- What m//, s/// and =~ call:  $re.replaceAll is Regex.replaceAll
    -- m//・s///・=~ が呼ぶもの
    <> [ (reName x, "Regex." <> x) | x <- regexSugarNames ]
    -- ANCHOR_END: regexHidden
    -- ANCHOR: shellHidden
    -- What $"..." calls:  $sh.args is Shell.toArgs
    -- $"…" が呼ぶもの
    <> [ (shName "command", "Shell.command"), (shName "glue", "Shell.glue")
       , (shName "args", "Shell.toArgs") ]
    -- ANCHOR_END: shellHidden
    -- ANCHOR: recvHidden
    -- What receive calls:  $recv.after is Actor.receiveAfter
    -- receive が呼ぶもの
    <> [ (recvName "with", "Actor.receiveWith")
       , (recvName "after", "Actor.receiveAfter") ]
    -- ANCHOR_END: recvHidden

regexSugarNames :: [Name]
regexSugarNames =
  ["matches", "replace", "replaceAll", "replaceWith", "replaceFirstWith"]

withHiddenNames :: Session -> Session
withHiddenNames session = session
  { sessionTypes  = hide (sessionTypes session)
  , sessionValues = hide (sessionValues session) }
  where
    hide table = Map.fromList
      [ (h, v) | (h, x) <- hiddenNames, Just v <- [Map.lookup x table] ]
      `Map.union` table
-- ANCHOR_END: hiddenNames
-- ANCHOR_END: formatNames

-- ANCHOR: session
-- 🌱 A session with the prelude loaded. The prelude is checked by
--    the tests, so a mistake in it is found before anyone runs Lam.
-- プレリュードを読みこんだセッション。プレリュードはテストで
-- 確かめるので、まちがいは、誰かが Lam を動かす前に見つかる
preludeSession :: Session
preludeSession =
  case buildPrelude of
    Right s  -> s
    Left err -> error ("the prelude is broken: " <> T.unpack err)
-- ANCHOR_END: session
-- ANCHOR_END: all
