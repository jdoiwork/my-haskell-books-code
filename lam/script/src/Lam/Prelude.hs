-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR: all
-- | The prelude: Lam's standard library, written in Lam itself, now
--   cut into modules (Maybe, List, Result, Prelude, String, Map, Set,
--   Float, Json, IO). Every file starts with the default imports.
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

import Lam.Parser (fixitiesOf, importLine, runLamParser, spaces, fmtName)
import Lam.Sugar (builtinOperator)
import Lam.Program
import Lam.Sugar (doAp, doBind, doFinally, doMap, doPure, tryBranch)
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
  , ("IO",      ioSource) ]
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
  "Json"    -> "import Json (Json (..))"
  -- ANCHOR: importIO
  "IO"      -> "import IO (putStr, putStrLn, getLine, readFile, writeFile, "
                 <> "getArgs, lookupEnv, getEnvironment, getContents, "
                 <> "ePutStr, ePutStrLn, exit)"
  -- ANCHOR_END: importIO
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
  -- their names start with  The classes of the Prelude make the
  -- same names work on other containers too.
  -- リスト。case で分ける。リストだけのもので、外からは名前が  で
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
    , parse, show, pretty, toFloat, fromFloat )

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
  """
-- ANCHOR_END: json

-- ANCHOR: io
ioSource :: Text
ioSource = """
  module IO exposing
    ( putStr, putStrLn, getLine, readFile, writeFile, andThen
    , finally, bracket, try
    , getArgs, lookupEnv, getEnvironment, getContents
    , ePutStr, ePutStrLn, exit )

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
  """
-- ANCHOR_END: io

-- ANCHOR: build
-- 🏗️ Make the standard modules one by one, importing each right
--    away (as every file does), so that the next one can use it.
-- 標準のモジュールを1つずつ作り、すぐに（どのファイルとも同じ形で）
-- import する。そうすれば、次のモジュールが使える
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
  session <- foldM addModule start standardSources
  Right (withHiddenNames session)
  where
    addModule session (m, source) = do
      made <- standardModule session m source
      imp  <- either (Left . T.pack . errorBundlePretty) Right
                (runLamParser Map.empty (spaces *> importLine <* eof)
                   "<default imports>" (defaultImports m))
      importInto session imp made
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
