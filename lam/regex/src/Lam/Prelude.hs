-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR: all
-- | The prelude: Lam's standard library, written in Lam itself.
-- プレリュード：Lam で書いた、Lam の標準ライブラリ。
module Lam.Prelude
  ( preludeSource
  , preludeSession
  ) where

import Data.Text (Text)

import Lam.Program
-- ANCHOR_END: head

-- ANCHOR: source
-- 📚 Every REPL and every program starts with these.
-- どの REPL も、どのプログラムも、これを読みこんでから始まる
preludeSource :: Text
preludeSource = """
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

  -- A value that may be missing, and a result that may be an error
  -- ないかもしれない値と、エラーかもしれない結果
  data Maybe a = Nothing | Just a
  data Result e a = Ok a | Err e

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

  -- ANCHOR: maybeMore
  -- Go on with a value that may be missing
  -- ないかもしれない値で、続きを計算する
  let Maybe.map f m =
    case m of
      | Nothing -> Nothing
      | Just x -> Just (f x)
  -- bind takes the value first (like >>= of Haskell); andThen is
  -- the same with the function first (like Elm).
  -- bind は値が先（Haskell の >>= と同じ）。andThen は、同じものの
  -- 関数が先の形（Elm と同じ）
  let Maybe.bind m f =
    case m of
      | Nothing -> Nothing
      | Just x -> f x
  let Maybe.andThen f m = Maybe.bind m f
  -- ANCHOR_END: maybeMore

  -- ANCHOR: resultMore
  -- Go on with a result that may be an error
  -- エラーかもしれない結果で、続きを計算する
  let Result.map f r =
    case r of
      | Ok x -> Ok (f x)
      | Err e -> Err e
  let Result.bind r f =
    case r of
      | Ok x -> f x
      | Err e -> Err e
  let Result.andThen f r = Result.bind r f
  -- ANCHOR_END: resultMore

  -- ANCHOR: ioAndThen
  -- bind for IO, with the function first, like Result.andThen.
  -- IO の bind の、関数が先の形。Result.andThen と同じ
  let IO.andThen f m = bind m f
  -- ANCHOR_END: ioAndThen

  -- ANCHOR: forEach
  -- Do an action for each item of a list, in order
  -- リストの1つずつに、順に頼みごとをする
  let forEach xs f =
    case xs of
      | [] -> pure ()
      | x : rest -> do
          f x
          forEach rest f
  -- ANCHOR_END: forEach

  -- ANCHOR: insertWith
  -- Maps from strings: functions that take a function, made of the
  -- builtins
  -- 文字列をキーにした Map：関数を受け取る関数を、組み込みの関数から作る
  let Map.insertWith f k v m =
    case Map.lookup k m of
      | Nothing -> Map.insert k v m
      | Just old -> Map.insert k (f v old) m
  let Map.map f m =
    Map.fromList (map (\\kv -> (fst kv, f (snd kv))) (Map.toList m))
  -- ANCHOR_END: insertWith

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
    | JObj (Map Json)
  -- ANCHOR_END: json

  -- ANCHOR: jsonTools
  -- Take values out of JSON. Each one gives Nothing when the JSON
  -- has another shape.
  -- JSON から値を取り出す。形がちがえば、どれも Nothing
  let Json.get k j =
    case j of
      | JObj m -> Map.lookup k m
      | _ -> Nothing
  let Json.at path j =
    case path of
      | [] -> Just j
      | k : rest -> Maybe.andThen (Json.at rest) (Json.get k j)
  let Json.int j = case j of | JInt n -> Just n | _ -> Nothing
  let Json.string j = case j of | JStr s -> Just s | _ -> Nothing
  let Json.bool j = case j of | JBool b -> Just b | _ -> Nothing
  let Json.array j = case j of | JArr xs -> Just xs | _ -> Nothing
  -- ANCHOR_END: jsonTools

  -- ANCHOR: mkDec
  -- Make a number with a fraction, in the same shape as Json.parse
  -- makes it: the 0s at the end of c move into d, and a whole number
  -- is JInt.  Json.mkDec 19990 3 is JDec 1999 2, and
  -- Json.mkDec 1900 2 is JInt 19.
  -- 小数を、Json.parse と同じ形で作る。c の後ろの 0 は d へ移し、
  -- 整数なら JInt にする
  let Json.mkDec c d =
    if c == 0 then JInt 0
    else if mod c 10 == 0 then Json.mkDec (c / 10) (d - 1)
    else if d > 0 || d < 0 - 1024 then JDec c d
    else JInt (c * pow 10 (0 - d))
  -- ANCHOR_END: mkDec

  -- ANCHOR: match
  -- What a regex found: the whole text, where it starts (counted in
  -- letters, from 0), the groups (1, 2, ...), and the named groups.
  -- A group that took no part is "".
  -- 正規表現が見つけたもの：全体の文字、始まる場所（0 から数えた文字の
  -- 数）、グループ（1, 2, ...）、名前付きのグループ。使われなかった
  -- グループは ""
  data Match = Match String Int [String] (Map String)

  let Match.text m = case m of | Match t _ _ _ -> t
  let Match.start m = case m of | Match _ i _ _ -> i
  let Match.groups m = case m of | Match _ _ gs _ -> gs
  -- Match.group 0 is the whole text, as in Python.
  -- 0番は全体（Python と同じ）
  let Match.group n m =
    if n == 0 then Just (Match.text m)
    else case drop (n - 1) (Match.groups m) of
      | g : _ -> Just g
      | [] -> Nothing
  let Match.named name m =
    case m of | Match _ _ _ names -> Map.lookup name names
  -- ANCHOR_END: match
  """
-- ANCHOR_END: source

-- ANCHOR: session
-- 🌱 A session with the prelude loaded. The prelude is checked by
--    the tests, so a mistake in it is found before anyone runs Lam.
-- プレリュードを読みこんだセッション。プレリュードはテストで
-- 確かめるので、まちがいは、誰かが Lam を動かす前に見つかる
preludeSession :: Session
preludeSession =
  case loadProgram emptySession "<prelude>" preludeSource of
    Right (s, _) -> s
    Left err     -> error ("the prelude is broken: " <> show err)
-- ANCHOR_END: session
-- ANCHOR_END: all
