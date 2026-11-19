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
  let abs n = if n < 0 then -n else n
  let mod a b = a - a // b * b
  let even n = mod n 2 == 0
  let pow b e = if e == 0 then 1 else b * pow b (e - 1)
  let gcd a b = if b == 0 then a else gcd b (mod a b)

  -- A value that may be missing, and a result that may be an error
  -- ないかもしれない値と、エラーかもしれない結果
  data Maybe a = Nothing | Just a
  data Result e a = Ok a | Err e

  -- ANCHOR: listBasics
  -- Lists, taken apart with case. These are for lists only; their
  -- names start with List. The classes below make the same names
  -- work on other containers too.
  -- リスト。case で分ける。リストだけのもので、名前は List. で始める。
  -- 下のクラスが、同じ名前をほかの入れ物にも効くようにする
  let List.foldr f z xs =
    case xs of
      | [] -> z
      | x : rest -> f x (List.foldr f z rest)
  let List.foldl f z xs =
    case xs of
      | [] -> z
      | x : rest -> List.foldl f (f z x) rest
  let List.map f xs = List.foldr (\\x rest -> f x : rest) [] xs
  let List.reverse xs = List.foldl (\\acc x -> x : acc) [] xs
  let List.take n xs =
    if n < 1 then [] else
      case xs of
        | [] -> []
        | x : rest -> x : List.take (n - 1) rest
  let List.drop n xs =
    if n < 1 then xs else
      case xs of
        | [] -> []
        | _ : rest -> List.drop (n - 1) rest
  -- ANCHOR_END: listBasics
  -- ANCHOR: listFolds
  let List.null xs = case xs of | [] -> true | _ -> false
  -- || and && stop early: any stops at the first item that passes.
  -- || と && は打ち切るので、any は最初に合ったところで止まる
  let List.any p xs =
    case xs of
      | [] -> false
      | x : rest -> p x || List.any p rest
  let List.all p xs =
    case xs of
      | [] -> true
      | x : rest -> p x && List.all p rest
  let List.sum xs = List.foldl (\\total x -> total + x) 0 xs
  let List.product xs = List.foldl (\\total x -> total * x) 1 xs
  -- An empty list has no biggest item: Nothing.
  -- 空のリストには、いちばん大きいものがない：Nothing
  let List.maximum xs =
    case xs of
      | [] -> Nothing
      | x : rest -> Just (List.foldl (\\m y -> if y > m then y else m) x rest)
  let List.minimum xs =
    case xs of
      | [] -> Nothing
      | x : rest -> Just (List.foldl (\\m y -> if y < m then y else m) x rest)
  let List.find p xs =
    case xs of
      | [] -> Nothing
      | x : rest -> if p x then Just x else List.find p rest
  -- ANCHOR_END: listFolds

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

  -- ANCHOR: fallible
  -- 🛟 Fallible: a container that may be missing its value. toMaybe
  --    says what is inside, as a Maybe. m ?: d asks it: the value
  --    inside, or d (computed only then).
  -- Fallible：値がないかもしれない入れ物。toMaybe は、中にあるものを
  -- Maybe で言う。m ?: d はそれに聞く：中の値か、なければ d（そのとき
  -- だけ計算する）
  class Fallible f where
    toMaybe : f a -> Maybe a

  instance Fallible Maybe where
    toMaybe m = m

  instance Fallible (Result e) where
    toMaybe r =
      case r of
        | Ok x -> Just x
        | Err _ -> Nothing
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
  -- Sort by a key, the smallest first (merge sort). Two items with the
  -- same key keep their order.  sortOn negate [3, 1, 2] is [3, 2, 1].
  -- キーで並べる。小さいものから（マージソート）。同じキーの2つは、
  -- 元の順のまま
  let List.merge before xs ys =
    case (xs, ys) of
      | ([], _) -> ys
      | (_, []) -> xs
      | (x : xr, y : yr) ->
          if before y x then y : List.merge before xs yr
          else x : List.merge before xr ys
  let List.sortBy before xs =
    if length xs < 2 then xs else
      let half = length xs // 2 in
      List.merge before (List.sortBy before (List.take half xs))
        (List.sortBy before (List.drop half xs))
  let sortOn key xs = List.sortBy (\\a b -> key a < key b) xs
  let sort xs = sortOn id xs
  -- ANCHOR_END: sort

  -- ANCHOR: strings
  -- Strings: trim, split, startsWith, ... are builtins (Haskell's
  -- Data.Text does the work). Here is one made of them.
  -- 文字列：trim・split・startsWith などは組み込み（中身は Haskell の
  -- Data.Text）。ここには、それを組み合わせたものを書く
  let String.isEmpty s = s == ""
  -- ANCHOR_END: strings

  let fromMaybe d m =
    case m of
      | Nothing -> d
      | Just x -> x
  let lookup k pairs =
    case pairs of
      | [] -> Nothing
      | (k2, v) : rest -> if k == k2 then Just v else lookup k rest

  -- ANCHOR: maybeMore
  -- Go on with a value that may be missing
  -- ないかもしれない値で、続きを計算する
  let Maybe.map f m = map f m
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

  -- ANCHOR: insertWith
  -- Maps: functions that take a function, made of the builtins
  -- Map：関数を受け取る関数を、組み込みの関数から作る
  let Map.insertWith f k v m =
    case Map.lookup k m of
      | Nothing -> Map.insert k v m
      | Just old -> Map.insert k (f v old) m
  -- ANCHOR_END: insertWith

  -- ANCHOR: mapMore
  -- Keep the values that pass (filterWithKey also sees the key)
  -- 合う値だけを残す（filterWithKey はキーも見る）
  let Map.filterWithKey p m =
    Map.fromList (filter (\\(k, v) -> p k v) (Map.toList m))
  let Map.filter p m = Map.filterWithKey (\\k v -> p v) m
  -- Change the value at k with f, which sees Nothing when k is not
  -- there, and gives Nothing to delete it (Elm's Dict.update).
  -- k の値を f で変える。k がなければ f は Nothing を受け取る。
  -- f が Nothing を返せば消す（Elm の Dict.update と同じ形）
  let Map.update k f m =
    case f (Map.lookup k m) of
      | Nothing -> Map.delete k m
      | Just v -> Map.insert k v m
  -- Go through the keys in order, with the key and the value.
  -- キーの順に、キーと値を見ながらたたむ
  let Map.foldWithKey f z m =
    List.foldr (\\(k, v) acc -> f k v acc) z (Map.toList m)
  -- Two maps into one; f decides when both have the key.
  -- 2つの Map を1つに。両方にあるキーは f で決める
  let Map.unionWith f a b =
    Map.foldWithKey
      (\\k v acc -> Map.insertWith (\\new old -> f old new) k v acc) a b
  -- ANCHOR_END: mapMore

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
    else if mod c 10 == 0 then Json.mkDec (c // 10) (d - 1)
    else if d > 0 || d < -1024 then JDec c d
    else JInt (c * pow 10 (-d))
  -- ANCHOR_END: mkDec

  -- ANCHOR: display
  -- 🖨️ Display: a value as text for people. {x} of f"..." calls it.
  --    Any type in Showable can join with one line (instance Display
  --    Money), and then it is shown as show shows it. A type can say
  --    how it wants to be shown, too: a string without its quotes,
  --    and JSON as JSON.
  -- Display：人に見せる文字。f"..." の {x} が呼ぶ。Showable の型なら、
  -- 1行（instance Display Money）で入れて、show と同じ形で見せる。
  -- 見せ方を自分で決めてもよい：文字列は引用符なしに、JSON は JSON に
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
  instance Display Json where
    display j = Json.show j
  -- ANCHOR_END: display
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
