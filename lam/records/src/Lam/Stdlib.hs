-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The toolbox: builtins for containers (toList, map), for strings
--   and lists alike (reverse, take, drop, slice), for strings
--   (String.trim, String.split, ...) and for maps (Map.union).
-- 道具箱：入れ物の組み込み（toList・map）、文字列とリストに共通の
-- 組み込み（reverse・take・drop・slice）、文字列の組み込み
-- （String.trim・String.split など）、Map の組み込み（Map.union）。
module Lam.Stdlib
  ( stdlibBuiltins
  , containerOf
  , splitOn
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
stdlibBuiltins :: [Builtin]
stdlibBuiltins =
  foldableBuiltins <> functorBuiltins <> sequenceBuiltins <> stringBuiltins
    <> [mapUnion]
-- ANCHOR_END: table

-- ANCHOR: containers
-- 📦 Foldable: the only builtin that looks inside any container to
--    fold it. Everything else (any, sum, maximum, ...) is Lam, written
--    on top of toList.
-- Foldable：どの入れ物の中も見る組み込みは、これだけ。ほか（any・sum・
-- maximum など）は、toList の上に Lam で書く
foldableBuiltins :: [Builtin]
foldableBuiltins =
  -- toList : Foldable f => f a -> [a]
  [ requiring [(Foldable, 0)] $
      prim1 "toList" (TApp f a ~> tList a) $ \v ->
        VList <$> containerOf v
  ]
  where
    f = TVar 0
    a = TVar 1

-- 📦 The items of a container, in order. A map gives its values (in
--    the order of the keys), Just x gives [x], and Nothing gives [].
--    The type checker made sure that v is one of these.
-- 入れ物の中身を、順に。Map は値（キーの順）、Just x は [x]、
-- Nothing は []。v がこのどれかであることは、型検査器が確かめてある
containerOf :: Value -> Either EvalError [Value]
containerOf v = case v of
  VList xs            -> Right xs
  VMap m              -> Right (Map.elems m)
  VCon "Just" 1 [x]   -> Right [x]
  VCon "Nothing" _ [] -> Right []
  _                   -> notAContainer "toList"

-- 「入れ物ではない値が来ました（ここには来ないはず）」
notAContainer :: Text -> Either EvalError a
notAContainer n = Left (BuiltinFailed n "got a value that is not a container")
-- ANCHOR_END: containers

-- ANCHOR: functor
-- 🎁 Functor: map changes each item, and keeps the shape of the
--    container (the keys of a map, the Err of a Result).
-- Functor：map は中身を1つずつ変え、入れ物の形はそのまま（Map のキー、
-- Result の Err）
functorBuiltins :: [Builtin]
functorBuiltins =
  -- map : Functor f => (a -> b) -> f a -> f b
  [ requiring [(Functor, 0)] $
      prim2 "map" ((a ~> b) ~> TApp f a ~> TApp f b) $ \g v ->
        case v of
          VList xs             -> VList <$> traverse (apply g) xs
          VMap m               -> VMap <$> traverse (apply g) m
          VCon "Just" 1 [x]    -> (\y -> con "Just" [y]) <$> apply g x
          VCon "Nothing" _ []  -> Right v
          VCon "Ok" 0 [x]      -> (\y -> con "Ok" [y]) <$> apply g x
          VCon "Err" _ [_]     -> Right v
          _                    -> notAContainer "map"
  ]
  where
    f = TVar 0
    a = TVar 1
    b = TVar 2
-- ANCHOR_END: functor

-- ANCHOR: sequences
-- 🔤 Sequence: the same name for a string and a list.
--    reverse "abc" is "cba", and reverse [1, 2, 3] is [3, 2, 1].
-- Sequence：文字列にもリストにも、同じ名前で
sequenceBuiltins :: [Builtin]
sequenceBuiltins =
  [ needSeq $ prim1 "reverse" (a ~> a) $ \v ->
      onSeq "reverse" T.reverse reverse v
  , needSeq $ prim2 "take" (TInt ~> a ~> a) $ \n v -> do
      k <- int n
      onSeq "take" (T.take k) (take k) v
  , needSeq $ prim2 "drop" (TInt ~> a ~> a) $ \n v -> do
      k <- int n
      onSeq "drop" (T.drop k) (drop k) v
  -- slice from to: from "from" up to (not with) "to". A negative
  -- number counts from the end, as in Python:  slice 1 (0 - 1) "Lam!"
  -- is "am".
  -- slice from to：from から、to の手前まで。負の数は後ろから数える
  -- （Python と同じ）
  , needSeq $ prim3 "slice" (TInt ~> TInt ~> a ~> a) $ \i j v -> do
      from <- int i
      to   <- int j
      -- Where to start and stop, for a sequence of len items.
      -- 長さ len のとき、どこから・どこの手前まで
      let place len k = max 0 (min len (if k < 0 then len + k else k))
      case v of
        VString t -> let len = T.length t
                     in Right (VString (T.drop (place len from)
                                          (T.take (place len to) t)))
        VList xs  -> let len = length xs
                     in Right (VList (drop (place len from)
                                        (take (place len to) xs)))
        _         -> notASequence "slice"
  ]
  where
    a = TVar 0
    -- Each row needs a (t0) to be a Sequence.
    -- どの行も、a（t0）が Sequence であること
    needSeq = requiring [(Sequence, 0)]
    onSeq n onText onList v = case v of
      VString t -> Right (VString (onText t))
      VList xs  -> Right (VList (onList xs))
      _         -> notASequence n
    notASequence n =
      Left (BuiltinFailed n "got a value that is neither a string nor a list")

-- 🔢 An Int of Lam, as a Haskell Int (a take of more than that is
--    the same as all of it).
-- Lam の Int を、Haskell の Int に
int :: Value -> Either EvalError Int
int v = case v of
  VInt n -> Right (fromInteger (max (-big) (min big n)))
  _      -> Left (BuiltinFailed "Int" "got a value of the wrong type")
  where
    big = toInteger (maxBound :: Int)
-- ANCHOR_END: sequences

-- ANCHOR: strings
-- ✂️ String: what scripts ask of a string every day. The text to look
--    for comes first and the string last, so that |> can be used:
--      line |> String.trim |> String.split ","
-- String：スクリプトが毎日、文字列に頼むこと。探す文字が先、文字列が
-- 最後なので、|> でつなげる
stringBuiltins :: [Builtin]
stringBuiltins =
  [ prim1 "String.trim" (tString ~> tString) (onText T.strip)
  , prim1 "String.trimStart" (tString ~> tString) (onText T.stripStart)
  , prim1 "String.trimEnd" (tString ~> tString) (onText T.stripEnd)
  -- String.split "," "a,,b" is ["a", "", "b"]. An empty separator
  -- cuts between every letter.
  -- 区切りが空なら、1文字ずつに切る
  , prim2 "String.split" (tString ~> tString ~> tList tString) $
      \sep v -> do
        s <- string sep
        t <- string v
        Right (VList (map VString (splitOn s t)))
  , test2 "String.startsWith" T.isPrefixOf
  , test2 "String.endsWith" T.isSuffixOf
  , test2 "String.contains" T.isInfixOf
  -- String.replace "cat" "dog" s: every "cat" becomes "dog". No regex.
  -- 正規表現なしで、全部を置き換える
  , prim3 "String.replace" (tString ~> tString ~> tString ~> tString) $
      \old new v -> do
        o <- string old
        n <- string new
        t <- string v
        Right (VString (T.intercalate n (splitOn o t)))
  , prim2 "String.repeat" (TInt ~> tString ~> tString) $ \n v -> do
      k <- int n
      t <- string v
      Right (VString (T.replicate (max 0 k) t))
  ]
  where
    onText g v = VString . g <$> string v
    test2 n g = prim2 n (tString ~> tString ~> TBool) $ \x v -> do
      needle <- string x
      t      <- string v
      Right (VBool (g needle t))

-- ✂️ Cut at every sep. T.splitOn stops the program on an empty sep, so
--    an empty sep cuts between the letters instead.
-- sep のところで全部切る。T.splitOn は空の sep でプログラムを止めるので、
-- 空なら1文字ずつに切る
splitOn :: Text -> Text -> [Text]
splitOn sep t
  | T.null sep = T.chunksOf 1 t
  | otherwise  = T.splitOn sep t
-- ANCHOR_END: strings

-- ANCHOR: mapUnion
-- 🗺️ Two maps into one. When both have a key, the first one wins
--    (Map.unionWith, in the module Map, chooses with a function).
-- 2つの Map を1つに。両方にあるキーは、1つ目が勝つ
mapUnion :: Builtin
mapUnion =
  prim2 "Map.union" (tMap a ~> tMap a ~> tMap a) $ \x y -> do
    l <- mapOf x
    r <- mapOf y
    Right (VMap (Map.union l r))
  where
    a = TVar 0
-- ANCHOR_END: mapUnion
-- ANCHOR_END: all
