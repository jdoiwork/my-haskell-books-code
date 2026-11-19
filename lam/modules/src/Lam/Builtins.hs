-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The builtins: functions written in Haskell, with their Lam types.
-- 組み込みの関数：Haskell で書いた関数と、その Lam の型。
module Lam.Builtins
  ( Builtin (..)
  , builtins
    -- For adding more builtins (the training).
    -- 組み込みの関数を足すための部品（トレーニング）
  , prim1
  , prim2
  , prim3
  , requiring
  , wrong
  , (~>)
  , string
  , list
  , mapOf
  , setOf
    -- The constructors of the prelude, made from Haskell.
    -- プレリュードの構成子を、Haskell から作る
  , preludeConstructors
  , con
  , maybeValue
  , tMaybe
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Eval
import Lam.Syntax (Name)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 🧰 One row of the table: a name, a type, and a value.
-- 表の1行：名前、型、値
data Builtin = Builtin
  { builtinName  :: Name
  , builtinType  :: Scheme
  , builtinValue :: Value
  }

-- 📋 The table of builtins. To add one, add one row here.
-- 組み込みの関数の表。足すときは、ここに1行足す
builtins :: [Builtin]
builtins =
  -- Strings
  -- 文字列
  [ prim1 "toUpper" (tString ~> tString) (onString T.toUpper)
  , prim1 "toLower" (tString ~> tString) (onString T.toLower)
  , prim1 "chars" (tString ~> tList tString) $ \v -> do
      s <- string v
      Right (VList [VString (T.singleton c) | c <- T.unpack s])
  , prim2 "join" (tString ~> tList tString ~> tString) $ \sep vs -> do
      s     <- string sep
      parts <- list vs >>= traverse string
      Right (VString (T.intercalate s parts))
  , prim1 "showInt" (TInt ~> tString) $ \v -> case v of
      VInt n -> Right (VString (T.pack (show n)))
      _      -> wrong "showInt"

  -- Tuples
  -- タプル
  , prim1 "fst" (tTuple [a, b] ~> a) (pairPart fst)
  , prim1 "snd" (tTuple [a, b] ~> b) (pairPart snd)

  -- The length of a string or a list: HasLength a => a -> Int
  -- 文字列やリストの長さ
  , requiring [(HasLength, 0)] $ prim1 "length" (a ~> TInt) $ \v ->
      case v of
        VString s -> Right (VInt (fromIntegral (T.length s)))
        VList xs  -> Right (VInt (fromIntegral (length xs)))
        _         -> wrong "length"

  -- Lists
  -- リスト
  , prim1 "head" (tList a ~> a) $ \v -> do
      xs <- list v
      case xs of
        x : _ -> Right x
        -- 「リストが空です」
        []    -> Left (BuiltinFailed "head" "the list is empty")
  , prim1 "tail" (tList a ~> tList a) $ \v -> do
      xs <- list v
      case xs of
        _ : rest -> Right (VList rest)
        []       -> Left (BuiltinFailed "tail" "the list is empty")
  ]
  <> mapBuiltins
  <> setBuiltins
  where
    a = TVar 0
    b = TVar 1
-- ANCHOR_END: table

-- ANCHOR: mapTable
-- 🗺️ Maps. Each name starts with Map., like Haskell's qualified import
--    (import qualified Data.Map as Map). A key may be of any
--    Comparable type k. Only the functions that compare keys say so,
--    as in Haskell:  KeyComparable k => k -> v -> Map k v -> Map k v
--    (KeyComparable: the Comparable types, compared so that NaN is
--    found again)
-- Map。名前は、Haskell の qualified な import と同じく、Map. で始める。
-- キーは、Comparable な型 k なら何でもよい。Haskell と同じく、キーを
-- 比べる関数だけが KeyComparable k を求める（KeyComparable：仲間は
-- Comparable と同じ。NaN も引けるように比べる）
mapBuiltins :: [Builtin]
mapBuiltins =
  [ Builtin "Map.empty" (closeOver (tMap k v)) (VMap Map.empty)
  , keyed $ prim3 "Map.insert" (k ~> v ~> tMap k v ~> tMap k v) $ \key x m ->
      VMap . Map.insert (Key key) x <$> mapOf m
  , keyed $ prim2 "Map.lookup" (k ~> tMap k v ~> tMaybe v) $ \key m -> do
      found <- Map.lookup (Key key) <$> mapOf m
      Right (maybeValue found)
  , keyed $ prim2 "Map.delete" (k ~> tMap k v ~> tMap k v) $ \key m ->
      VMap . Map.delete (Key key) <$> mapOf m
  , keyed $ prim2 "Map.member" (k ~> tMap k v ~> TBool) $ \key m ->
      VBool . Map.member (Key key) <$> mapOf m
  , prim1 "Map.size" (tMap k v ~> TInt) $ \m ->
      VInt . fromIntegral . Map.size <$> mapOf m
  , prim1 "Map.toList" (tMap k v ~> tList (tTuple [k, v])) $ \m -> do
      kvs <- Map.toList <$> mapOf m
      Right (VList [ VTuple [key, x] | (Key key, x) <- kvs ])
  , keyed $ prim1 "Map.fromList" (tList (tTuple [k, v]) ~> tMap k v) $
      \pairs -> do
        kvs <- list pairs >>= traverse keyValue
        Right (VMap (Map.fromList kvs))
  , prim1 "Map.keys" (tMap k v ~> tList k) $ \m ->
      VList . map (\(Key key) -> key) . Map.keys <$> mapOf m
  , prim1 "Map.values" (tMap k v ~> tList v) $ \m ->
      VList . Map.elems <$> mapOf m
  , keyed $ prim2 "Map.union" (tMap k v ~> tMap k v ~> tMap k v) $ \m1 m2 ->
      VMap <$> (Map.union <$> mapOf m1 <*> mapOf m2)
  ]
  where
    k = TVar 0
    v = TVar 1
    keyed = requiring [(KeyComparable, 0)]
    keyValue pair = case pair of
      VTuple [key, x] -> Right (Key key, x)
      _               -> wrong "a pair"
-- ANCHOR_END: mapTable

-- ANCHOR: setTable
-- 🧺 Sets: each value once, kept in order.
-- Set：どの値も1つずつ、順に並べて持つ
setBuiltins :: [Builtin]
setBuiltins =
  [ Builtin "Set.empty" (closeOver (tSet a)) (VSet Set.empty)
  , keyed $ prim1 "Set.fromList" (tList a ~> tSet a) $ \xs ->
      VSet . Set.fromList . map Key <$> list xs
  , prim1 "Set.toList" (tSet a ~> tList a) $ \s ->
      VList . map (\(Key x) -> x) . Set.toList <$> setOf s
  , keyed $ prim2 "Set.insert" (a ~> tSet a ~> tSet a) $ \x s ->
      VSet . Set.insert (Key x) <$> setOf s
  , keyed $ prim2 "Set.delete" (a ~> tSet a ~> tSet a) $ \x s ->
      VSet . Set.delete (Key x) <$> setOf s
  , keyed $ prim2 "Set.member" (a ~> tSet a ~> TBool) $ \x s ->
      VBool . Set.member (Key x) <$> setOf s
  , prim1 "Set.size" (tSet a ~> TInt) $ \s ->
      VInt . fromIntegral . Set.size <$> setOf s
  , keyed $ setOp "Set.union" Set.union
  , keyed $ setOp "Set.intersection" Set.intersection
  , keyed $ setOp "Set.difference" Set.difference
  ]
  where
    a = TVar 0
    keyed = requiring [(KeyComparable, 0)]
    setOp n f = prim2 n (tSet a ~> tSet a ~> tSet a) $ \s1 s2 ->
      VSet <$> (f <$> setOf s1 <*> setOf s2)
-- ANCHOR_END: setTable

-- ANCHOR: preludeCons
-- 🧱 The constructors of the prelude that the builtins make, with
--    their numbers in the declarations (Nothing is 0, Just is 1).
--    The prelude declares them in Lam, so a test checks that the
--    numbers here are the same.
-- 組み込みの関数が作る、プレリュードの構成子と、宣言の中での番号。
-- 宣言は Lam で書いたプレリュードにあるので、番号が同じことを
-- テストで確かめる
preludeConstructors :: [(Name, Int)]
preludeConstructors =
  [ ("Nothing", 0), ("Just", 1)                  -- Maybe a
  , ("Ok", 0), ("Err", 1)                        -- Result e a
  , ("JNull", 0), ("JBool", 1), ("JInt", 2), ("JDec", 3)
  , ("JStr", 4), ("JArr", 5), ("JObj", 6) ]      -- Json

-- 🧱 con "Just" [v] is the value Just v.
-- con "Just" [v] は、値 Just v
con :: Name -> [Value] -> Value
con c args = case lookup c preludeConstructors of
  Just i  -> VCon c i args
  Nothing -> error ("not a constructor of the prelude: " <> T.unpack c)

-- 🧱 Haskell's Maybe, as Lam's Maybe, and its type.
-- Haskell の Maybe を Lam の Maybe に。その型も
maybeValue :: Maybe Value -> Value
maybeValue m = case m of
  Just x  -> con "Just" [x]
  Nothing -> con "Nothing" []

tMaybe :: Type -> Type
tMaybe t = TCon "Maybe" [t]
-- ANCHOR_END: preludeCons

-- ANCHOR: helpers
-- ➡️ A function type, written like Lam:  String ~> Int  is  String -> Int.
-- 関数の型を、Lam のように書く
(~>) :: Type -> Type -> Type
(~>) = TFun
infixr 5 ~>

-- 🔧 Make a row for a function of one or two arguments (and prim3).
-- 引数が1つ・2つの関数の行を作る（3つの prim3 も同じ形）
prim1 :: Name -> Type -> (Value -> Either EvalError Value) -> Builtin
prim1 n t f = Builtin n (closeOver t) (VPrim n 1 run [])
  where
    run [x] = f x
    run _   = wrong n

prim2 :: Name -> Type -> (Value -> Value -> Either EvalError Value) -> Builtin
prim2 n t f = Builtin n (closeOver t) (VPrim n 2 run [])
  where
    run [x, y] = f x y
    run _      = wrong n

prim3
  :: Name -> Type
  -> (Value -> Value -> Value -> Either EvalError Value) -> Builtin
prim3 n t f = Builtin n (closeOver t) (VPrim n 3 run [])
  where
    run [x, y, z] = f x y z
    run _         = wrong n

-- 🎓 The same row, but some type variables must be in a class.
--    requiring [(HasLength, 0)] puts HasLength on t0 (that is, a).
-- 同じ行だが、型変数にクラスの条件を付ける
requiring :: [(Class, Int)] -> Builtin -> Builtin
requiring cs b = case builtinType b of
  Forall _ _ t -> b { builtinType = closeOverWith cs t }

-- 🔍 Take the Haskell value out of a Lam value. The type checker has
--    already made sure of the type, so `wrong` is never reached.
-- Lam の値から、Haskell の値を取り出す。型は型検査器が確かめてあるので、
-- wrong には来ないはず
string :: Value -> Either EvalError Text
string v = case v of
  VString s -> Right s
  _         -> wrong "a string"

list :: Value -> Either EvalError [Value]
list v = case v of
  VList xs -> Right xs
  _        -> wrong "a list"

mapOf :: Value -> Either EvalError (Map Key Value)
mapOf v = case v of
  VMap m -> Right m
  _      -> wrong "a map"

setOf :: Value -> Either EvalError (Set Key)
setOf v = case v of
  VSet s -> Right s
  _      -> wrong "a set"

onString :: (Text -> Text) -> Value -> Either EvalError Value
onString f v = VString . f <$> string v

pairPart :: ((Value, Value) -> Value) -> Value -> Either EvalError Value
pairPart part v = case v of
  VTuple [x, y] -> Right (part (x, y))
  _             -> wrong "a pair"

-- 「型が合いません（ここには来ないはず）」
wrong :: Text -> Either EvalError a
wrong what = Left (BuiltinFailed what "got a value of the wrong type")
-- ANCHOR_END: helpers
-- ANCHOR_END: all
