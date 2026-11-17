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
  , prim4
  , requiring
  , (~>)
  , string
  , list
  , mapOf
    -- The constructors of the prelude, made from Haskell.
    -- プレリュードの構成子を、Haskell から作る
  , preludeConstructors
  , con
  , maybeValue
  ) where

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

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
  <> wordBuiltins
  <> oneLinerBuiltins
  where
    a = TVar 0
    b = TVar 1
-- ANCHOR_END: table

-- ANCHOR: wordTable
-- ✂️ For scripts: cut a text into lines and words, and sort.
-- スクリプトのため：文字列を行と単語に切る、並べ替える
wordBuiltins :: [Builtin]
wordBuiltins =
  -- "a\nb\n" gives ["a", "b"], and words cuts at any spaces.
  -- 行に切る・空白で単語に切る（Haskell の lines・words と同じ）
  [ prim1 "lines" (tString ~> tList tString) (cutWith T.lines)
  , prim1 "words" (tString ~> tList tString) (cutWith T.words)
  -- Sort from the smallest: Comparable a => [a] -> [a]
  -- 小さい順に並べる
  , requiring [(Comparable, 0)] $ prim1 "sort" (tList a ~> tList a) $ \v -> do
      xs <- list v
      Right (VList (sortBy order xs))
  -- Sort by a key that a Lam function gives:
  --   sortOn (\p -> 0 - snd p) pairs   (the biggest count first)
  -- Lam の関数がくれるキーで並べる
  , requiring [(Comparable, 1)] $
      prim2 "sortOn" ((a ~> b) ~> tList a ~> tList a) $ \f v -> do
        xs   <- list v
        -- Each key is made once, before sorting.
        -- キーは、並べる前に1回ずつ作る
        keys <- traverse (apply f) xs
        Right (VList (map snd (sortBy (\x y -> order (fst x) (fst y))
                                       (zip keys xs))))
  ]
  where
    a = TVar 0
    b = TVar 1
    cutWith f v = VList . map VString . f <$> string v
    -- The type checker made sure that these can be compared (no
    -- functions inside), so compareValues gives Right here.
    -- 型検査器が、比べられる（中に関数がない）ことを確かめてあるので、
    -- ここで compareValues は Right をくれる
    order x y = either (const EQ) id (compareValues x y)
-- ANCHOR_END: wordTable

-- ANCHOR: oneLinerTable
-- 🔢 For one-liners: a number from text, and the columns of a line.
-- ワンライナーのため：文字列から数を、行から列を
oneLinerBuiltins :: [Builtin]
oneLinerBuiltins =
  -- toInt " 42" is Just 42, toInt "12abc" is Nothing.
  -- 前後の空白は飛ばす。数でなければ Nothing
  [ prim1 "toInt" (tString ~> TCon "Maybe" [TInt]) $ \v -> do
      s <- string v
      Right (maybeValue (VInt <$> wholeInt (T.strip s)))
  -- $field line fields n: what -a gives as field. 0 is the whole line,
  -- 1 the first column, and a column that is not there is "".
  -- -a の field の中身。0 は行全体、1 は1列目。ない列は ""
  , prim3 "$field" (tString ~> tList tString ~> TInt ~> tString) $
      \l vs i -> case i of
        VInt 0 -> Right l
        VInt n | n > 0 -> do
          cols <- list vs
          Right (case drop (fromInteger n - 1) cols of
                   col : _ -> col
                   []      -> VString "")
        _ -> Right (VString "")
  ]
  where
    -- The whole text must be the number: "+7", "-3", "120".
    -- 文字列の全部が数のときだけ
    wholeInt s = case TR.signed TR.decimal s of
      Right (n, rest) | T.null rest -> Just n
      _                             -> Nothing
-- ANCHOR_END: oneLinerTable

-- ANCHOR: mapTable
-- 🗺️ Maps from strings. Each name starts with Map., like Haskell's
--    qualified import (import qualified Data.Map as Map).
-- 文字列をキーにした Map。名前は、Haskell の qualified な import と
-- 同じく、Map. で始める
mapBuiltins :: [Builtin]
mapBuiltins =
  [ Builtin "Map.empty" (closeOver (tMap a)) (VMap Map.empty)
  , prim3 "Map.insert" (tString ~> a ~> tMap a ~> tMap a) $ \k v m -> do
      key <- string k
      VMap . Map.insert key v <$> mapOf m
  , prim2 "Map.lookup" (tString ~> tMap a ~> tMaybe a) $ \k m -> do
      key <- string k
      found <- Map.lookup key <$> mapOf m
      Right $ case found of
        Just v  -> con "Just" [v]
        Nothing -> con "Nothing" []
  , prim2 "Map.delete" (tString ~> tMap a ~> tMap a) $ \k m -> do
      key <- string k
      VMap . Map.delete key <$> mapOf m
  , prim2 "Map.member" (tString ~> tMap a ~> TBool) $ \k m -> do
      key <- string k
      VBool . Map.member key <$> mapOf m
  , prim1 "Map.size" (tMap a ~> TInt) $ \m ->
      VInt . fromIntegral . Map.size <$> mapOf m
  , prim1 "Map.toList" (tMap a ~> tList (tTuple [tString, a])) $ \m -> do
      kvs <- Map.toList <$> mapOf m
      Right (VList [ VTuple [VString k, v] | (k, v) <- kvs ])
  , prim1 "Map.fromList" (tList (tTuple [tString, a]) ~> tMap a) $ \v -> do
      kvs <- list v >>= traverse keyValue
      Right (VMap (Map.fromList kvs))
  , prim1 "Map.keys" (tMap a ~> tList tString) $ \m ->
      VList . map VString . Map.keys <$> mapOf m
  , prim1 "Map.values" (tMap a ~> tList a) $ \m ->
      VList . Map.elems <$> mapOf m
  ]
  where
    a = TVar 0
    tMaybe t = TCon "Maybe" [t]
    keyValue v = case v of
      VTuple [k, x] -> do
        key <- string k
        Right (key, x)
      _ -> wrong "a pair"
-- ANCHOR_END: mapTable

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
  , ("JStr", 4), ("JArr", 5), ("JObj", 6)       -- Json
  , ("Match", 0)                                 -- Match
  -- ANCHOR: outsideCons
  , ("Response", 0)                              -- Http.Response
  , ("Db", 0) ]                                  -- Sql.Db
  -- ANCHOR_END: outsideCons

-- 🧱 con "Just" [v] is the value Just v.
-- con "Just" [v] は、値 Just v
con :: Name -> [Value] -> Value
con c args = case lookup c preludeConstructors of
  Just i  -> VCon c i args
  Nothing -> error ("not a constructor of the prelude: " <> T.unpack c)

-- 🧱 A Haskell Maybe, as a Lam Maybe.
-- Haskell の Maybe を、Lam の Maybe に
maybeValue :: Maybe Value -> Value
maybeValue m = case m of
  Just v  -> con "Just" [v]
  Nothing -> con "Nothing" []
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

-- ANCHOR: prim4
-- 🧰 A builtin with four arguments (Http.send).
-- 引数が4つの組み込みの関数（Http.send）
prim4
  :: Name -> Type
  -> (Value -> Value -> Value -> Value -> Either EvalError Value)
  -> Builtin
prim4 n t f = Builtin n (closeOver t) (VPrim n 4 run [])
  where
    run [w, x, y, z] = f w x y z
    run _            = wrong n
-- ANCHOR_END: prim4

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

mapOf :: Value -> Either EvalError (Map Text Value)
mapOf v = case v of
  VMap m -> Right m
  _      -> wrong "a map"

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
