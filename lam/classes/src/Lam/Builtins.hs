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
  , requiring
  , wrong
  , (~>)
  , string
  , list
  ) where

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
  , prim1 "null" (tList a ~> TBool) $ \v -> do
      xs <- list v
      Right (VBool (null xs))
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
  where
    a = TVar 0
    b = TVar 1
-- ANCHOR_END: table

-- ANCHOR: helpers
-- ➡️ A function type, written like Lam:  String ~> Int  is  String -> Int.
-- 関数の型を、Lam のように書く
(~>) :: Type -> Type -> Type
(~>) = TFun
infixr 5 ~>

-- 🔧 Make a row for a function of one or two arguments.
-- 引数が1つ・2つの関数の行を作る
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
