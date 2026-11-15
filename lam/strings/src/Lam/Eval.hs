{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The evaluator: walk the tree and compute a value.
-- 評価器：木をたどって、値を計算する。
module Lam.Eval
  ( Value (..)
  , Env
  , EvalError (..)
  , eval
  , binOp
  , renderValue
  , renderError
  ) where

-- The LAZY Map: a value is not computed when it is put in.
-- The knot of let needs this (see the chapter).
-- 遅延な Map：値は、入れたときには計算されない。
-- let の結び目には、これが要る（本文を参照）
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Pretty (opSymbol, renderString)
import Lam.Syntax

-- ANCHOR: value
-- 💎 What a Lam program computes.
-- Lam のプログラムが計算するもの
data Value
  = VInt Integer
  | VBool Bool
  | VString Text           -- "hello"
  | VTuple [Value]         -- (1, "one")
  | VList [Value]          -- [1, 2, 3]
  -- A function, together with the variables it could see when it
  -- was made (a closure).
  -- 関数と、それが作られたときに見えていた変数たち（クロージャ）
  | VClosure Name Expr Env
  -- A function written in Haskell: its name, how many arguments
  -- it needs, what it does, and the arguments given so far.
  -- Haskell で書いた関数：名前、要る引数の数、すること、ここまでの引数
  | VPrim Name Int ([Value] -> Either EvalError Value) [Value]

-- 📒 The environment: the value of each variable.
-- 環境：それぞれの変数の値
type Env = Map Name Value

-- 🚨 Things that can go wrong while computing.
-- 計算の途中で起きうるまちがい
data EvalError
  = UnboundVariable Name
  | NotAFunction Text
  | WrongOperand Op Text
  | IfNeedsBool Text
  | DivideByZero
  | CannotCompare Text           -- a function cannot be compared
  | BuiltinFailed Name Text      -- head []
  deriving (Eq, Show)
-- ANCHOR_END: value

-- ANCHOR: eval
-- ⚙️ The value of an expression, in an environment.
-- 環境の中での、式の値
eval :: Env -> Expr -> Either EvalError Value
eval env expr = case expr of
  Lit (LInt n)    -> Right (VInt n)
  Lit (LBool b)   -> Right (VBool b)
  Lit (LString s) -> Right (VString s)

  Var x -> case Map.lookup x env of
    Just v  -> Right v
    Nothing -> Left (UnboundVariable x)

  -- A function remembers the environment of NOW.
  -- 関数は、「いま」の環境を覚えておく
  Lam x body -> Right (VClosure x body env)

  -- ANCHOR: app
  App f a -> do
    fv <- eval env f
    av <- eval env a
    apply fv av
  -- ANCHOR_END: app

  If c t e -> do
    cv <- eval env c
    case cv of
      VBool True  -> eval env t
      VBool False -> eval env e
      other       -> Left (IfNeedsBool (renderValue other))

  -- ANCHOR: collections
  -- Compute every part, from left to right.
  -- 部品を、左から順に全部計算する
  Tuple es -> VTuple <$> traverse (eval env) es
  List es  -> VList  <$> traverse (eval env) es
  -- ANCHOR_END: collections

  -- ANCHOR: shortcut
  -- && and || look at the right side only when they need it.
  -- && と || は、要るときだけ右側を見る
  BinOp And l r -> do
    lv <- eval env l
    case lv of
      VBool False -> Right (VBool False)   -- false && ...  is false
      VBool True  -> eval env r            -- true && r     is r
      other       -> Left (WrongOperand And (renderValue other))

  BinOp Or l r -> do
    lv <- eval env l
    case lv of
      VBool True  -> Right (VBool True)    -- true || ...   is true
      VBool False -> eval env r            -- false || r    is r
      other       -> Left (WrongOperand Or (renderValue other))
  -- ANCHOR_END: shortcut

  -- The other operators need both sides.
  -- ほかの演算子は、両側とも要る
  BinOp op l r -> do
    lv <- eval env l
    rv <- eval env r
    binOp op lv rv

  -- ANCHOR: let
  Let x e body -> do
    -- let is recursive: e can see x. env' holds v, and v is made
    -- from env'. Laziness ties this knot.
    -- let は再帰する：e から x が見える。env' は v を持ち、
    -- v は env' から作られる。遅延評価が、この結び目を結ぶ
    let env'   = Map.insert x v env
        result = eval env' e
        v      = either (const (VInt 0)) id result
    _ <- result
    eval env' body
  -- ANCHOR_END: let
-- ANCHOR_END: eval

-- ANCHOR: apply
-- 📞 Call a function: the body runs in the function's OWN environment,
--    plus the argument.
-- 関数を呼ぶ：本体は、関数「自身」の環境に、引数を足して動く
apply :: Value -> Value -> Either EvalError Value
apply (VClosure x body closed) arg = eval (Map.insert x arg closed) body
-- A builtin waits until it has all its arguments, then runs.
-- 組み込みの関数は、引数がそろうまで待ってから動く
apply (VPrim n arity run args) arg
  | length args' == arity = run args'
  | otherwise             = Right (VPrim n arity run args')
  where
    args' = args <> [arg]
apply other _ = Left (NotAFunction (renderValue other))
-- ANCHOR_END: apply

-- ANCHOR: binOp
-- ➕ The operators that need both sides.
-- 両側とも要る演算子
binOp :: Op -> Value -> Value -> Either EvalError Value
binOp op lv rv = case (op, lv, rv) of
  (Add, VInt a, VInt b) -> Right (VInt (a + b))
  (Sub, VInt a, VInt b) -> Right (VInt (a - b))
  (Mul, VInt a, VInt b) -> Right (VInt (a * b))
  (Div, VInt a, VInt b)
    | b == 0            -> Left DivideByZero
    | otherwise         -> Right (VInt (a `div` b))
  (Concat, VString a, VString b) -> Right (VString (a <> b))
  (Cons, x, VList xs)            -> Right (VList (x : xs))
  _ | Just test <- comparison op -> do
        o <- compareValues lv rv
        Right (VBool (test o))
    | otherwise -> Left (WrongOperand op (renderValue rv))

-- 🔍 What each comparing operator wants to see.
-- 比べる演算子が、それぞれ見たい答え
comparison :: Op -> Maybe (Ordering -> Bool)
comparison op = case op of
  Eq -> Just (== EQ)
  Ne -> Just (/= EQ)
  Lt -> Just (== LT)
  Le -> Just (/= GT)
  Gt -> Just (== GT)
  Ge -> Just (/= LT)
  _  -> Nothing
-- ANCHOR_END: binOp

-- ANCHOR: compare
-- ⚖️ Compare two values of the same type. Lists and tuples are
--    compared from the left, like words in a dictionary.
--    Functions cannot be compared.
-- 同じ型の2つの値を比べる。リストとタプルは、辞書の言葉のように
-- 左から比べる。関数は比べられない
compareValues :: Value -> Value -> Either EvalError Ordering
compareValues a b = case (a, b) of
  (VInt x,    VInt y)    -> Right (compare x y)
  (VBool x,   VBool y)   -> Right (compare x y)
  (VString x, VString y) -> Right (compare x y)
  (VTuple xs, VTuple ys) -> compareLists xs ys
  (VList xs,  VList ys)  -> compareLists xs ys
  _                      -> Left (CannotCompare (renderValue a))

compareLists :: [Value] -> [Value] -> Either EvalError Ordering
compareLists xs ys = case (xs, ys) of
  ([],      [])      -> Right EQ
  ([],      _)       -> Right LT
  (_,       [])      -> Right GT
  (x : xs', y : ys') -> do
    o <- compareValues x y
    if o == EQ then compareLists xs' ys' else Right o
-- ANCHOR_END: compare

-- ANCHOR: render
-- 🖨️ A value, as the REPL shows it.
-- REPL が見せる値
renderValue :: Value -> Text
renderValue v = case v of
  VInt n         -> T.pack (show n)
  VBool True     -> "true"
  VBool False    -> "false"
  VString s      -> renderString s
  VTuple vs      -> "(" <> T.intercalate ", " (map renderValue vs) <> ")"
  VList vs       -> "[" <> T.intercalate ", " (map renderValue vs) <> "]"
  VClosure x _ _ -> "<function of " <> x <> ">"
  VPrim n _ _ _  -> "<builtin " <> n <> ">"

-- 📣 An error, as people read it.
-- 人が読むエラー
renderError :: EvalError -> Text
renderError err = case err of
  -- 「知らない変数です」
  UnboundVariable x  -> "unknown variable: " <> x
  -- 「関数ではないので、呼べません」
  NotAFunction v     -> "not a function, cannot call: " <> v
  -- 「この演算子には使えません」
  WrongOperand op v  -> opSymbol op <> " cannot use " <> v
  -- 「if の条件は真偽値でなければなりません」
  IfNeedsBool v      -> "if needs true or false, but got " <> v
  -- 「0 で割りました」
  DivideByZero       -> "divided by zero"
  -- 「関数は比べられません」
  CannotCompare v    -> "cannot compare a function: " <> v
  -- 「（組み込みの関数）：……」
  BuiltinFailed n m  -> n <> ": " <> m
-- ANCHOR_END: render
-- ANCHOR_END: all
