-- ANCHOR: all
-- | The abstract syntax tree (AST) of Lam: a program, as a value.
-- Lam の抽象構文木（AST）：プログラムを、値として表す。
module Lam.Syntax
  ( Name
  , Expr (..)
  , Literal (..)
  , Op (..)
  , freeVars
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- ANCHOR: expr
-- 🏷️ A variable's name: x, double, fact
-- 変数の名前
type Name = Text

-- 🌳 An expression of Lam. Everything in Lam is an expression.
-- Lam の式。Lam では、すべてが式
data Expr
  = Lit Literal            -- 42, true
  | Var Name               -- x
  | Lam Name Expr          -- \x -> body
  | App Expr Expr          -- f x
  | Let Name Expr Expr     -- let x = e in body
  | If Expr Expr Expr      -- if c then a else b
  | BinOp Op Expr Expr     -- a + b
  deriving (Eq, Show)

-- 🔢 The values you can write directly.
-- そのまま書ける値
data Literal
  = LInt Integer           -- 42
  | LBool Bool             -- true, false
  deriving (Eq, Show)

-- ➕ The operators.
-- 演算子
data Op = Add | Sub | Mul | Div | Eq | Lt
  deriving (Eq, Show, Enum, Bounded)
-- ANCHOR_END: expr

-- ANCHOR: freeVars
-- 🔓 The variables used but not defined inside the expression.
--    \x -> x + y  uses y from outside.
-- 式の中で使われているのに、中で決められていない変数。
-- \x -> x + y は、外の y を使っている
freeVars :: Expr -> Set Name
freeVars expr = case expr of
  Lit _           -> Set.empty
  Var x           -> Set.singleton x
  Lam x body      -> Set.delete x (freeVars body)
  App f a         -> freeVars f <> freeVars a
  -- let is recursive: x is defined in both e and body.
  -- let は再帰する：x は e の中でも body の中でも決まっている
  Let x e body    -> Set.delete x (freeVars e <> freeVars body)
  If c a b        -> freeVars c <> freeVars a <> freeVars b
  BinOp _ l r     -> freeVars l <> freeVars r
-- ANCHOR_END: freeVars
-- ANCHOR_END: all
