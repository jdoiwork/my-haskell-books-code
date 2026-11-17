{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The abstract syntax tree (AST) of Lam: a program, as a value.
-- Lam の抽象構文木（AST）：プログラムを、値として表す。
module Lam.Syntax
  ( Name
  , Expr (..)
  , Literal (..)
  , Op (..)
  , Pattern (..)
  , TypeExpr (..)
  , DataDecl (..)
  , Decl (..)
  , Loc (..)
  , renderLoc
  , stripLocations
  , freeVars
  , patternVars
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- ANCHOR: expr
-- 🏷️ A variable's name: x, double, fact
-- 変数の名前
type Name = Text

-- 🌳 An expression of Lam. Everything in Lam is an expression.
-- Lam の式。Lam では、すべてが式
data Expr
  = Lit Literal            -- 42, true, "hello"
  | Var Name               -- x, and constructors: Just, Circle
  | Lam Name Expr          -- \x -> body
  | App Expr Expr          -- f x
  | Let Name Expr Expr     -- let x = e in body
  | If Expr Expr Expr      -- if c then a else b
  | BinOp Op Expr Expr     -- a + b
  | Tuple [Expr]           -- (1, "one"), and () with none
  | List [Expr]            -- [1, 2, 3]
  | Case Expr [(Pattern, Expr)]   -- case e of | p -> a | q -> b
  -- ANCHOR: at
  -- A mark of where the expression inside was written. It does not
  -- change the value: only an error looks at it.
  -- 中の式を書いた場所の印。値は変えない。見るのはエラーだけ
  | At Loc Expr
  -- ANCHOR_END: at
  deriving (Eq, Show)
-- ANCHOR_END: expr

-- ANCHOR: loc
-- 📍 A place in a file: the name, the line and the column (from 1).
-- ファイルの中の場所：名前、行、列（どれも 1 から）
data Loc = Loc FilePath Int Int
  deriving (Eq, Show)

-- 📍 wordfreq.lam:12:15  (the same shape as GHC and gcc)
-- GHC や gcc と同じ形
renderLoc :: Loc -> Text
renderLoc (Loc file line column) =
  T.pack file <> ":" <> T.pack (show line) <> ":" <> T.pack (show column)
-- ANCHOR_END: loc

-- ANCHOR: strip
-- 🧽 The same tree without the marks of places, to compare trees in
--    the tests.
-- 場所の印を取った、同じ木。テストで木を比べるため
stripLocations :: Expr -> Expr
stripLocations expr = case expr of
  At _ e       -> stripLocations e
  Lit l        -> Lit l
  Var x        -> Var x
  Lam x b      -> Lam x (stripLocations b)
  App f a      -> App (stripLocations f) (stripLocations a)
  Let x e b    -> Let x (stripLocations e) (stripLocations b)
  If c a b     -> If (stripLocations c) (stripLocations a) (stripLocations b)
  BinOp o l r  -> BinOp o (stripLocations l) (stripLocations r)
  Tuple es     -> Tuple (map stripLocations es)
  List es      -> List (map stripLocations es)
  Case e alts  -> Case (stripLocations e)
                    [ (p, stripLocations b) | (p, b) <- alts ]
-- ANCHOR_END: strip

-- 🔢 The values you can write directly.
-- そのまま書ける値
data Literal
  = LInt Integer           -- 42
  | LBool Bool             -- true, false
  | LString Text           -- "hello"
  deriving (Eq, Show)

-- ➕ The operators.
-- 演算子
data Op
  = Add | Sub | Mul | Div            -- + - * /
  | Eq | Ne | Lt | Le | Gt | Ge      -- == /= < <= > >=
  | Concat                           -- ++  (strings)
  | Cons                             -- :   (an item in front of a list)
  | And | Or                         -- && ||
  deriving (Eq, Show, Enum, Bounded)

-- ANCHOR: pattern
-- 🧩 A pattern: the shape a value must have, with names for its parts.
-- パターン：値の形と、部品に付ける名前
data Pattern
  = PVar Name                -- x       (anything; call it x)
  | PWild                    -- _       (anything; no name)
  | PLit Literal             -- 0, "hi", true
  | PCon Name [Pattern]      -- Just x, Circle r, Nothing
  | PTuple [Pattern]         -- (name, score)
  | PList [Pattern]          -- [], [x], [x, y]
  | PCons Pattern Pattern    -- x : rest
  deriving (Eq, Show)
-- ANCHOR_END: pattern

-- ANCHOR: decl
-- 🏷️ A type, as written in a data declaration: Int, [a], Maybe a, a -> b
-- data の宣言に書く型
data TypeExpr
  = TyCon Name [TypeExpr]    -- Int, Maybe a, Tree a
  | TyVar Name               -- a
  | TyFun TypeExpr TypeExpr  -- a -> b
  | TyList TypeExpr          -- [a]
  | TyTuple [TypeExpr]       -- (a, b)
  deriving (Eq, Show)

-- 📐 data Shape = Circle Int | Rect Int Int
--    The name of the type, its type variables, and the constructors
--    with the types of their arguments.
-- 型の名前、型変数、構成子とその引数の型
data DataDecl = DataDecl
  { dataName   :: Name                  -- Shape
  , dataParams :: [Name]                -- []   (Maybe a has [a])
  , dataCons   :: [(Name, [TypeExpr])]  -- Circle [Int], Rect [Int, Int]
  }
  deriving (Eq, Show)

-- 📜 One line of a program: a definition, or a data declaration.
-- プログラムの1つの宣言：定義か、data の宣言
data Decl
  = DefValue Name Expr       -- let x = e
  | DefData DataDecl         -- data Shape = ...
  deriving (Eq, Show)
-- ANCHOR_END: decl

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
  Tuple es        -> foldMap freeVars es
  List es         -> foldMap freeVars es
  -- The names in a pattern are defined in its branch.
  -- パターンの名前は、その枝の中で決まっている
  Case e alts     -> freeVars e <> foldMap
    (\(p, body) -> freeVars body `Set.difference` patternVars p) alts
  At _ e          -> freeVars e

-- 🧩 The names a pattern gives:  (x, Just y)  gives x and y.
-- パターンが付ける名前
patternVars :: Pattern -> Set Name
patternVars p = case p of
  PVar x      -> Set.singleton x
  PWild       -> Set.empty
  PLit _      -> Set.empty
  PCon _ ps   -> foldMap patternVars ps
  PTuple ps   -> foldMap patternVars ps
  PList ps    -> foldMap patternVars ps
  PCons h t   -> patternVars h <> patternVars t
-- ANCHOR_END: freeVars
-- ANCHOR_END: all
