-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the language design chapter.
-- 言語を設計する章のトレーニングの解答。
module Lam.Answers
  ( size
  , constants
  , twice
  ) where

import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: size
-- 📏 How many nodes the tree has.
-- 木の節の数
size :: Expr -> Int
size expr = case expr of
  Lit _       -> 1
  Var _       -> 1
  Lam _ b     -> 1 + size b
  App f a     -> 1 + size f + size a
  Let _ e b   -> 1 + size e + size b
  If c a b    -> 1 + size c + size a + size b
  BinOp _ l r -> 1 + size l + size r
-- ANCHOR_END: size

-- ANCHOR: constants
-- 🔢 Every integer written in the program, from left to right.
-- プログラムに書かれた整数を、左から順に全部
constants :: Expr -> [Integer]
constants expr = case expr of
  Lit (LInt n)  -> [n]
  Lit (LBool _) -> []
  Var _         -> []
  Lam _ b       -> constants b
  App f a       -> constants f <> constants a
  Let _ e b     -> constants e <> constants b
  If c a b      -> constants c <> constants a <> constants b
  BinOp _ l r   -> constants l <> constants r
-- ANCHOR_END: constants

-- ANCHOR: twice
-- let twice = \f -> \x -> f (f x) in twice (\n -> n + 3) 10
twice :: Expr
twice =
  Let "twice"
    (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))
    (App (App (Var "twice") (Lam "n" (BinOp Add (Var "n") (Lit (LInt 3)))))
         (Lit (LInt 10)))
-- ANCHOR_END: twice
-- ANCHOR_END: all
