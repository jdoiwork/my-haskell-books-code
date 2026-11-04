{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Some Lam programs, written as Haskell values (no parser yet).
-- Haskell の値として書いた、Lam のプログラム（まだパーサーがない）。
module Lam.Examples
  ( double21
  , factorial10
  , adder
  ) where

import Lam.Syntax

-- ANCHOR: double
-- let double = \x -> x * 2 in double 21
double21 :: Expr
double21 =
  Let "double" (Lam "x" (BinOp Mul (Var "x") (Lit (LInt 2))))
    (App (Var "double") (Lit (LInt 21)))
-- ANCHOR_END: double

-- ANCHOR: fact
-- let fact = \n -> if n == 0 then 1 else n * fact (n - 1) in fact 10
factorial10 :: Expr
factorial10 =
  Let "fact"
    (Lam "n"
      (If (BinOp Eq (Var "n") (Lit (LInt 0)))
          (Lit (LInt 1))
          (BinOp Mul (Var "n")
                     (App (Var "fact")
                          (BinOp Sub (Var "n") (Lit (LInt 1)))))))
    (App (Var "fact") (Lit (LInt 10)))
-- ANCHOR_END: fact

-- ANCHOR: adder
-- \x -> \y -> x + y   (a function that returns a function)
-- （関数を返す関数）
adder :: Expr
adder = Lam "x" (Lam "y" (BinOp Add (Var "x") (Var "y")))
-- ANCHOR_END: adder
-- ANCHOR_END: all
