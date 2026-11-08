-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Turn an AST back into Lam source code, with as few
--   parentheses as possible.
-- AST を、Lam のソースコードに戻す。括弧はできるだけ少なく。
module Lam.Pretty
  ( render
  , opSymbol
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: levels
-- 🪜 How tightly each kind of expression holds together.
--    A bigger number holds tighter: 2 * 3 + 4 is (2 * 3) + 4.
-- 式の種類ごとの、結びつきの強さ。大きいほど強く結びつく
level :: Expr -> Int
level expr = case expr of
  Lam {}        -> 0   -- \x -> ...     （右の端まで続く）
  Let {}        -> 0   -- let ... in ...
  If {}         -> 0   -- if ... then ... else ...
  BinOp op _ _  -> opLevel op
  App {}        -> 4   -- f x
  Lit _         -> 5   -- 42
  Var _         -> 5   -- x

opLevel :: Op -> Int
opLevel op = case op of
  Eq  -> 1
  Lt  -> 1
  Add -> 2
  Sub -> 2
  Mul -> 3
  Div -> 3

opSymbol :: Op -> Text
opSymbol op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Eq  -> "=="
  Lt  -> "<"
-- ANCHOR_END: levels

-- ANCHOR: render
-- 🖨️ The source code of an expression.
-- 式のソースコード
render :: Expr -> Text
render = at 0

-- Render e where at least level p is needed: if e holds looser
-- than that, wrap it in parentheses.
-- 少なくとも強さ p が要る場所に e を書く。e がそれより弱ければ、括弧で包む
at :: Int -> Expr -> Text
at p e
  | level e < p = "(" <> plain e <> ")"
  | otherwise   = plain e

plain :: Expr -> Text
plain expr = case expr of
  Lit (LInt n)      -> T.pack (show n)
  Lit (LBool True)  -> "true"
  Lit (LBool False) -> "false"
  Var x             -> x
  Lam x body        -> "\\" <> x <> " -> " <> at 0 body
  App f a           -> at 4 f <> " " <> at 5 a
  Let x e body      -> "let " <> x <> " = " <> at 0 e <> " in " <> at 0 body
  If c a b          -> "if " <> at 0 c <> " then " <> at 0 a
                         <> " else " <> at 0 b
  BinOp op l r      ->
    let n = opLevel op
        -- + - * / lean to the left: 1 - 2 - 3 is (1 - 2) - 3.
        -- == and < do not chain, so both sides need more.
        -- + - * / は左に寄る。== と < は続けられないので、両側とも強さが要る
        (lp, rp) = if n == 1 then (n + 1, n + 1) else (n, n + 1)
    in at lp l <> " " <> opSymbol op <> " " <> at rp r
-- ANCHOR_END: render
-- ANCHOR_END: all
