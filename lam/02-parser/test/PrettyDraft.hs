{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | A draft of render with a bug, to see QuickCheck find it.
-- バグのある render の下書き。QuickCheck が見つけるのを見るため。
module PrettyDraft (renderDraft) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Pretty (opSymbol)
import Lam.Syntax

level :: Expr -> Int
level expr = case expr of
  Lam {}        -> 0
  Let {}        -> 0
  If {}         -> 0
  BinOp op _ _  -> opLevel op
  App {}        -> 4
  Lit _         -> 5
  Var _         -> 5

opLevel :: Op -> Int
opLevel op = case op of
  Eq  -> 1
  Lt  -> 1
  Add -> 2
  Sub -> 2
  Mul -> 3
  Div -> 3

-- ANCHOR: draft
-- 🖨️ The source code of an expression.
-- 式のソースコード
renderDraft :: Expr -> Text
renderDraft = at 0

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
        -- BUG: == and < are treated like + (they lean to the left).
        -- バグ：== と < を、+ と同じく左に寄るものとして扱っている
        (lp, rp) = (n, n + 1)
    in at lp l <> " " <> opSymbol op <> " " <> at rp r
-- ANCHOR_END: draft
-- ANCHOR_END: all
