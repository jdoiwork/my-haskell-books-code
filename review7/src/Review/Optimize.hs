-- ANCHOR: all
-- | A tiny optimizer for Lam: rewrite the tree into a simpler tree
--   that computes the same answer.
-- Lam の小さな最適化：木を、同じ答えを出す、もっと単純な木に書き換える。
module Review.Optimize
  ( optimize
  , optimizeWith
  , foldStep
  , optimizeDraft
  ) where

import Lam.Syntax

-- ANCHOR: optimize
-- 🧹 Optimize the children first, then this node.
-- 先に子どもを、それからこの節を最適化する
optimize :: Expr -> Expr
optimize = optimizeWith foldStep

-- 🧹 The same, with any rewrite rule for one node.
-- 同じことを、1つの節を書き換えるどんな規則でも
optimizeWith :: (Expr -> Expr) -> Expr -> Expr
optimizeWith rule = go
  where
    go expr = rule $ case expr of
      Lam x b      -> Lam x (go b)
      App f a      -> App (go f) (go a)
      Let x e b    -> Let x (go e) (go b)
      If c a b     -> If (go c) (go a) (go b)
      BinOp o l r  -> BinOp o (go l) (go r)
      _            -> expr
-- ANCHOR_END: optimize

-- ANCHOR: step
-- ✂️ One rewrite at the top of the tree.
-- 木のてっぺんで、1回だけ書き換える
foldStep :: Expr -> Expr
foldStep expr = case expr of
  -- 2 + 3  ->  5   (but leave 1 / 0 alone: it must still fail)
  -- 2 + 3 -> 5（ただし 1 / 0 はそのまま：失敗しなければならない）
  BinOp op (Lit (LInt a)) (Lit (LInt b)) | not (op == Div && b == 0) ->
    Lit (compute op a b)
  -- if true then a else b  ->  a
  If (Lit (LBool True))  a _ -> a
  If (Lit (LBool False)) _ b -> b
  -- x + 0, 0 + x, x * 1, 1 * x, x - 0  ->  x
  BinOp Add x (Lit (LInt 0)) -> x
  BinOp Add (Lit (LInt 0)) x -> x
  BinOp Sub x (Lit (LInt 0)) -> x
  BinOp Mul x (Lit (LInt 1)) -> x
  BinOp Mul (Lit (LInt 1)) x -> x
  _ -> expr
  where
    compute op a b = case op of
      Add -> LInt (a + b)
      Sub -> LInt (a - b)
      Mul -> LInt (a * b)
      Div -> LInt (a `div` b)
      Eq  -> LBool (a == b)
      Lt  -> LBool (a < b)
-- ANCHOR_END: step

-- ANCHOR: draft
-- 🐛 A draft with one more rule: x * 0 -> 0. Is it safe?
-- 規則を1つ足した下書き：x * 0 -> 0。これは安全？
optimizeDraft :: Expr -> Expr
optimizeDraft = optimizeWith (timesZero . foldStep)
  where
    timesZero e = case e of
      BinOp Mul _ (Lit (LInt 0)) -> Lit (LInt 0)
      other                      -> other
-- ANCHOR_END: draft
-- ANCHOR_END: all
