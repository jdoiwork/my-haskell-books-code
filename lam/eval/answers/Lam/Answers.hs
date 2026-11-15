-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the evaluator chapter.
-- 評価器の章のトレーニングの解答。
module Lam.Answers
  ( evalDynamic
  , showEnv
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Eval
import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: dynamic
-- 🌀 Dynamic scope: a function's body sees the variables of the
--    place where it is CALLED, not where it was made.
-- 動的スコープ：関数の本体には、作られた場所ではなく、
-- 「呼ばれた」場所の変数が見える
evalDynamic :: Env -> Expr -> Either EvalError Value
evalDynamic env expr = case expr of
  App f a -> do
    fv <- evalDynamic env f
    av <- evalDynamic env a
    case fv of
      -- Use env (the caller's), not the closure's own environment.
      -- クロージャ自身の環境ではなく、呼んだ側の env を使う
      VClosure x body _ -> evalDynamic (Map.insert x av env) body
      other             -> Left (NotAFunction (renderValue other))
  -- No knot is needed: a function body sees the caller's variables,
  -- and the caller can already see the function itself.
  -- 結び目は要らない：関数の本体には呼んだ側の変数が見え、
  -- 呼んだ側には、もう関数自身が見えているから
  Let x e body -> do
    v <- evalDynamic env e
    evalDynamic (Map.insert x v env) body
  -- Everything else is the same as eval, one level down.
  -- それ以外は eval と同じ（1段だけ下ろす）
  Lam x body -> Right (VClosure x body env)
  If c t e -> do
    cv <- evalDynamic env c
    case cv of
      VBool True  -> evalDynamic env t
      VBool False -> evalDynamic env e
      other       -> Left (IfNeedsBool (renderValue other))
  BinOp op l r -> do
    lv <- evalDynamic env l
    rv <- evalDynamic env r
    binOp op lv rv
  -- Numbers, true/false, and variables: the same as eval.
  -- 数、真偽値、変数：eval と同じ
  _ -> eval env expr
-- ANCHOR_END: dynamic

-- ANCHOR: showEnv
-- 📒 Every variable with its value, one per line.
-- すべての変数と、その値を1行ずつ
showEnv :: Env -> Text
showEnv env =
  T.intercalate "\n"
    [ x <> " = " <> renderValue v | (x, v) <- Map.toList env ]
-- ANCHOR_END: showEnv
-- ANCHOR_END: all
