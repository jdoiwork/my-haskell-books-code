-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the chapter on patterns and sugar.
-- パターンと糖衣構文の章のトレーニングの解答。
module Lam.Answers
  ( lambdaCase
  , canFail
  , riskyPatterns
  ) where

import qualified Data.Map as Map

import Lam.Parser
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: lambdaCase
-- 🔀 Step 2:  \case | Nothing -> 0 | Just x -> x
--    is  \$arg -> case $arg of | Nothing -> 0 | Just x -> x
--    (Haskell's LambdaCase). The branches are read by branchesOf,
--    the same as after case ... of, so guards work too.
-- ステップ2：\case は、引数を case で分けるラムダ（Haskell の LambdaCase）。
-- 枝は case … of のあとと同じ branchesOf で読むので、ガードも使える
lambdaCase :: Parser Expr
lambdaCase = do
  _ <- symbol "\\"
  keyword "case"
  Lam "$arg" <$> branchesOf (Var "$arg")
-- ANCHOR_END: lambdaCase

-- ANCHOR: canFail
-- ⚠️ Step 3: can a value of the right type still fail to fit the
--    pattern?  (a, b) cannot fail; Just x and [] can. A constructor
--    fails only when its type has other constructors.
-- ステップ3：型が合っていても、値がパターンに合わないことがあるか。
-- 構成子は、その型にほかの構成子があるときだけ外れる
canFail :: DataTable -> Pattern -> Bool
canFail table p = case p of
  PVar _    -> False
  PWild     -> False
  PLit _    -> True
  PList _   -> True
  PCons _ _ -> True
  PTuple ps -> any (canFail table) ps
  PCon c ps -> others c || any (canFail table) ps
  where
    others c = case Map.lookup c (constructors table) of
      Nothing  -> True
      Just con -> length (siblings (conTypeName con)) > 1
    siblings t =
      [ c | (c, con) <- Map.toList (constructors table)
          , conTypeName con == t ]

-- ⚠️ The patterns of cases with only one branch that can fail. A
--    lambda or a let with a pattern opens into such a case, so this
--    finds  \(Just x) -> x  and  let [a] = xs in a.
-- 枝が1つで、外れうるパターンの case。パターンのラムダや let は、
-- そういう case に開くので、\(Just x) -> x や let [a] = xs in a が見つかる
riskyPatterns :: DataTable -> Expr -> [Pattern]
riskyPatterns table e = here <> concatMap (riskyPatterns table) (inside e)
  where
    here = case e of
      Case _ [(p, _)] | canFail table p -> [p]
      _                                 -> []

-- 🌳 The expressions just inside an expression.
-- 式のすぐ内側の式
inside :: Expr -> [Expr]
inside e = case e of
  Lit _          -> []
  Var _          -> []
  Lam _ body     -> [body]
  App f a        -> [f, a]
  Let _ a body   -> [a, body]
  If c a b       -> [c, a, b]
  BinOp _ l r    -> [l, r]
  Tuple es       -> es
  List es        -> es
  Case s bs      -> s : map snd bs
-- ANCHOR_END: canFail
-- ANCHOR_END: all
