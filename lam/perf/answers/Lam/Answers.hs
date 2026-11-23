{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answers of the training of "Making Lam fast".
-- 「Lam を速くする」のトレーニングの解答。
module Lam.Answers
  ( takeFast
  , foldConstants
  ) where

import Lam.Eval
import Lam.Syntax

-- ANCHOR: take
-- ✂️ Step 1: List.take n xs, in Haskell. The Lam definition calls no
--    function it was given, so there is no order to keep: only the
--    answer. n below 1 is [], and n longer than the list is the list.
--    Lam:  take n xs = if n < 1 then [] else
--                        case xs of
--                          | [] -> []
--                          | x : rest -> x : take (n - 1) rest
-- ステップ1：List.take の Haskell の版。Lam の定義は、受け取った関数を
-- 呼ばないので、守る順番はない。守るのは答えだけ。n が 1 より小さければ
-- []、リストより長ければリスト全体
takeFast :: [Value] -> Either EvalError Value
takeFast [VInt n, VList xs]
  | n < 1     = Right (VList [])
  | otherwise = Right (VList (take (fromInteger (min n big)) xs))
  where
    -- More than any list can hold: Haskell's take wants an Int.
    -- どんなリストより長い。Haskell の take は Int を受け取る
    big = toInteger (maxBound :: Int)
takeFast _ = Left (BuiltinFailed "take" "a number and a list")
-- ANCHOR_END: take

-- ANCHOR: foldConstants
-- 🧮 Step 2: compute the sums of numbers written in the program once,
--    before it runs:  1 + 2 * 3  becomes  7. A division by zero stays:
--    it must stop the program when (and only when) it is reached.
-- ステップ2：プログラムに書いた数の計算を、動かす前に1度だけしておく。
-- 1 + 2 * 3 は 7 になる。0 で割るものは、そのまま残す。そこに来たとき
-- （来たときだけ）プログラムを止めないといけないから
foldConstants :: Expr -> Expr
foldConstants expr = case expr of
  BinOp op l r ->
    let l' = foldConstants l
        r' = foldConstants r
    in case (number l', number r') of
         (Just a, Just b) | Just v <- folded op a b -> Lit (LInt v)
         _ -> BinOp op l' r'
  Lit lit        -> Lit lit
  Var x          -> Var x
  Lam x body     -> Lam x (go body)
  App f a        -> App (go f) (go a)
  Let x e body   -> Let x (go e) (go body)
  If c t e       -> If (go c) (go t) (go e)
  Tuple es       -> Tuple (map go es)
  List es        -> List (map go es)
  Case e alts    -> Case (go e) [ (p, go b) | (p, b) <- alts ]
  Record fs      -> Record [ (x, go e) | (x, e) <- fs ]
  Field e x      -> Field (go e) x
  Update e fs    -> Update (go e) [ (x, go f) | (x, f) <- fs ]
  At loc e       -> At loc (go e)
  Annot sig e    -> Annot sig (go e)
  where
    go = foldConstants
    -- A whole number written as it is (the mark of its place peeled).
    -- そのまま書いた整数（場所の印ははがす）
    number e = case e of
      Lit (LInt n) -> Just n
      At _ inner   -> number inner
      _            -> Nothing
    -- The same answers as binOp; // by zero is left alone.
    -- binOp と同じ答え。0 で割る // は、そのまま
    folded op a b = case op of
      Add                -> Just (a + b)
      Sub                -> Just (a - b)
      Mul                -> Just (a * b)
      IntDiv | b /= 0    -> Just (a `div` b)
      _                  -> Nothing
-- ANCHOR_END: foldConstants
-- ANCHOR_END: all
