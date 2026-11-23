{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answers of the training of "Turning Lam into JavaScript".
-- 「JavaScript に変換する」のトレーニングの解答。
module Lam.Answers
  ( moreJsPrims
  , moreJsNatives
  , foldConstants
  ) where

import Data.Text (Text)

import Lam.Core (Core (..))
import Lam.Syntax (Literal (..), Op (..))
import Lam.ToJs (jsNatives, jsPrims)

-- ANCHOR: more
-- 🧰 Steps 1 and 2: the names the runtime has with the answers of
--    answers/runtime/answers.mjs: Time.toDate, and groupBy as a loop.
-- ステップ1と2：answers/runtime/answers.mjs の解答を足したときに、
-- ランタイムが持つ名前。Time.toDate と、ループで書いた groupBy
moreJsPrims :: [Text]
moreJsPrims = jsPrims <> ["Time.toDate"]

moreJsNatives :: [Text]
moreJsNatives = jsNatives <> ["Prelude.groupBy"]
-- ANCHOR_END: more

-- ANCHOR: foldConstants
-- ✂️ Step 3: compute what can be computed already, while turning:
--    2 * 3 + 1 becomes 7, and if true then a else b becomes a. Only
--    Ints and Bools: a Float could differ by the last digit, and a
--    division by 0 must stay, to stop when the program runs.
-- ステップ3：変換のあいだに、もう計算できるものを計算しておく。
-- 2 * 3 + 1 は 7 に、if true then a else b は a に。Int と Bool だけ：
-- Float は最後の桁がずれうるし、0 で割るのは、プログラムを動かしたときに
-- 止まるように残しておく
foldConstants :: Core -> Core
foldConstants core = case core of
  COp o l r -> folded o (foldConstants l) (foldConstants r)
  CIf c a b -> case foldConstants c of
    CLit (LBool True)  -> foldConstants a
    CLit (LBool False) -> foldConstants b
    c'                 -> CIf c' (foldConstants a) (foldConstants b)
  CLam x b      -> CLam x (foldConstants b)
  CApp f a      -> CApp (foldConstants f) (foldConstants a)
  CLet x e b    -> CLet x (foldConstants e) (foldConstants b)
  CTuple es     -> CTuple (map foldConstants es)
  CList es      -> CList (map foldConstants es)
  CCase e alts  -> CCase (foldConstants e)
                     [ (p, foldConstants b) | (p, b) <- alts ]
  CRecord fs    -> CRecord [ (n, foldConstants v) | (n, v) <- fs ]
  CField e n    -> CField (foldConstants e) n
  CUpdate e fs  -> CUpdate (foldConstants e)
                     [ (n, foldConstants v) | (n, v) <- fs ]
  _             -> core

-- 🧮 One operator on two sides already folded.
-- 畳み終えた両側に、演算子を1つ
folded :: Op -> Core -> Core -> Core
folded o l r = case (o, l, r) of
  (Add, CLit (LInt a), CLit (LInt b)) -> CLit (LInt (a + b))
  (Sub, CLit (LInt a), CLit (LInt b)) -> CLit (LInt (a - b))
  (Mul, CLit (LInt a), CLit (LInt b)) -> CLit (LInt (a * b))
  (IntDiv, CLit (LInt a), CLit (LInt b))
    | b /= 0 -> CLit (LInt (a `div` b))
  (Lt, CLit (LInt a), CLit (LInt b)) -> CLit (LBool (a < b))
  (Eq, CLit (LInt a), CLit (LInt b)) -> CLit (LBool (a == b))
  (And, CLit (LBool a), CLit (LBool b)) -> CLit (LBool (a && b))
  (Or, CLit (LBool a), CLit (LBool b)) -> CLit (LBool (a || b))
  _ -> COp o l r
-- ANCHOR_END: foldConstants
-- ANCHOR_END: all
