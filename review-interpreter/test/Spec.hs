-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Lam.Eval
import Lam.Parser
import Lam.Pretty
import Lam.Syntax
import Lam.Types
import Review.Optimize
-- ANCHOR_END: head

-- ANCHOR: gen
-- 🎲 Random closed expressions (no free names), small numbers.
-- でたらめな、閉じた式（外の名前を使わない）。数は小さく
genClosed :: Gen Expr
genClosed = sized (go [])
  where
    go scope 0 = leaf scope
    go scope n =
      let sub = go scope (n `div` 2)
      in frequency
           [ (3, leaf scope)
           , (1, do x <- elements ["x", "y"]
                    Lam x <$> go (x : scope) (n `div` 2))
           , (1, App <$> sub <*> sub)
           , (1, do x <- elements ["x", "y"]
                    Let x <$> sub <*> go (x : scope) (n `div` 2))
           , (2, If <$> sub <*> sub <*> sub)
           -- * and / come more often: that is where the traps are.
           -- * と / を多めに：落とし穴があるのは、そこだから
           , (3, BinOp <$> elements [Add, Sub, Mul, Mul, Div, Div, Eq, Lt]
                       <*> sub <*> sub) ]
    leaf scope = oneof $
      [ Lit . LInt <$> chooseInteger (0, 2)
      , Lit . LBool <$> arbitrary ]
      <> [ Var <$> elements scope | not (null scope) ]

-- 🔁 No let that uses its own name (it may never finish:
--    Lam's let is recursive, so  let x = x + 1  means itself).
-- 自分の名前を使う let はなし（終わらないかもしれない。Lam の let は
-- 再帰するので、let x = x + 1 の x は自分自身）
noSelfLet :: Expr -> Bool
noSelfLet e = case e of
  Let x d b   -> not (x `Set.member` freeVars d) && noSelfLet d && noSelfLet b
  Lam _ b     -> noSelfLet b
  App a b     -> noSelfLet a && noSelfLet b
  If c a b    -> noSelfLet c && noSelfLet a && noSelfLet b
  BinOp _ a b -> noSelfLet a && noSelfLet b
  _           -> True

-- 🎯 Closed, terminating, and well-typed: the programs we compare.
-- 閉じていて、終わり、型の付くもの：比べるプログラム
genProgram :: Gen Expr
genProgram =
  genClosed `suchThat` \e -> noSelfLet e && isRight (inferType Map.empty e)

-- 🧮 The result as text (value or error).
-- 結果を文字で（値か、エラー）
run :: Expr -> Text
run e = either renderError renderValue (eval Map.empty e)

-- 🏷️ The type as text (or the type error).
-- 型を文字で（または型のエラー）
typeOf :: Expr -> Text
typeOf e = either renderTypeError renderType (inferType Map.empty e)
-- ANCHOR_END: gen

-- ANCHOR: naive
-- ✅ The answer does not change.
-- 答えは変わらない
propSameAnswer :: Property
propSameAnswer =
  forAll genProgram $ \e ->
    run (optimize e) === run e

-- 🏷️ The type does not change.
-- 型は変わらない
propSameType :: Property
propSameType =
  forAll genProgram $ \e ->
    typeOf (optimize e) === typeOf e
-- ANCHOR_END: naive

-- ANCHOR: general
-- 🔍 Is  specific  one of the types that  general  stands for?
--    (Fill the type variables of  general  to get  specific.)
--    Bool -> Bool  is one of  a -> a ;  Int -> Bool  is not.
-- specific は、general が表す型の1つ？（general の型変数を埋めると
-- specific になる？）Bool -> Bool は a -> a の1つ。Int -> Bool は違う
isInstanceOf :: Type -> Type -> Bool
isInstanceOf specific general = go Map.empty [(general, specific)]
  where
    go _ [] = True
    go seen ((g, s) : rest) = case (g, s) of
      (TVar v, _) -> case Map.lookup v seen of
        Nothing -> go (Map.insert v s seen) rest
        Just t  -> t == s && go seen rest
      (TInt, TInt)             -> go seen rest
      (TBool, TBool)           -> go seen rest
      (TFun g1 g2, TFun s1 s2) -> go seen ((g1, s1) : (g2, s2) : rest)
      _                        -> False

-- 🏷️ The type after optimizing is the same, or more general.
-- 最適化した後の型は、同じか、もっと一般的
sameOrMoreGeneral :: Expr -> Expr -> Bool
sameOrMoreGeneral before after =
  case (inferType Map.empty before, inferType Map.empty after) of
    (Right t, Right t') -> t `isInstanceOf` t'
    _                   -> False
-- ANCHOR_END: general

-- ANCHOR: parse
-- 🧾 Read source code; a parse error stops the test.
-- ソースコードを読む。読めなければ、テストを止める
parseOrDie :: Text -> Expr
parseOrDie src = either (error . show) id (parseExpr src)
-- ANCHOR_END: parse

main :: IO ()
main = hspec $ do
  -- 「書き換えの例」
  describe "optimize" $ do
    it "folds constants and drops dead branches" $
      map (render . optimize . parseOrDie)
        [ "1 + 2 * 3"
        , "if 1 < 2 then 10 else 20"
        , "\\x -> x * (3 - 2) + 0"
        , "1 / 0" ]
        `shouldBe` ["7", "10", "\\x -> x", "1 / 0"]

    -- 「最適化しても、答えは変わらない」
    it "keeps the same answer" propSameAnswer

    -- 「型がいつもまったく同じとは限らない（propSameType は落ちる）」
    it "does NOT always keep exactly the same type" $
      expectFailure (withMaxSuccess 5000 propSameType)

    -- ANCHOR: props
    -- 「最適化しても、型は同じか、もっと一般的になるだけ」
    it "keeps the type, or makes it more general" $
      property $ forAll genProgram $ \e ->
        counterexample (show (typeOf e, typeOf (optimize e))) $
          sameOrMoreGeneral e (optimize e)
    -- ANCHOR_END: props

    -- ANCHOR: generalex
    -- 「型がもっと一般的になる例」
    it "can make the type more general" $
      let before src = typeOf (parseOrDie src)
          after  src = typeOf (optimize (parseOrDie src))
      in map (\src -> (before src, after src))
        [ "\\x -> x * 1"
        , "\\x -> if false then false else x" ]
        `shouldBe`
        [ ("Int -> Int"  , "a -> a")
        , ("Bool -> Bool", "a -> a") ]
    -- ANCHOR_END: generalex

    -- ANCHOR: draftprop
    -- 「x * 0 -> 0 の下書きは、0 で割るまちがいを消してしまう」
    it "the draft with x * 0 -> 0 hides a division by zero" $ do
      let e = parseOrDie "(1 / 0) * 0"
      (run e, run (optimize e), run (optimizeDraft e))
        `shouldBe` ("divided by zero", "divided by zero", "0")
    -- ANCHOR_END: draftprop
