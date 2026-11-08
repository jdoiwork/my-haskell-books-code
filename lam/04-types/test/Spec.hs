-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Lam.Eval
import Lam.Parser
import Lam.Repl
import Lam.Syntax
import Lam.Types

-- 🔍 Parse and show the type (or the type error).
-- 読んで、型を見せる（または型のエラー）
typeOf :: Text -> Text
typeOf source = case parseExpr source of
  Left _  -> "parse error"
  Right e -> either renderTypeError renderType (inferType Map.empty e)
-- ANCHOR_END: head

-- ANCHOR: gen
-- 🎲 Random expressions over x, y : Int and f : Int -> Int.
-- x, y : Int と f : Int -> Int を使う、でたらめな式
genExpr :: Gen Expr
genExpr = sized go
  where
    go 0 = leaf
    go n =
      let sub = go (n `div` 2)
      in frequency
           [ (3, leaf)
           , (1, Lam <$> elements ["x", "y"] <*> sub)
           , (2, App <$> sub <*> sub)
           , (1, Let <$> elements ["x", "y", "g"] <*> sub <*> sub)
           -- The condition is often true/false or a comparison,
           -- so that the program has a type more often.
           -- 条件は、true・false や比べる式が多くなるようにする
           -- （型の付くプログラムが、多くできるように）
           , (2, If <$> condition sub <*> sub <*> sub)
           , (2, BinOp <$> arbitraryBoundedEnum <*> sub <*> sub) ]
    leaf = oneof
      [ Lit . LInt <$> chooseInteger (0, 9)
      , Lit . LBool <$> arbitrary
      , Var <$> elements ["x", "y", "f"] ]
    condition sub = oneof
      [ Lit . LBool <$> arbitrary
      , BinOp <$> elements [Eq, Lt] <*> sub <*> sub
      , sub ]

-- 🌍 The world those names live in: their types and their values.
-- 名前たちの住む世界：型と値
startTypes :: TypeEnv
startTypes = Map.fromList
  [ ("x", Forall [] TInt), ("y", Forall [] TInt)
  , ("f", Forall [] (TFun TInt TInt)) ]

startValues :: Env
startValues = Map.fromList
  [ ("x", VInt 3), ("y", VInt 4)
  , ("f", VClosure "n" (BinOp Add (Var "n") (Lit (LInt 1))) Map.empty) ]

-- 🔁 No let that uses its own name (those may never finish).
-- 自分の名前を使う let はなし（終わらないかもしれないから）
noSelfLet :: Expr -> Bool
noSelfLet e = case e of
  Let x d b   -> not (x `Set.member` freeVars d) && noSelfLet d && noSelfLet b
  Lam _ b     -> noSelfLet b
  App a b     -> noSelfLet a && noSelfLet b
  If c a b    -> noSelfLet c && noSelfLet a && noSelfLet b
  BinOp _ a b -> noSelfLet a && noSelfLet b
  _           -> True
-- ANCHOR_END: gen

-- ANCHOR: fits
-- 🧩 Does the value have the shape the type promised?
-- 値は、型が約束した形をしている？
fits :: Value -> Type -> Bool
fits v t = case (v, t) of
  (VInt _, TInt)             -> True
  (VBool _, TBool)           -> True
  (VClosure {}, TFun _ _)    -> True
  _                          -> False
-- ANCHOR_END: fits

main :: IO ()
main = hspec $ do
  -- 「型を当てる」
  describe "inferType" $ do
    -- ANCHOR: examples
    -- 「型を1つも書かずに、型を当てる」
    it "finds types without any type written" $
      map typeOf
        [ "\\x -> x * 2"
        , "\\x -> \\y -> x + y"
        , "\\x -> x"
        , "\\f -> \\x -> f (f x)"
        , "\\f -> \\g -> \\x -> f (g x)"
        , "let fact = \\n -> if n == 0 then 1 else n * fact (n - 1) in fact" ]
        `shouldBe`
          [ "Int -> Int", "Int -> Int -> Int", "a -> a"
          , "(a -> a) -> a -> a", "(a -> b) -> (c -> a) -> c -> b"
          , "Int -> Int" ]

    -- 「let で決めた名前は、いろいろな型で使える」
    it "lets a let-defined name be used at many types" $
      typeOf "let id = \\x -> x in if id true then id 1 else 2"
        `shouldBe` "Int"

    -- 「まちがいを、動かす前に見つける」
    it "finds mistakes before running" $
      map typeOf
        ["1 + true", "3 4", "\\x -> x x", "nope", "if 1 then 2 else 3"]
        `shouldBe`
          [ "the types do not fit: Bool where Int is needed"
          , "the types do not fit: Int where Int -> a is needed"
          , "an infinite type: a = a -> b (is a function applied to itself?)"
          , "unknown name: nope"
          , "the types do not fit: Int where Bool is needed" ]
    -- ANCHOR_END: examples

  -- ANCHOR: property
  -- 「型の付いたプログラムは、まちがった動きをしない」
  describe "well-typed programs" $
    it "cannot go wrong" $
      withNumTests 300 $ forAll genExpr $ \e ->
        noSelfLet e ==>
          case inferType startTypes e of
            Left _  -> property Discard
            Right t -> case eval startValues e of
              Right v           -> property (fits v t)
              Left DivideByZero -> property True
              Left err          -> counterexample (show err) False
  -- ANCHOR_END: property

  -- 「REPL」
  describe "step" $
    it "checks types before computing" $ do
      let inputs =
            [ "let double = \\x -> x * 2", "double 21", "double true"
            , "let id = \\x -> x", ":type id"
            -- id is already in the REPL: the inner id must still be
            -- generalized (a bug the author once had).
            -- REPL にはもう id がある。内側の id も一般化されるはず
            , ":type let id = \\x -> x in id id" ]
          go (s, outs) line =
            let (s', out) = step s line in (s', outs <> [out])
      snd (foldl go (emptySession, []) inputs)
        `shouldBe`
          [ "double : Int -> Int", "42 : Int"
          , "type error: the types do not fit: Bool where Int is needed"
          , "id : a -> a", "a -> a", "a -> a" ]
