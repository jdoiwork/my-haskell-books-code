{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Set as Set
import Test.Hspec

import Lam.Answers
import Lam.Examples
import Lam.Pretty
import Lam.Syntax

main :: IO ()
main = hspec $ do
  -- 「ソースコードに戻す」
  describe "render" $ do
    -- 「例のプログラム」
    it "renders the examples" $
      map render [double21, factorial10, adder]
        `shouldBe`
          [ "let double = \\x -> x * 2 in double 21"
          , "let fact = \\n -> if n == 0 then 1 else n * fact (n - 1)"
              <> " in fact 10"
          , "\\x -> \\y -> x + y" ]

    -- ANCHOR: parens
    -- 「要る括弧だけを付ける」
    it "adds only the parentheses that are needed" $
      map render
        [ BinOp Mul (BinOp Add (Var "a") (Var "b")) (Var "c")
        , BinOp Add (Var "a") (BinOp Mul (Var "b") (Var "c"))
        , BinOp Sub (Var "a") (BinOp Sub (Var "b") (Var "c"))
        , App (Var "f") (App (Var "g") (Var "x"))
        , App (Lam "x" (Var "x")) (Lit (LInt 1)) ]
        `shouldBe`
          [ "(a + b) * c", "a + b * c", "a - (b - c)"
          , "f (g x)", "(\\x -> x) 1" ]
    -- ANCHOR_END: parens

  -- 「外から来る変数」
  describe "freeVars" $
    -- ANCHOR: freevars
    -- 「外から来る変数を見つける」
    it "finds the variables from outside" $
      map freeVars
        [adder, Lam "x" (BinOp Add (Var "x") (Var "y")), factorial10]
        `shouldBe` [Set.empty, Set.fromList ["y"], Set.empty]
    -- ANCHOR_END: freevars

  -- 「トレーニング」
  describe "answers" $ do
    it "size counts the nodes" $
      map size [double21, adder] `shouldBe` [8, 5]
    it "constants lists the integers" $
      constants factorial10 `shouldBe` [0, 1, 1, 10]
    it "twice renders" $
      render twice
        `shouldBe` ( "let twice = \\f -> \\x -> f (f x)"
                       <> " in twice (\\n -> n + 3) 10" )
