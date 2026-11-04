{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Map as Map
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck

import Lam.Answers
import Lam.Eval
import Lam.Examples
import Lam.Parser
import Lam.Repl
import Lam.Syntax

-- 🧮 Parse, evaluate, and show (or show the error).
-- 読んで、計算して、見せる（またはエラーを見せる）
run :: Text -> Text
run source = case parseExpr source of
  Left _  -> "parse error"
  Right e -> either renderError renderValue (eval Map.empty e)

-- ANCHOR: arith
-- 🎲 Random arithmetic: numbers with + - * only.
-- でたらめな計算式：数と + - * だけ
genArith :: Gen Expr
genArith = sized go
  where
    go 0 = Lit . LInt <$> chooseInteger (0, 20)
    go n = oneof
      [ Lit . LInt <$> chooseInteger (0, 20)
      , BinOp <$> elements [Add, Sub, Mul] <*> go (n `div` 2)
                                           <*> go (n `div` 2) ]

-- 📏 The same arithmetic, done directly in Haskell (the reference).
-- 同じ計算を、Haskell で直接する（お手本）
reference :: Expr -> Integer
reference e = case e of
  Lit (LInt n)   -> n
  BinOp Add l r  -> reference l + reference r
  BinOp Sub l r  -> reference l - reference r
  BinOp Mul l r  -> reference l * reference r
  _              -> 0
-- ANCHOR_END: arith

main :: IO ()
main = hspec $ do
  -- 「計算する」
  describe "eval" $ do
    -- ANCHOR: examples
    -- 「例のプログラム」
    it "runs the examples" $
      map (either renderError renderValue . eval Map.empty)
        [double21, factorial10]
        `shouldBe` ["42", "3628800"]

    -- 「関数を返す関数と、関数を受け取る関数」
    it "handles functions that make and take functions" $
      map run
        [ "let add = \\x -> \\y -> x + y in let inc = add 1 in inc 41"
        , "let twice = \\f -> \\x -> f (f x) in twice (\\n -> n + 3) 10" ]
        `shouldBe` ["42", "16"]

    -- 「関数は、作られた場所の変数を覚えている」
    it "remembers the variables where a function was made" $
      run "let x = 1 in let f = \\y -> x in let x = 2 in f 0"
        `shouldBe` "1"
    -- ANCHOR_END: examples

    -- 「まちがいを、エラーにする」
    it "turns mistakes into errors" $
      map run ["nope + 1", "1 + true", "10 / 0", "if 1 then 2 else 3", "3 4"]
        `shouldBe`
          [ "unknown variable: nope"
          , "+ needs numbers, but got true"
          , "divided by zero"
          , "if needs true or false, but got 1"
          , "not a function, cannot call: 3" ]

    -- ANCHOR: property
    -- 「計算式は、Haskell と同じ答えになる」
    it "computes arithmetic like Haskell does" $
      property $ forAll genArith $ \e ->
        fmap renderValue (eval Map.empty e)
          === Right (renderValue (VInt (reference e)))
    -- ANCHOR_END: property

  -- ANCHOR: repl
  -- 「REPL」
  describe "step" $
    -- 「定義を覚えて、次の行で使える」
    it "remembers definitions for the next lines" $ do
      let inputs =
            [ "let double = \\x -> x * 2"
            , "double 21"
            , "let fact = \\n -> if n == 0 then 1 else n * fact (n - 1)"
            , "fact 5"
            , "missing" ]
          go (env, outs) line =
            let (env', out) = step env line in (env', outs <> [out])
      snd (foldl go (Map.empty, []) inputs)
        `shouldBe`
          [ "double = <function of x>", "42"
          , "fact = <function of n>", "120"
          , "error: unknown variable: missing" ]
  -- ANCHOR_END: repl

  -- 「トレーニング」
  describe "answers" $ do
    it "dynamic scope sees the caller's x" $
      fmap (fmap renderValue . evalDynamic Map.empty)
        (parseExpr "let x = 1 in let f = \\y -> x in let x = 2 in f 0")
        `shouldBe` Right (Right "2")
    it "showEnv shows names and values" $
      showEnv (fst (step Map.empty "let answer = 6 * 7"))
        `shouldBe` "answer = 42"
