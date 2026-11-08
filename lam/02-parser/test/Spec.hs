-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Either (isLeft)
import Text.Megaparsec (eof, parse)
import Test.Hspec
import Test.QuickCheck

import Lam.Answers
import Lam.Examples
import Lam.Parser
import Lam.Pretty
import Lam.Syntax
import PrettyDraft (renderDraft)
-- ANCHOR_END: head

-- ANCHOR: gen
-- 🎲 A random Lam expression. `sized` keeps it from growing forever.
-- でたらめな Lam の式。sized で、大きくなりすぎないようにする
genExpr :: Gen Expr
genExpr = sized go
  where
    go 0 = leaf
    go n =
      let sub = go (n `div` 2)
      in frequency
           [ (2, leaf)
           , (1, Lam <$> genName <*> sub)
           , (2, App <$> sub <*> sub)
           , (1, Let <$> genName <*> sub <*> sub)
           , (1, If <$> sub <*> sub <*> sub)
           , (2, BinOp <$> arbitraryBoundedEnum <*> sub <*> sub) ]
    leaf = oneof
      [ Lit . LInt <$> chooseInteger (0, 100)
      , Lit . LBool <$> arbitrary
      , Var <$> genName ]

genName :: Gen Name
genName = elements ["x", "y", "f", "acc", "n'"]

-- ✂️ To shrink a failing expression: try its children, then the same
--    shape with one child shrunk. A leaf shrinks to 0.
-- 失敗した式を縮める：まず子どもを試し、次に、子どもを1つだけ縮めた
-- 同じ形を試す。葉は 0 に縮める
shrinkExpr :: Expr -> [Expr]
shrinkExpr ex = case ex of
  Lit (LInt 0) -> []
  Lit _        -> [zero]
  Var _        -> [zero]
  Lam x b      -> b : [Lam x b' | b' <- shrinkExpr b]
  App f a      -> [f, a] <> [App f' a | f' <- shrinkExpr f]
                        <> [App f a' | a' <- shrinkExpr a]
  Let x e b    -> [e, b] <> [Let x e' b | e' <- shrinkExpr e]
                          <> [Let x e b' | b' <- shrinkExpr b]
  If c a b     -> [c, a, b] <> [If c' a b | c' <- shrinkExpr c]
                            <> [If c a' b | a' <- shrinkExpr a]
                            <> [If c a b' | b' <- shrinkExpr b]
  BinOp o l r  -> [l, r] <> [BinOp o l' r | l' <- shrinkExpr l]
                         <> [BinOp o l r' | r' <- shrinkExpr r]
  where
    zero = Lit (LInt 0)
-- ANCHOR_END: gen

main :: IO ()
main = hspec $ do
  -- 「ソースコードを読む」
  describe "parseExpr" $ do
    -- ANCHOR: examples
    -- 「例のプログラムを、手で組んだ木と同じに読む」
    it "reads the examples into the same trees" $
      map parseExpr
        [ "let double = \\x -> x * 2 in double 21"
        , "let fact = \\n -> if n == 0 then 1 else n * fact (n - 1)\n\
          \in fact 10" ]
        `shouldBe` [Right double21, Right factorial10]

    -- 「強さと向きのとおりに組み立てる」
    it "follows the levels and the directions" $
      map parseExpr ["1 + 2 * 3", "1 - 2 - 3", "f x y", "f x + 1"]
        `shouldBe` map Right
          [ BinOp Add (Lit (LInt 1)) (BinOp Mul (Lit (LInt 2)) (Lit (LInt 3)))
          , BinOp Sub (BinOp Sub (Lit (LInt 1)) (Lit (LInt 2))) (Lit (LInt 3))
          , App (App (Var "f") (Var "x")) (Var "y")
          , BinOp Add (App (Var "f") (Var "x")) (Lit (LInt 1)) ]
    -- ANCHOR_END: examples

    -- 「まちがったプログラムは断る」
    it "rejects broken programs" $
      map (isLeft . parseExpr)
        ["let x = in x", "1 == 2 == 3", "if x then 1", "(1 + 2", "\\let -> 1"]
        `shouldBe` replicate 5 True

    -- 「コメントと改行は飛ばす」
    it "skips comments and line breaks" $
      parseExpr "-- the answer\n  6 * 7 -- is here"
        `shouldBe` Right (BinOp Mul (Lit (LInt 6)) (Lit (LInt 7)))

  -- ANCHOR: roundtrip
  -- 「表示して読み直すと、元の木に戻る」
  describe "render and parse" $
    it "parsing the rendered code gives the same tree" $
      property $ forAllShrink genExpr shrinkExpr $ \e ->
        parseExpr (render e) === Right e
  -- ANCHOR_END: roundtrip

  -- ANCHOR: draft
  -- 「下書きの render のバグを、QuickCheck が見つける」
  describe "renderDraft" $
    it "has a bug that QuickCheck finds" $
      expectFailure $ forAllShrink genExpr shrinkExpr $ \e ->
        parseExpr (renderDraft e) === Right e
  -- ANCHOR_END: draft

  -- 「トレーニング」
  describe "answers" $ do
    let run p = parse (p <* eof) ""
    it "negative reads -5 as 0 - 5" $
      run negative "-5"
        `shouldBe` Right (BinOp Sub (Lit (LInt 0)) (Lit (LInt 5)))
    it "lambdaMany reads several arguments" $
      run lambdaMany "\\x y -> x + y" `shouldBe` Right adder
    it "letWithArgs reads arguments after the name" $
      fmap render (run letWithArgs "let add x y = x + y in add 1 2")
        `shouldBe` Right "let add = \\x -> \\y -> x + y in add 1 2"
