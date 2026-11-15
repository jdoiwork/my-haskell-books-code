-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the parser chapter.
-- パーサーの章のトレーニングの解答。
module Lam.Answers
  ( negative
  , lambdaMany
  , letWithArgs
  ) where

import Text.Megaparsec (some, (<|>))

import Lam.Parser
import Lam.Syntax
-- ANCHOR_END: head

-- ANCHOR: negative
-- ➖ -5 means 0 - 5 (Lam has no negative numbers in the tree).
-- -5 は 0 - 5 のこと（Lam の木には、マイナスの数がない）
negative :: Parser Expr
negative = do
  _ <- symbol "-"
  a <- atom
  pure (BinOp Sub (Lit (LInt 0)) a)
-- ANCHOR_END: negative

-- ANCHOR: lambdaMany
-- 🍬 \x y -> body means \x -> \y -> body.
-- \x y -> body は \x -> \y -> body のこと
lambdaMany :: Parser Expr
lambdaMany = do
  _    <- symbol "\\"
  xs   <- some name
  _    <- symbol "->"
  body <- expr
  pure (foldr Lam body xs)
-- ANCHOR_END: lambdaMany

-- ANCHOR: letWithArgs
-- 🍬 let f x y = body in rest  means  let f = \x -> \y -> body in rest.
-- let f x y = body in rest は let f = \x -> \y -> body in rest のこと
letWithArgs :: Parser Expr
letWithArgs = do
  keyword "let"
  f    <- name
  xs   <- many' name
  _    <- symbol "="
  body <- expr
  keyword "in"
  rest <- expr
  pure (Let f (foldr Lam body xs) rest)
  where
    many' p = some p <|> pure []
-- ANCHOR_END: letWithArgs
-- ANCHOR_END: all
