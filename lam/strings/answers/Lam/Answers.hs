-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the chapter on strings, tuples and lists.
-- 文字列・タプル・リストの章のトレーニングの解答。
module Lam.Answers
  ( moreBuiltins
  , withBuiltins
  , listOrRange
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Megaparsec (sepBy, try, (<|>))

import Lam.Builtins
import Lam.Eval
import Lam.Parser
import Lam.Program
import Lam.Syntax
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: words
-- ✂️ Step 1: words and lines, as new rows of the table.
-- ステップ1：words と lines を、表の新しい行にする
moreBuiltins :: [Builtin]
moreBuiltins =
  [ prim1 "words" (tString ~> tList tString) (splitWith T.words)
  , prim1 "lines" (tString ~> tList tString) (splitWith T.lines)
  ]
  where
    splitWith f v = do
      s <- string v
      Right (VList (map VString (f s)))

-- 🧰 Put more builtins into a session.
-- セッションに、組み込みの関数を足す
withBuiltins :: [Builtin] -> Session -> Session
withBuiltins bs session = session
  { sessionTypes  = foldr addType (sessionTypes session) bs
  , sessionValues = foldr addValue (sessionValues session) bs
  }
  where
    addType b  = Map.insert (builtinName b) (builtinType b)
    addValue b = Map.insert (builtinName b) (builtinValue b)
-- ANCHOR_END: words

-- ANCHOR: range
-- 🔢 Step 2: [1 .. 5] means range 1 5. Otherwise, a list as before.
-- ステップ2：[1 .. 5] は range 1 5 のこと。それ以外は、これまでのリスト
listOrRange :: Parser Expr
listOrRange = do
  _ <- symbol "["
  e <- try range <|> items
  _ <- symbol "]"
  pure e
  where
    range = do
      from <- expr
      _    <- symbol ".."
      to   <- expr
      pure (App (App (Var "range") from) to)
    items = List <$> expr `sepBy` symbol ","
-- ANCHOR_END: range
-- ANCHOR_END: all
