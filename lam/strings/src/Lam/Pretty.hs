{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Turn an AST back into Lam source code, with as few
--   parentheses as possible.
-- AST を、Lam のソースコードに戻す。括弧はできるだけ少なく。
module Lam.Pretty
  ( render
  , renderString
  , opSymbol
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Syntax

-- ANCHOR: levels
-- 🪜 How tightly each kind of expression holds together.
--    A bigger number holds tighter: 2 * 3 + 4 is (2 * 3) + 4.
-- 式の種類ごとの、結びつきの強さ。大きいほど強く結びつく
level :: Expr -> Int
level expr = case expr of
  Lam {}        -> 0   -- \x -> ...     （右の端まで続く）
  Let {}        -> 0   -- let ... in ...
  If {}         -> 0   -- if ... then ... else ...
  BinOp op _ _  -> opLevel op
  App {}        -> 7   -- f x
  Lit _         -> 8   -- 42
  Var _         -> 8   -- x
  Tuple _       -> 8   -- (1, 2)    has its own parentheses / 自前の括弧がある
  List _        -> 8   -- [1, 2]

-- The same order as Haskell: || < && < == < : ++ < + - < * /
-- Haskell と同じ順番
opLevel :: Op -> Int
opLevel op = case op of
  Or     -> 1
  And    -> 2
  Eq     -> 3
  Ne     -> 3
  Lt     -> 3
  Le     -> 3
  Gt     -> 3
  Ge     -> 3
  Concat -> 4
  Cons   -> 4
  Add    -> 5
  Sub    -> 5
  Mul    -> 6
  Div    -> 6

opSymbol :: Op -> Text
opSymbol op = case op of
  Add    -> "+"
  Sub    -> "-"
  Mul    -> "*"
  Div    -> "/"
  Eq     -> "=="
  Ne     -> "/="
  Lt     -> "<"
  Le     -> "<="
  Gt     -> ">"
  Ge     -> ">="
  Concat -> "++"
  Cons   -> ":"
  And    -> "&&"
  Or     -> "||"
-- ANCHOR_END: levels

-- ANCHOR: render
-- 🖨️ The source code of an expression.
-- 式のソースコード
render :: Expr -> Text
render = at 0

-- Render e where at least level p is needed: if e holds looser
-- than that, wrap it in parentheses.
-- 少なくとも強さ p が要る場所に e を書く。e がそれより弱ければ、括弧で包む
at :: Int -> Expr -> Text
at p e
  | level e < p = "(" <> plain e <> ")"
  | otherwise   = plain e

plain :: Expr -> Text
plain expr = case expr of
  Lit (LInt n)      -> T.pack (show n)
  Lit (LBool True)  -> "true"
  Lit (LBool False) -> "false"
  Lit (LString s)   -> renderString s
  Var x             -> x
  Lam x body        -> "\\" <> x <> " -> " <> at 0 body
  App f a           -> at 7 f <> " " <> at 8 a
  Let x e body      -> "let " <> x <> " = " <> at 0 e <> " in " <> at 0 body
  If c a b          -> "if " <> at 0 c <> " then " <> at 0 a
                         <> " else " <> at 0 b
  Tuple es          -> "(" <> T.intercalate ", " (map (at 0) es) <> ")"
  List es           -> "[" <> T.intercalate ", " (map (at 0) es) <> "]"
  BinOp op l r      ->
    let n = opLevel op
        -- How each operator leans. 1 - 2 - 3 is (1 - 2) - 3,
        -- 1 : 2 : xs is 1 : (2 : xs), and a == b == c is not allowed.
        -- 演算子の寄り方。+ - * / は左、: ++ && || は右に寄る。
        -- == などは続けられないので、両側とも強さが要る
        (lp, rp)
          | n == 3        = (n + 1, n + 1)
          | leansRight op = (n + 1, n)
          | otherwise     = (n, n + 1)
        leansRight o = o `elem` [Concat, Cons, And, Or]
    in at lp l <> " " <> opSymbol op <> " " <> at rp r
-- ANCHOR_END: render

-- ANCHOR: string
-- 🧵 A string as Lam source code: "say \"hi\"\n".
--    Only " \ newline and tab need a backslash.
-- 文字列を、Lam のソースコードにする。
-- バックスラッシュが要るのは、" \ 改行 タブだけ
renderString :: Text -> Text
renderString s = "\"" <> T.concatMap escape s <> "\""
  where
    escape c = case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      _    -> T.singleton c
-- ANCHOR_END: string
-- ANCHOR_END: all
