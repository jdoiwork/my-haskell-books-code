{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Read Lam source code into an AST, with megaparsec.
-- Lam のソースコードを、megaparsec で AST に読む。
module Lam.Parser
  ( Parser
  , parseExpr
  , parseLam
  , expr
  , definition
  , program
  , atom
  , name
  , spaces
  , symbol
  , keyword
  , keywords
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lam.Syntax

-- ANCHOR: lexer
-- 🔤 A parser that reads Text; no custom error type (Void).
-- Text を読むパーサー。自分のエラーの型はなし（Void）
type Parser = Parsec Void Text

-- 🫧 Skip spaces, line breaks, and comments from -- to the end.
-- 空白・改行と、-- から行の終わりまでのコメントを飛ばす
spaces :: Parser ()
spaces = L.space space1 (L.skipLineComment "--") empty

-- ✂️ Read p, then skip the spaces after it.
-- p を読んでから、後ろの空白を飛ばす
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

-- 🔣 Read exactly this text: "(", "->", "in", ...
-- ちょうどこの文字を読む
symbol :: Text -> Parser Text
symbol = L.symbol spaces
-- ANCHOR_END: lexer

-- ANCHOR: words
-- 🚫 Words that cannot be variable names.
-- 変数の名前にできない言葉
keywords :: [Text]
keywords = ["let", "in", "if", "then", "else", "true", "false"]

-- 🔑 A keyword, not followed by more letters ("in" but not "inside").
-- キーワード。後ろに文字が続かない（"in" だが "inside" ではない）
keyword :: Text -> Parser ()
keyword w = lexeme (string w *> notFollowedBy (alphaNumChar <|> char '_'))

-- 🏷️ A name: a small letter, then letters, digits, _ or '.
-- 名前：小文字で始まり、文字・数字・_・' が続く
name :: Parser Name
name = (<?> "a name") $ lexeme $ try $ do
  first <- lowerChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
  let n = T.pack (first : rest)
  if n `elem` keywords
    then fail ("the keyword " <> show n <> " cannot be a name")
    else pure n

-- 🔢 A whole number: 0, 42, ...
-- 整数
integer :: Parser Integer
integer = lexeme L.decimal <?> "a number"
-- ANCHOR_END: words

-- ANCHOR: atom
-- ⚛️ The smallest pieces: a number, true/false, a name, or ( expr ).
-- いちばん小さな部品：数、true・false、名前、( 式 )
-- <?> gives a part a name for error messages: "expecting a value".
-- <?> は、エラーの文で使う部品の名前：「値が来るはず」
atom :: Parser Expr
atom = choice
  [ Lit . LInt <$> integer
  , Lit (LBool True)  <$ keyword "true"
  , Lit (LBool False) <$ keyword "false"
  , Var <$> name
  , between (symbol "(") (symbol ")") expr
  ] <?> "a value"

-- ANCHOR: application
-- 📞 Calling a function: atoms side by side.
--    f x y means (f x) y.
-- 関数を呼ぶ：部品を並べる。f x y は (f x) y
application :: Parser Expr
application = do
  f    <- atom
  args <- many (continuing *> atom)
  pure (foldl App f args)
-- ANCHOR_END: application

-- ANCHOR: continuing
-- 📏 A word at the very left (column 1) starts a new definition,
--    so it cannot continue the expression above it.
-- いちばん左（1桁目）の言葉は、新しい定義の始まり。
-- だから、上の式の続きにはならない
continuing :: Parser ()
continuing = do
  column <- L.indentLevel
  if column > pos1 then pure () else empty
-- ANCHOR_END: continuing
-- ANCHOR_END: atom

-- ANCHOR: operators
-- ➕ Operators, from the tightest to the loosest.
--    InfixL: 1 - 2 - 3 is (1 - 2) - 3.  InfixN: a == b == c is an error.
-- 演算子。強いものから順に
operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (BinOp Mul <$ op "*"), InfixL (BinOp Div <$ op "/") ]
  , [ InfixL (BinOp Add <$ op "+"), InfixL (BinOp Sub <$ op "-") ]
  , [ InfixN (BinOp Eq  <$ op "=="), InfixN (BinOp Lt <$ op "<") ]
  ]
  where
    -- "-" must not be the start of "->".
    -- "-" は "->" の始まりであってはいけない
    op s = lexeme (try (continuing *> string s
                          <* notFollowedBy (char '>')))
             <?> "an operator"

-- 🧮 Calls and operators together.
-- 関数の呼び出しと、演算子
operatorExpr :: Parser Expr
operatorExpr = makeExprParser application operators
-- ANCHOR_END: operators

-- ANCHOR: expr
-- 🌳 A whole expression. \, let and if reach to the right end.
-- 式全体。\、let、if は右の端まで続く
expr :: Parser Expr
expr = choice [lambda, letIn, ifThenElse, operatorExpr]

-- ANCHOR: lambda
-- \x y -> body  is  \x -> \y -> body  (sugar, no new tree shape).
-- \x y -> body は \x -> \y -> body のこと（糖衣構文。木の形は増えない）
lambda :: Parser Expr
lambda = do
  _    <- symbol "\\"
  xs   <- some name
  _    <- symbol "->"
  body <- expr
  pure (foldr Lam body xs)

letIn :: Parser Expr
letIn = do
  (x, e) <- definition
  keyword "in"
  body <- expr
  pure (Let x e body)
-- ANCHOR_END: lambda

-- ANCHOR: definition
-- 🏷️ let f x y = body  is  let f = \x -> \y -> body.
-- let f x y = body は let f = \x -> \y -> body のこと
definition :: Parser (Name, Expr)
definition = do
  keyword "let"
  f    <- name
  xs   <- many name
  _    <- lexeme (string "=" <* notFollowedBy (char '='))
  body <- expr
  pure (f, foldr Lam body xs)
-- ANCHOR_END: definition

ifThenElse :: Parser Expr
ifThenElse = do
  keyword "if"
  c <- expr
  keyword "then"
  a <- expr
  keyword "else"
  b <- expr
  pure (If c a b)
-- ANCHOR_END: expr

-- ANCHOR: run
-- 📖 Read a whole program (spaces at the start, nothing left at the end).
-- プログラム全体を読む（先頭の空白から、最後まで読みきる）
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (spaces *> expr <* eof) "<input>"

-- 📣 The same, with an error message for people.
-- 同じだが、人に読めるエラーの文にする
parseLam :: FilePath -> Text -> Either Text Expr
parseLam file source =
  case parse (spaces *> expr <* eof) file source of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right e  -> Right e

-- ANCHOR: program
-- 📜 A program file: definitions (let without in), then an
--    expression to compute at the end (it can be left out).
-- プログラムのファイル：定義（in のない let）を並べ、最後に
-- 計算する式を1つ置く（なくてもよい）
program :: Parser ([(Name, Expr)], Maybe Expr)
program = do
  spaces
  defs <- many (try (definition <* notFollowedBy (keyword "in")))
  final <- optional expr
  eof
  pure (defs, final)
-- ANCHOR_END: program
-- ANCHOR_END: run
-- ANCHOR_END: all
