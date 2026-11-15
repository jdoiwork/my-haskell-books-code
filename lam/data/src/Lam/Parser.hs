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
  , conName
  , pattern
  , dataDecl
  , typeAtom
  , continuing
  , bar
  , declaration
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
keywords =
  [ "let", "in", "if", "then", "else", "true", "false"
  , "case", "of", "data" ]

-- 🔑 A keyword, not followed by more letters ("in" but not "inside").
-- キーワード。後ろに文字が続かない（"in" だが "inside" ではない）
--    try: if "if" is the start of "iffy", read nothing, so that the
--    name iffy can be read next.
-- try：「if」が「iffy」の頭なら、何も読まなかったことにする。
-- そうすれば、次に名前の iffy を読める
keyword :: Text -> Parser ()
keyword w =
  lexeme (try (string w *> notFollowedBy (alphaNumChar <|> char '_')))

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

-- ANCHOR: conName
-- 🏷️ A constructor or a type: a capital letter, then letters, digits,
--    _ or '.  Just, Circle, Maybe
-- 構成子や型の名前：大文字で始まり、文字・数字・_・' が続く
conName :: Parser Name
conName = (<?> "a constructor") $ lexeme $ try $ do
  first <- upperChar
  rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure (T.pack (first : rest))
-- ANCHOR_END: conName

-- 🔢 A whole number: 0, 42, ...
-- 整数
integer :: Parser Integer
integer = lexeme L.decimal <?> "a number"
-- ANCHOR_END: words

-- ANCHOR: stringLiteral
-- 🧵 A string: "hello", "say \"hi\"\n". The escapes are
--    \n (new line), \t (tab), \" and \\.
-- 文字列。エスケープは \n（改行）、\t（タブ）、\" と \\
stringLiteral :: Parser Text
stringLiteral = (<?> "a string") $ lexeme $ do
  _ <- char '"'
  cs <- manyTill stringChar (char '"')
  pure (T.pack cs)
  where
    stringChar = (char '\\' *> escaped) <|> anySingle
    escaped = choice
      [ '\n'  <$ char 'n'
      , '\t'  <$ char 't'
      , '"'   <$ char '"'
      , '\\'  <$ char '\\'
      ] <?> "an escape: \\n \\t \\\" or \\\\"
-- ANCHOR_END: stringLiteral

-- ANCHOR: atom
-- ⚛️ The smallest pieces: a number, true/false, a string, a name,
--    ( expr ), a tuple, or a list.
-- いちばん小さな部品：数、true・false、文字列、名前、( 式 )、タプル、リスト
-- <?> gives a part a name for error messages: "expecting a value".
-- <?> は、エラーの文で使う部品の名前：「値が来るはず」
-- ANCHOR: atomOnly
atom :: Parser Expr
atom = choice
  [ Lit . LInt <$> integer
  , Lit (LBool True)  <$ keyword "true"
  , Lit (LBool False) <$ keyword "false"
  , Lit . LString <$> stringLiteral
  , Var <$> name
  , Var <$> conName
  , parensOrTuple
  , List <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
  ] <?> "a value"
-- ANCHOR_END: atomOnly

-- ANCHOR: tuple
-- 🧺 (e) is just e, but (a, b) and (a, b, c) are tuples.
-- (e) はただの e。(a, b) や (a, b, c) はタプル
parensOrTuple :: Parser Expr
parensOrTuple = do
  es <- between (symbol "(") (symbol ")") (expr `sepBy1` symbol ",")
  pure $ case es of
    [e] -> e
    _   -> Tuple es
-- ANCHOR_END: tuple

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
-- ➕ Operators, from the tightest to the loosest (the same order
--    as Haskell). InfixL: 1 - 2 - 3 is (1 - 2) - 3.
--    InfixR: 1 : 2 : xs is 1 : (2 : xs).  InfixN: a == b == c is an error.
-- 演算子。強いものから順に（Haskell と同じ順番）
operators :: [[Operator Parser Expr]]
operators =
  [ [ InfixL (BinOp Mul <$ op "*"), InfixL (BinOp Div <$ op "/") ]
  , [ InfixL (BinOp Add <$ op "+"), InfixL (BinOp Sub <$ op "-") ]
  , [ InfixR (BinOp Concat <$ op "++"), InfixR (BinOp Cons <$ op ":") ]
  , [ InfixN (BinOp Eq <$ op "=="), InfixN (BinOp Ne <$ op "/=")
    , InfixN (BinOp Lt <$ op "<" ), InfixN (BinOp Le <$ op "<=")
    , InfixN (BinOp Gt <$ op ">" ), InfixN (BinOp Ge <$ op ">=") ]
  , [ InfixR (BinOp And <$ op "&&") ]
  , [ InfixR (BinOp Or  <$ op "||") ]
  ]
  where
    -- An operator is not followed by more operator letters:
    -- "+" is not the start of "++", "<" not of "<=", "-" not of "->".
    -- 演算子のあとに、演算子の文字は続かない
    op s = lexeme (try (continuing *> string s
                          <* notFollowedBy (oneOf opLetters)))
             <?> "an operator"
    opLetters = "+-*/=<>:&|" :: String

-- 🧮 Calls and operators together.
-- 関数の呼び出しと、演算子
operatorExpr :: Parser Expr
operatorExpr = makeExprParser application operators
-- ANCHOR_END: operators

-- ANCHOR: expr
-- 🌳 A whole expression. \, let and if reach to the right end.
-- 式全体。\、let、if は右の端まで続く
expr :: Parser Expr
expr = choice [lambda, letIn, ifThenElse, caseOf, operatorExpr]

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

-- ANCHOR: caseOf
-- 🔀 case e of | p -> a | q -> b
--    Each branch starts with |  (the first | may be left out).
--    A branch may have a guard:  | n if n < 0 -> "negative"
-- 枝は | で始める（最初の | は省いてよい）。枝にはガードを付けられる
caseOf :: Parser Expr
caseOf = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  _ <- optional bar
  branches <- branch `sepBy1` bar
  pure (expandGuards scrutinee branches)
  where
    branch = do
      p     <- pattern
      guard <- optional (keyword "if" *> expr)
      _     <- symbol "->"
      body  <- expr
      pure (p, guard, body)

-- 🚧 A single | (not the start of ||).
-- 1つだけの |（|| の頭ではない）
bar :: Parser ()
bar = lexeme (try (char '|' *> notFollowedBy (char '|'))) <?> "|"
-- ANCHOR_END: caseOf

-- ANCHOR: guards
-- 🛡️ Guards are sugar. A branch whose guard is false goes on to the
--    branches below it, so they become a function $next, called then.
--      case e of | p if g -> a | rest
--    is
--      let $it = e in
--      let $next = \$u -> case $it of | rest in
--      case $it of | p -> (if g then a else $next 0) | _ -> $next 0
--    $it and $next cannot be written in Lam, so they hit no other name.
-- ガードは糖衣構文。ガードが偽の枝は、下の枝へ進むので、下の枝を
-- 関数 $next にしておいて、そのときに呼ぶ。$it と $next は Lam では
-- 書けない名前なので、ほかの名前とぶつからない
expandGuards :: Expr -> [(Pattern, Maybe Expr, Expr)] -> Expr
expandGuards scrutinee branches
  | all unguarded branches = Case scrutinee [ (p, b) | (p, _, b) <- branches ]
  | otherwise              = Let "$it" scrutinee (go branches)
  where
    unguarded (_, g, _) = g == Nothing
    go bs = case break (not . unguarded) bs of
      (plain, (p, Just g, b) : rest) ->
        let next  = App (Var "$next") (Lit (LInt 0))
            alts  = [ (q, c) | (q, _, c) <- plain ]
                    <> [(p, If g b next), (PWild, next)]
        in Let "$next" (Lam "$u" (go rest)) (Case (Var "$it") alts)
      _ -> Case (Var "$it") [ (q, c) | (q, _, c) <- bs ]
-- ANCHOR_END: guards

-- ANCHOR: patterns
-- 🧩 A pattern:  x : rest  leans to the right, like the operator.
-- パターン。x : rest は、演算子と同じく右に寄る
pattern :: Parser Pattern
pattern = do
  p <- patternApp
  rest <- optional (consSymbol *> pattern)
  pure (maybe p (PCons p) rest)
  where
    consSymbol =
      lexeme (try (char ':' *> notFollowedBy (oneOf ("+-*/=<>:&|" :: String))))

-- 🧩 A constructor with its patterns (Just x, Rect w h), or a small one.
-- 構成子と、その引数のパターン。または小さなパターン
patternApp :: Parser Pattern
patternApp =
  (PCon <$> conName <*> many (continuing *> patternAtom)) <|> patternAtom

patternAtom :: Parser Pattern
patternAtom = choice
  [ PWild <$ lexeme (try (char '_' <* notFollowedBy nameChar))
  , PVar <$> name
  , (\c -> PCon c []) <$> conName
  , PLit . LInt <$> integer
  , PLit . LString <$> stringLiteral
  , PLit (LBool True)  <$ keyword "true"
  , PLit (LBool False) <$ keyword "false"
  , parensOrTuplePattern
  , PList <$> between (symbol "[") (symbol "]") (pattern `sepBy` symbol ",")
  ] <?> "a pattern"
  where
    nameChar = alphaNumChar <|> char '_' <|> char '\''
    parensOrTuplePattern = do
      ps <- between (symbol "(") (symbol ")") (pattern `sepBy1` symbol ",")
      pure $ case ps of
        [p] -> p
        _   -> PTuple ps
-- ANCHOR_END: patterns

-- ANCHOR: dataDecl
-- 📐 data Shape = Circle Int | Rect Int Int
--    data Maybe a = Nothing | Just a
-- data の宣言
dataDecl :: Parser DataDecl
dataDecl = do
  keyword "data"
  t      <- conName
  params <- many name
  _      <- lexeme (string "=" <* notFollowedBy (char '='))
  _      <- optional bar
  cons   <- constructor `sepBy1` bar
  pure (DataDecl t params cons)
  where
    constructor = (,) <$> conName <*> many (continuing *> typeAtom)

-- 🏷️ A type:  Int -> Maybe a  (-> leans to the right).
-- 型。-> は右に寄る
typeExpr :: Parser TypeExpr
typeExpr = do
  t <- typeApp
  rest <- optional (symbol "->" *> typeExpr)
  pure (maybe t (TyFun t) rest)

-- 🏷️ A type name with its arguments (Maybe Int), or a small type.
-- 型の名前と、その引数。または小さな型
typeApp :: Parser TypeExpr
typeApp = (TyCon <$> conName <*> many (continuing *> typeAtom)) <|> typeAtom

typeAtom :: Parser TypeExpr
typeAtom = choice
  [ (\c -> TyCon c []) <$> conName
  , TyVar <$> name
  , TyList <$> between (symbol "[") (symbol "]") typeExpr
  , do ts <- between (symbol "(") (symbol ")") (typeExpr `sepBy1` symbol ",")
       pure $ case ts of
         [t] -> t
         _   -> TyTuple ts
  ] <?> "a type"
-- ANCHOR_END: dataDecl

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
-- 📜 A program file: declarations (let without in, and data), then
--    an expression to compute at the end (it can be left out).
-- プログラムのファイル：宣言（in のない let と、data）を並べ、最後に
-- 計算する式を1つ置く（なくてもよい）
program :: Parser ([Decl], Maybe Expr)
program = do
  spaces
  decls <- many declaration
  final <- optional expr
  eof
  pure (decls, final)

-- 📜 One declaration. A let followed by in is not one: it is the
--    expression at the end.
-- 宣言を1つ。in が続く let は宣言ではなく、最後の式
declaration :: Parser Decl
declaration = choice
  [ DefData <$> dataDecl
  , try (uncurry DefValue <$> definition <* notFollowedBy (keyword "in")) ]
-- ANCHOR_END: program
-- ANCHOR_END: run
-- ANCHOR_END: all
