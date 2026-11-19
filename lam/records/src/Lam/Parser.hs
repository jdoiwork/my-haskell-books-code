{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Read Lam source code into an AST, with megaparsec.
-- Lam のソースコードを、megaparsec で AST に読む。
module Lam.Parser
  ( Parser
  , runLamParser
  , parseExpr
  , parseLam
  , expr
  , definition
  , topDefinition
  , signature
  , signatureHead
  , aliasDecl
  , Binding (..)
  , letBinding
  , program
  , atom
  , name
  , conName
  , pattern
  , patternAtom
  , dataDecl
  , typeAtom
  , continuing
  , bar
  , branchesOf
  , declaration
  , spaces
  , symbol
  , keyword
  , keywords
  ) where

import Control.Monad (unless, when)
import Control.Monad.Combinators.Expr (makeExprParser)
import Control.Monad.Reader (Reader, ask, local, runReader)
import qualified Control.Monad.Combinators.Expr as E
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lam.Sugar
import Lam.Syntax

-- ANCHOR: lexer
-- 🔤 A parser that reads Text; no custom error type (Void).
--    It can also read one column (Pos): an expression goes on only on
--    lines to the right of it (see continuing).
-- Text を読むパーサー。自分のエラーの型はなし（Void）。
-- 列（Pos）を1つ読める：式は、それより右の行にだけ続く（continuing）
-- ANCHOR: parserType
type Parser = ParsecT Void Text (Reader Pos)

-- ▶️ Run a parser. At first, the column is 1: a word at column 1
--    starts a new definition.
-- パーサーを動かす。はじめの列は 1：1桁目の言葉は新しい定義の始まり
runLamParser
  :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runLamParser p file source = runReader (runParserT p file source) pos1
-- ANCHOR_END: parserType

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
-- ANCHOR: keywordList
keywords :: [Text]
keywords =
  [ "let", "in", "if", "then", "else", "true", "false"
  , "case", "of", "data"
  -- ANCHOR: keywordsRecords
  , "type", "where" ]
  -- ANCHOR_END: keywordsRecords
-- ANCHOR_END: keywordList

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
name = (<?> "a name") $ lexeme $ try rawName

-- 🏷️ A name, with no spaces after it.
-- 名前（後ろの空白は読まない）
rawName :: Parser Name
rawName = do
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
--    something in ( ) (an expression, a tuple, an operator, a
--    section), or a list (or a list comprehension).
-- いちばん小さな部品：数、true・false、文字列、名前、( ) の中身（式、
-- タプル、演算子、セクション）、リスト（とリスト内包表記）
-- <?> gives a part a name for error messages: "expecting a value".
-- <?> は、エラーの文で使う部品の名前：「値が来るはず」
-- ANCHOR: atomOnly
atom :: Parser Expr
atom = choice
  [ Lit . LInt <$> integer
  , Lit (LBool True)  <$ keyword "true"
  , Lit (LBool False) <$ keyword "false"
  , Lit . LString <$> stringLiteral
  , nameWithFields
  , accessor
  , Var <$> conName
  , parenthesized
  , listOrComprehension
  , recordLiteral
  ] <?> "a value"
-- ANCHOR_END: atomOnly

-- ANCHOR: tuple
-- 🧺 Inside ( ): an operator alone (+), a section (+ 1) or (10 -),
--    or expressions: (e) is just e, but (a, b) and (a, b, c) are tuples.
-- ( ) の中身：演算子だけの (+)、セクションの (+ 1)・(10 -)、または式。
-- (e) はただの e。(a, b) や (a, b, c) はタプル
parenthesized :: Parser Expr
parenthesized = do
  _ <- symbol "("
  inside <- operatorFirst <|> expressionFirst
  -- ANCHOR: parenFields
  -- Fields may follow the ) with no space:  (f x).name
  -- ) のすぐ後ろに、フィールドを続けてよい
  lexeme (fields inside)
  -- ANCHOR_END: parenFields
  where
    -- (+) and (+ 1). An expression cannot start with an operator, so
    -- once an operator is read, it is one of these two.
    -- 式は演算子で始まらないので、演算子を読んだら、この2つのどちらか
    operatorFirst = do
      (s, o) <- bareOperator
      choice
        [ operatorFunction o <$ try (spaces *> char ')')
        , do
            -- (- 1) is a section, but (-1) waits for negative numbers.
            -- (- 1) はセクション。(-1) は負の数のためにとっておく
            when (s == "-") spaceAfterMinus
            spaces
            r <- expr
            _ <- char ')'
            pure (rightSection o r) ]
    expressionFirst = do
      first <- expr
      choice
        [ do (_, o) <- operatorSymbol
             _ <- char ')'
             pure (leftSection first o)
        , do rest <- many (symbol "," *> expr)
             _ <- char ')'
             pure (case rest of
                     [] -> first
                     _  -> Tuple (first : rest)) ]
    spaceAfterMinus = do
      spaced <- option False (True <$ lookAhead spaceChar)
      unless spaced $
        fail "(-1) is not a section: write (- 1) for \\x -> x - 1"

-- ANCHOR_END: tuple

-- ANCHOR: dotted
-- 🔎 A name and the fields after it, with no spaces:  r.name,
--    order.customer.name.  A small letter right after the dot is a
--    field.
-- 名前と、その後ろのフィールド（空白なし）。点のすぐ後ろの小文字は
-- フィールド
nameWithFields :: Parser Expr
nameWithFields = (<?> "a name") $ lexeme $ try $ do
  n <- rawName
  fields (Var n)

-- 🔎 The fields after something:  e.name.age  is  (e.name).age
-- 何かの後ろのフィールド
fields :: Expr -> Parser Expr
fields e = do
  -- hidden: an error message does not say "expecting '.'".
  -- hidden：エラーの文に「'.' が来るはず」と出さない
  fs <- many (hidden dotField)
  pure (foldl Field e fs)

-- .name, right after the thing before it.
-- 前のものにくっついた .name
dotField :: Parser Name
dotField = try (char '.' *> rawName)
-- ANCHOR_END: dotted

-- ANCHOR: accessor
-- 🔧 .name alone is a function that reads the field name:
--    map .name crew.  It is sugar for  \$r -> $r.name.
-- .name だけなら、フィールド name を読む関数。\$r -> $r.name の糖衣構文
accessor :: Parser Expr
accessor = (<?> "a field like .name") $ lexeme $ do
  x <- dotField
  pure (Lam "$r" (Field (Var "$r") x))
-- ANCHOR_END: accessor

-- ANCHOR: recordLiteral
-- 📇 A record:  { name = "Lam", age = 3 },  and a changed copy of
--    one:  { r | age = 4 }.
-- レコードと、その一部を変えたもの
recordLiteral :: Parser Expr
recordLiteral = lexeme $ do
  -- A record starts with  { name =  or  { name |.  Read ahead, so that
  -- nothing is read if it is not one (the training adds { name, days }).
  -- レコードは { name = か { name | で始まる。先読みして、そうでなければ
  -- 何も読まない（トレーニングで { name, days } を足すため）
  _ <- try (symbol "{" <* lookAhead (name *> (bar <|> equals)))
  base <- optional (try (name <* bar))
  given <- recordField `sepBy1` symbol ","
  _ <- char '}'
  case [ x | (i, (x, _)) <- zip [0 :: Int ..] given
           , x `elem` map fst (drop (i + 1) given) ] of
    -- 「フィールド age が2度あります」
    x : _ -> fail ("the field " <> T.unpack x <> " is written twice")
    []    -> fields
               (maybe (Record given) (\r -> Update (Var r) given) base)
  where
    recordField = do
      x <- name
      _ <- equals
      e <- expr
      pure (x, e)
-- ANCHOR_END: recordLiteral

-- ANCHOR: listOrComprehension
-- 🧺 [1, 2, 3], or a list comprehension [x * 2 | x <- xs, x > 1].
-- リスト、またはリスト内包表記
listOrComprehension :: Parser Expr
listOrComprehension = between (symbol "[") (symbol "]") $ do
  items <- expr `sepBy` symbol ","
  case items of
    [e] -> option (List [e])
             (comprehension e <$> (bar *> qualifier `sepBy1` symbol ","))
    _   -> pure (List items)

-- 🧺 x <- xs,  let y = x * 2,  or a condition x > 1.
--    A let followed by in is not one: it is a condition.
-- 内包表記の部品。in が続く let は部品ではなく、条件の式
qualifier :: Parser Qualifier
qualifier = choice
  [ do binding <- try (letBinding <* notFollowedBy (keyword "in"))
       pure $ case binding of
         Named y e        -> LetName y e
         Destructured p e -> LetPattern p e
  , do p <- try (pattern <* symbol "<-")
       Generator p <$> expr
  , Condition <$> expr ]
-- ANCHOR_END: listOrComprehension

-- ANCHOR: operatorSymbol
-- 🔣 The symbol of an operator, and the way to build its tree.
--    bareOperator leaves the spaces after it, so that (- 1) and (-1)
--    can be told apart.
-- 演算子の記号と、その木の作り方。bareOperator は後ろの空白を残す
-- （(- 1) と (-1) を見分けるため）
operatorSymbol :: Parser (Text, Operator)
operatorSymbol = lexeme bareOperator

bareOperator :: Parser (Text, Operator)
bareOperator = choice
  [ (s, o) <$ try (string s <* notFollowedBy (oneOf opLetters))
  | (_, s, o) <- concat operatorRows ]
-- ANCHOR_END: operatorSymbol

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
--    so it cannot continue the expression above it. The column is
--    read with ask: it is 1, except in the parts of a where (they
--    start at their own column).
-- いちばん左（1桁目）の言葉は、新しい定義の始まり。
-- だから、上の式の続きにはならない。列は ask で読む。ふつうは 1 だが、
-- where の部品の中では、部品の始まる列になる
continuing :: Parser ()
continuing = do
  column <- L.indentLevel
  limit  <- ask
  if column > limit then pure () else empty
-- ANCHOR_END: continuing
-- ANCHOR_END: atom

-- ANCHOR: operators
-- ➕ Operators, from the tightest to the loosest (the same order
--    as Haskell, and >> << |> <| as in Elm). LeftA: 1 - 2 - 3 is
--    (1 - 2) - 3.  RightA: 1 : 2 : xs is 1 : (2 : xs).
--    NoneA: a == b == c is an error.
-- 演算子。強いものから順に（Haskell と同じ順番。>> << |> <| は Elm と同じ）
data Assoc = LeftA | RightA | NoneA

operatorRows :: [[(Assoc, Text, Operator)]]
operatorRows =
  [ [ (RightA, ">>", Operator composeRight) ]
  , [ (LeftA,  "<<", Operator composeLeft) ]
  , [ (LeftA, "*", binary Mul), (LeftA, "/", binary Div) ]
  , [ (LeftA, "+", binary Add), (LeftA, "-", binary Sub) ]
  , [ (RightA, "++", binary Concat), (RightA, ":", binary Cons) ]
  , [ (NoneA, "==", binary Eq), (NoneA, "/=", binary Ne)
    , (NoneA, "<",  binary Lt), (NoneA, "<=", binary Le)
    , (NoneA, ">",  binary Gt), (NoneA, ">=", binary Ge) ]
  , [ (RightA, "&&", binary And) ]
  , [ (RightA, "||", binary Or) ]
  , [ (LeftA,  "|>", Operator pipe) ]
  , [ (RightA, "<|", Operator pipeLeft) ]
  ]
  where
    binary o = Operator (BinOp o)

-- The letters that operators are made of.
-- 演算子を作る文字
opLetters :: String
opLetters = "+-*/=<>:&|"

-- 🧮 Calls and operators together. An operator is not followed by
--    more operator letters ("+" is not the start of "++", "<" not of
--    "<=", "-" not of "->"), nor by ")" (that is a section: (10 -)).
-- 関数の呼び出しと、演算子。演算子のあとに、演算子の文字は続かない。
-- ) も続かない（それはセクションの (10 -)）
operatorExpr :: Parser Expr
operatorExpr = makeExprParser application (map (map toInfix) operatorRows)
  where
    toInfix (assoc, s, Operator build) =
      let symbolOnly = (build <$ op s) <?> "an operator"
      in case assoc of
           LeftA  -> E.InfixL symbolOnly
           RightA -> E.InfixR symbolOnly
           NoneA  -> E.InfixN symbolOnly
    op s = try (lexeme (continuing *> string s
                          <* notFollowedBy (oneOf opLetters))
                  <* notFollowedBy (char ')'))
-- ANCHOR_END: operators

-- ANCHOR: expr
-- 🌳 A whole expression. \, let and if reach to the right end.
-- 式全体。\、let、if は右の端まで続く
expr :: Parser Expr
expr = choice [lambda, letIn, ifThenElse, caseOf, operatorExpr]

-- ANCHOR: lambda
-- \x y -> body  is  \x -> \y -> body  (sugar, no new tree shape).
--    The arguments may be patterns: \(a, b) -> a + b.
-- \x y -> body は \x -> \y -> body のこと（糖衣構文。木の形は増えない）。
-- 引数はパターンでもよい
lambda :: Parser Expr
lambda = do
  _      <- symbol "\\"
  params <- some patternAtom
  _      <- symbol "->"
  body   <- expr
  pure (lambdaWith params body)
-- ANCHOR_END: lambda

-- ANCHOR: letIn
-- 🏷️ let f x = e in body, or let (q, r) = e in body.
-- ふつうの let と、パターンの let
letIn :: Parser Expr
letIn = do
  binding <- letBinding
  case binding of
    Named x e -> do
      keyword "in"
      Let x e <$> expr
    -- At the top, only names can be defined: a pattern needs in.
    -- いちばん外で定義できるのは名前だけ。パターンには in が要る
    Destructured p e -> do
      keyword "in" <?> "in (a let with a pattern needs in)"
      letPattern p e <$> expr
-- ANCHOR_END: letIn

-- ANCHOR: definition
-- 🏷️ What a let gives a name to: a name (with arguments), or the
--    parts of a pattern.
-- let が名前を付けるもの：名前（引数つきでもよい）か、パターンの部品
data Binding
  = Named Name Expr           -- let f x y = body
  | Destructured Pattern Expr -- let (q, r) = e

letBinding :: Parser Binding
letBinding = do
  keyword "let"
  named <|> destructured
  where
    named = do
      (f, e) <- afterLet
      pure (Named f e)
    destructured = do
      p <- pattern
      _ <- equals
      Destructured p <$> expr

-- 🏷️ let f x y = body  is  let f = \x -> \y -> body.
--    The arguments may be patterns, as in a lambda.
-- let f x y = body は let f = \x -> \y -> body のこと。
-- 引数は、ラムダと同じくパターンでもよい
definition :: Parser (Name, Expr)
definition = keyword "let" *> afterLet

afterLet :: Parser (Name, Expr)
afterLet = do
  f      <- name
  params <- many patternAtom
  _      <- equals
  body   <- expr
  pure (f, lambdaWith params body)

-- = but not ==
-- = だが == ではない
equals :: Parser ()
equals = () <$ lexeme (string "=" <* notFollowedBy (char '='))
-- ANCHOR_END: definition

-- ANCHOR: topDefinition
-- 🏷️ A definition at the top of a file: let may be left out, and
--    where may follow.
--      area w h = w * h            is  let area w h = w * h
--      f x = a + b where ...       is  f x = let a = ... in let b = ...
--    Without let, we read ahead to the = first: a line that is not a
--    definition is the expression at the end.
-- ファイルのいちばん外の定義：let は省ける。where を後ろに付けてよい。
-- let がなければ、先に = まで読んでみる。定義でない行は、最後の式
topDefinition :: Parser (Name, Expr)
topDefinition = do
  (f, params) <- try $ do
    _      <- optional (keyword "let")
    f      <- name
    params <- many patternAtom
    _      <- equals
    pure (f, params)
  body   <- expr
  locals <- option [] whereBlock
  pure (f, lambdaWith params (foldr (uncurry Let) body locals))
-- ANCHOR_END: topDefinition

-- ANCHOR: whereBlock
-- 📎 where, then definitions one under another (at the same column):
--      summary scores = total ++ " points"
--        where
--          total = showInt (sum scores)
--    The parts may use the arguments, and the parts above them.
-- where のあとに、定義を縦に並べる（同じ列から）。部品は、引数と、
-- 上にある部品を使える
whereBlock :: Parser [(Name, Expr)]
whereBlock = do
  try (continuing *> keyword "where")
  column <- L.indentLevel
  local (const column) (afterLet `sepBy1` atColumn column)
  where
    atColumn column = do
      now <- L.indentLevel
      if now == column then pure () else empty
-- ANCHOR_END: whereBlock

-- ANCHOR: signature
-- ✍️ A type annotation, on its own line above the definition:
--      add : Int -> Int -> Int
--      largest : Comparable a => [a] -> Maybe a
--    It must be followed by a definition, so that  x : xs  at the end
--    of a file is still an expression.
-- 型の注釈。定義の上の行に書く。後ろに定義が続かなければならない。
-- そうすれば、ファイルの終わりの x : xs は、式のまま
signature :: Parser Decl
signature = try $ do
  sig <- signatureHead
  _   <- lookAhead (optional (keyword "let") *> name)
  pure sig

-- ✍️ The annotation alone (the REPL puts ; and the definition after it).
-- 注釈だけ（REPL では、後ろに ; と定義を続ける）
signatureHead :: Parser Decl
signatureHead = do
  x     <- name
  _     <- lexeme (char ':' <* notFollowedBy (oneOf opLetters))
  conds <- option [] (try (constraints <* symbol "=>"))
  t     <- typeExpr
  pure (DefSig x (Signature conds t))
  where
    constraints =
      between (symbol "(") (symbol ")") (condition `sepBy1` symbol ",")
        <|> fmap pure condition
    condition = (,) <$> conName <*> name
-- ANCHOR_END: signature

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
  branchesOf scrutinee

-- 🔀 The branches after "of", for the value e.
-- of のあとの枝。e の値を分ける
branchesOf :: Expr -> Parser Expr
branchesOf scrutinee = do
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

-- 🚧 A single | (not the start of || or |>).
-- 1つだけの |（|| や |> の頭ではない）
bar :: Parser ()
bar = lexeme (try (char '|' *> notFollowedBy (oneOf ("|>" :: String))))
        <?> "|"
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
  -- ANCHOR: recordPattern
  -- { name, age }: take these fields, under their own names.
  -- これらのフィールドを、同じ名前で取り出す
  , PRecord <$> between (symbol "{") (symbol "}") (name `sepBy1` symbol ",")
  -- ANCHOR_END: recordPattern
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
  , recordType
  ] <?> "a type"

-- ANCHOR: recordType
-- 📇 { name : String, age : Int },  and  { r | name : String }:
--    a record with name, and maybe other fields (r stands for them).
-- name を持ち、ほかのフィールドもあるかもしれないレコード
--（r が、ほかのフィールドを表す）
recordType :: Parser TypeExpr
recordType = between (symbol "{") (symbol "}") $ do
  rest  <- optional (try (name <* bar))
  given <- fieldType `sepBy1` symbol ","
  pure (TyRecord given rest)
  where
    fieldType = do
      x <- name
      _ <- lexeme (char ':')
      t <- typeExpr
      pure (x, t)
-- ANCHOR_END: recordType

-- ANCHOR: aliasDecl
-- 🏷️ type alias Member = { name : String, days : Int }
-- 型の別名
aliasDecl :: Parser AliasDecl
aliasDecl = do
  keyword "type"
  keyword "alias"
  t      <- conName
  params <- many name
  _      <- equals
  AliasDecl t params <$> typeExpr
-- ANCHOR_END: aliasDecl
-- ANCHOR_END: dataDecl

-- ANCHOR: run
-- 📖 Read a whole program (spaces at the start, nothing left at the end).
-- プログラム全体を読む（先頭の空白から、最後まで読みきる）
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runLamParser (spaces *> expr <* eof) "<input>"

-- 📣 The same, with an error message for people.
-- 同じだが、人に読めるエラーの文にする
parseLam :: FilePath -> Text -> Either Text Expr
parseLam file source =
  case runLamParser (spaces *> expr <* eof) file source of
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
-- ANCHOR: declaration
declaration :: Parser Decl
declaration = choice
  [ DefData <$> dataDecl
  -- ANCHOR: declarationMore
  , DefAlias <$> aliasDecl
  , signature
  -- ANCHOR_END: declarationMore
  , try (uncurry DefValue <$> topDefinition <* notFollowedBy (keyword "in")) ]
-- ANCHOR_END: declaration
-- ANCHOR_END: program
-- ANCHOR_END: run
-- ANCHOR_END: all
