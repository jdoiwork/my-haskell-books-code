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
  , program
  , atom
  , name
  , conName
  , qualifiedName
  , mapLiteral
  , pattern
  , doBlock
  , formatString
  , regexLiteral
  , matchRegex
  , substRegex
    -- For the training: a field of your own, and the parts of one.
    -- トレーニングのため：自分のフィールドと、その部品
  , formatStringWith
  , field
  , formatExpr
  , FormatSpec (..)
  , Align (..)
  , formatSpec
  , noSpec
  , applySpec
  , textOf
  , pad
  , call
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
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Lam.Pcre (Regex (..), compileRegex)
import Lam.Syntax

-- ANCHOR: lexer
-- ANCHOR: parserType
-- 🔤 A parser that reads Text; no custom error type (Void).
--    It can also read one Pos: the column that a word must be to the
--    right of, to go on with the expression (see `continuing`).
-- Text を読むパーサー。自分のエラーの型はなし（Void）。
-- 列の位置（Pos）を1つ読める。式の続きになるには、言葉がこの列より
-- 右になければならない（continuing を見る）
type Parser = ParsecT Void Text (Reader Pos)

-- ▶️ Run a parser. At first, a word must be to the right of column 1.
-- パーサーを動かす。はじめは、1桁目より右なら続き
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
keywords :: [Text]
keywords =
  [ "let", "in", "if", "then", "else", "true", "false"
  , "case", "of", "data", "do" ]

-- ANCHOR: keyword
-- 🔑 A keyword, not followed by more letters ("in" but not "inside").
-- キーワード。後ろに文字が続かない（"in" だが "inside" ではない）
--    try: if "do" is the start of "double", read nothing, so that the
--    name double can be read next.
-- try：「do」が「double」の頭なら、何も読まなかったことにする。
-- そうすれば、次に名前の double を読める
keyword :: Text -> Parser ()
keyword w =
  lexeme (try (string w *> notFollowedBy (alphaNumChar <|> char '_')))
-- ANCHOR_END: keyword

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

-- ANCHOR: qualifiedName
-- 🏷️ A name with a module in front: Map.insert, Json.parse.
--    No spaces around the dot. (Lam has no modules yet; the part in
--    front only groups the names, like a family name.)
-- 前にモジュールの名前が付いた名前。点のまわりに空白は入れない。
-- （Lam には、まだモジュールがない。前の部分は、名字のように、
-- 名前をまとめるだけ）
qualifiedName :: Parser Name
qualifiedName = (<?> "a name") $ lexeme $ try $ do
  first <- upperChar
  rest  <- many (alphaNumChar <|> char '_')
  _     <- char '.'
  small <- lowerChar
  more  <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure (T.pack (first : rest <> ('.' : small : more)))
-- ANCHOR_END: qualifiedName

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

-- 🧵 One letter of a string, with the escapes. f"..." uses it too.
-- 文字列の1文字。f"..." でも使う
stringChar :: Parser Char
stringChar = (char '\\' *> escaped) <|> anySingle
  where
    escaped = choice
      [ '\n'  <$ char 'n'
      , '\t'  <$ char 't'
      , '"'   <$ char '"'
      , '\\'  <$ char '\\'
      ] <?> "an escape: \\n \\t \\\" or \\\\"
-- ANCHOR_END: stringLiteral

-- ANCHOR: atom
-- ⚛️ The smallest pieces: a number, true/false, a string, a name,
--    ( expr ), a tuple, a list, or a map.
-- いちばん小さな部品：数、true・false、文字列、名前、( 式 )、タプル、
-- リスト、Map
-- <?> gives a part a name for error messages: "expecting a value".
-- <?> は、エラーの文で使う部品の名前：「値が来るはず」
-- ANCHOR: atomOnly
atom :: Parser Expr
atom = choice
  [ Lit . LInt <$> integer
  , Lit (LBool True)  <$ keyword "true"
  , Lit (LBool False) <$ keyword "false"
  , Lit . LString <$> stringLiteral
  , formatString
  , regexLiteral
  , matchRegex
  , substRegex
  , Var <$> name
  , Var <$> qualifiedName
  , Var <$> conName
  , parensOrTuple
  , List <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
  , mapLiteral
  ] <?> "a value"
-- ANCHOR_END: atomOnly

-- ANCHOR: mapLiteral
-- 🗺️ {"Python": 82, "TS": 75}  is sugar for
--    Map.fromList [("Python", 82), ("TS", 75)].  {} is the empty map.
--    A key is a string written as it is, like JSON. When it looks like
--    a name, the quotes may be left out, like JavaScript:
--    {name: "Lam"} is {"name": "Lam"} (name is NOT a variable here).
--    To use the value of an expression as the key, put it in [ ]:
--    {[name]: "Lam"} uses the string in the variable name.
-- Map.fromList の糖衣構文。キーは、JSON と同じく、そのまま書いた文字列。
-- 名前の形なら、JavaScript と同じく " を省ける。{name: "Lam"} は
-- {"name": "Lam"} のこと（ここの name は変数ではない）。式の値をキーに
-- するときは、[ ] で包む。{[name]: "Lam"} は、変数 name の文字列を使う
mapLiteral :: Parser Expr
mapLiteral = do
  entries <- between (symbol "{") (symbol "}") (entry `sepBy` symbol ",")
  pure (App (Var "Map.fromList")
            (List [ Tuple [k, v] | (k, v) <- entries ]))
  where
    entry = do
      k <- choice
        [ Lit . LString <$> stringLiteral
        , Lit . LString <$> bareKey
        , between (symbol "[") (symbol "]") expr ]
      _ <- symbol ":"
      v <- expr
      pure (k, v)
    -- A key without quotes: letters, digits, _ and ', not starting
    -- with a digit. Keywords are fine here ({if: 1}).
    -- " のないキー：文字・数字・_・'（数字では始まらない）。
    -- ここでは、キーワードも使える
    bareKey = (<?> "a key") $ lexeme $ do
      first <- letterChar <|> char '_'
      rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
      pure (T.pack (first : rest))
-- ANCHOR_END: mapLiteral

-- ANCHOR: tuple
-- 🧺 (e) is just e, but (a, b) and (a, b, c) are tuples.
--    () is the tuple of nothing.
-- (e) はただの e。(a, b) や (a, b, c) はタプル。() は何もないタプル
parensOrTuple :: Parser Expr
parensOrTuple = do
  es <- between (symbol "(") (symbol ")") (expr `sepBy` symbol ",")
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
--    so it cannot continue the expression above it. Inside do, a word
--    at the column of the lines of do starts the next line.
-- いちばん左（1桁目）の言葉は、新しい定義の始まり。
-- だから、上の式の続きにはならない。do の中では、do の行と同じ列の
-- 言葉が、次の行の始まり
continuing :: Parser ()
continuing = do
  column <- L.indentLevel
  limit  <- ask
  if column > limit then pure () else empty
-- ANCHOR_END: continuing
-- ANCHOR_END: atom

-- ANCHOR: operators
-- ➕ Operators, from the tightest to the loosest (the same order
--    as Haskell). InfixL: 1 - 2 - 3 is (1 - 2) - 3.
--    InfixR: 1 : 2 : xs is 1 : (2 : xs).  InfixN: a == b == c is an error.
-- 演算子。強いものから順に（Haskell と同じ順番）
operators :: [[Operator Parser Expr]]
operators = operatorsWith [InfixR (BinOp Cons <$ op ":")]

-- ANCHOR: formatOperators
-- 🪡 Inside { } of f"...", a : ends the expression (the format comes
--    after it), so : (an item in front of a list) is left out there.
--    Write (x : xs) in parentheses, as Python wants lambda in them.
-- f"..." の { } の中では、: で式が終わる（後ろに書式が来る）ので、
-- :（リストの前に足す）は外す。Python の lambda と同じく、括弧で包む
formatOperators :: [[Operator Parser Expr]]
formatOperators = operatorsWith []
-- ANCHOR_END: formatOperators

-- 🧮 The table, with the operators of the level of ++ given.
-- 表。++ と同じ強さの演算子を、渡されたものにする
operatorsWith :: [Operator Parser Expr] -> [[Operator Parser Expr]]
operatorsWith cons =
  [ [ InfixL (BinOp Mul <$ op "*"), InfixL (BinOp Div <$ op "/") ]
  , [ InfixL (BinOp Add <$ op "+"), InfixL (BinOp Sub <$ op "-") ]
  , InfixR (BinOp Concat <$ op "++") : cons
  , [ InfixN (BinOp Eq <$ op "=="), InfixN (BinOp Ne <$ op "/=")
    , InfixN (BinOp Lt <$ op "<" ), InfixN (BinOp Le <$ op "<=")
    , InfixN (BinOp Gt <$ op ">" ), InfixN (BinOp Ge <$ op ">=")
    -- line =~ m/ERROR/  is  Regex.matches m/ERROR/ line (as in Perl)
    -- 「合うか」の演算子（Perl と同じ）
    , InfixN (matchesOp <$ op "=~") ]
  , [ InfixR (BinOp And <$ op "&&") ]
  , [ InfixR (BinOp Or  <$ op "||") ]
  ]

-- An operator is not followed by more operator letters:
-- "+" is not the start of "++", "<" not of "<=", "-" not of "->".
-- 演算子のあとに、演算子の文字は続かない
op :: Text -> Parser Text
op s = lexeme (try (continuing *> string s
                      <* notFollowedBy (oneOf opLetters)))
         <?> "an operator"
  where
    opLetters = "+-*/=<>:&|~" :: String

-- ANCHOR: matchesOp
-- 🐪 a =~ r  is sugar for  Regex.matches r a.
-- a =~ r は Regex.matches r a の糖衣構文
matchesOp :: Expr -> Expr -> Expr
matchesOp text regex = call "Regex.matches" [regex, text]
-- ANCHOR_END: matchesOp

-- 🧮 Calls and operators together.
-- 関数の呼び出しと、演算子
operatorExpr :: Parser Expr
operatorExpr = makeExprParser application operators
-- ANCHOR_END: operators

-- ANCHOR: expr
-- 🌳 A whole expression. \, let and if reach to the right end.
-- 式全体。\、let、if は右の端まで続く
expr :: Parser Expr
expr = choice [lambda, letIn, ifThenElse, caseOf, doBlock, operatorExpr]

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
--    The name may have a module in front:  let Map.insertWith f ...
-- let f x y = body は let f = \x -> \y -> body のこと。
-- 名前の前に、モジュールの名前を付けてもよい
definition :: Parser (Name, Expr)
definition = do
  keyword "let"
  f    <- qualifiedName <|> name
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

-- ANCHOR: doBlock
-- 🎬 do: lines of IO, one under another (or split by ;).
--      do
--        name <- getLine
--        putStrLn name
--    Each line starts at the same column as the first one.
-- do：IO の行を、縦に並べる（; で区切ってもよい）。
-- どの行も、最初の行と同じ列から始める
doBlock :: Parser Expr
doBlock = do
  keyword "do"
  column <- L.indentLevel
  lines' <- local (const column) (doLine `sepBy1` nextLine column)
  either fail pure (desugarDo lines')
  where
    nextLine column = (() <$ symbol ";") <|> atColumn column
    atColumn column = do
      here <- L.indentLevel
      if here == column then pure () else empty

-- 📝 One line of do.
-- do の1行
data DoLine
  = DoBind Name Expr       -- name <- getLine
  | DoLet Name Expr        -- let greeting = "hi " ++ name
  | DoExpr Expr            -- putStrLn greeting

doLine :: Parser DoLine
doLine = choice
  [ do x <- try (name <* symbol "<-")
       DoBind x <$> expr
  , try (uncurry DoLet <$> definition <* notFollowedBy (keyword "in"))
  , DoExpr <$> expr ]
-- ANCHOR_END: doBlock

-- ANCHOR: desugarDo
-- 🍬 do is sugar: it opens into bind and functions.
--      x <- e; rest    is    bind e (\x -> rest)
--      e; rest         is    bind e (\_ -> rest)
--      let x = e; rest is    let x = e in rest
--    The evaluator and the type checker never see do.
-- do は糖衣構文：bind と関数に開く。評価器も型検査器も、do を知らない
desugarDo :: [DoLine] -> Either String Expr
desugarDo ls = case ls of
  [DoExpr e]       -> Right e
  DoExpr e : rest  -> andThen e "_" <$> desugarDo rest
  DoBind x e : rest -> andThen e x <$> desugarDo rest
  DoLet x e : rest -> Let x e <$> desugarDo rest
  -- 「do の最後の行は、式でなければなりません」
  _ -> Left "the last line of do must be an expression"
  where
    andThen e x body = App (App (Var "bind") e) (Lam x body)
-- ANCHOR_END: desugarDo

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
      ps <- between (symbol "(") (symbol ")") (pattern `sepBy` symbol ",")
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
  , do ts <- between (symbol "(") (symbol ")") (typeExpr `sepBy` symbol ",")
       pure $ case ts of
         [t] -> t
         _   -> TyTuple ts
  ] <?> "a type"
-- ANCHOR_END: dataDecl

-- ANCHOR: regexLiteral
-- 🔎 re"\d{4}-\d\d" is a regular expression. Inside it, a \ is kept as
--    it is, like Python's r"...": \d stays \d. \" is kept too, and
--    does not end the regex. It is sugar for  $regex "\\d{4}-\\d\\d".
--    The pattern is compiled HERE, so a mistake in it is a mistake of
--    the parser, found before the program runs.
-- re"..." は正規表現。中の \ は、Python の r"..." と同じく、そのまま
-- 残す（\d は \d のまま）。\" も残し、正規表現は終わらない。
-- $regex "..." の糖衣構文。パターンはここでコンパイルしてみるので、
-- まちがいはパーサーのエラーになり、動かす前に見つかる
regexLiteral :: Parser Expr
regexLiteral = (<?> "a regex") $ lexeme $ do
  _      <- try (string "re\"")
  start  <- getOffset
  source <- T.concat <$> manyTill rawChar (char '"')
  case compileRegex source of
    Right _  -> pure (App (Var "$regex") (Lit (LString source)))
    Left msg -> do
      setOffset start
      -- 「この正規表現は、コンパイルできません」
      fail (T.unpack ("this regex does not compile: " <> msg))
  where
    rawChar = choice
      [ do _ <- char '\\'
           c <- anySingleBut '\n'
           pure (T.pack ['\\', c])
      , T.singleton <$> noneOf ['"', '\n'] ]
-- ANCHOR_END: regexLiteral

-- ANCHOR: matchRegex
-- 🐪 m/\d+/i  is  re"(?i)\d+": the regex between the two /, and then
--    the flags (i: ignore case, m: ^ and $ at each line, s: . takes a
--    line break too, x: spaces and # comments in the pattern are
--    left out). As in re"...", a \ is kept; \/ is the letter /.
--    It is a regex only when / comes right after m and a second /
--    closes it on the same line: m / 2 and m/2 are still divisions.
--    Instead of /, the pattern may be put between ! # or %, letters
--    that mean nothing else in Lam: m!/usr/bin! needs no \/.
--    (Not ( [ { < , or |: m(x), m[1], m{a: 1}, m<2 mean something.)
-- m/…/ のあとにフラグ。re"…" と同じく \ はそのまま（\/ は / の文字）。
-- m のすぐ後ろに / があり、同じ行にもう1つ / があるときだけ正規表現。
-- m / 2 や m/2 は、いままでどおり割り算。/ のかわりに、Lam でほかの
-- 意味のない ! # % でもはさめる（m!/usr/bin! なら \/ が要らない）。
-- ( [ { < , | は使わない：m(x)・m[1]・m{a: 1}・m<2 には意味がある
matchRegex :: Parser Expr
matchRegex = (<?> "a regex") $ lexeme $ do
  (start, body) <- try $ do
    _ <- char 'm'
    d <- oneOf delimiters
    (,) <$> getOffset <*> upTo d
  flags <- regexFlags "imsx"
  regex <- compiledAt start (withFlags flags body)
  pure (regexValue regex)

-- The letters that may stand where / does.
-- / のかわりに使える文字
delimiters :: String
delimiters = "/!#%"

-- The text up to the next d (/ or another delimiter). A \ keeps the
-- letter after it.
-- 次の d（/ か、ほかの区切り）までの文字。\ は、後ろの1文字と一緒に残す
upTo :: Char -> Parser Text
upTo d = T.concat <$> manyTill piece (char d)
  where
    piece = choice
      [ do _ <- char '\\'
           c <- anySingleBut '\n'
           pure (T.pack ['\\', c])
      , T.singleton <$> noneOf [d, '\n'] ]

-- 🚩 The flags after the last /. A letter that is not a flag is an
--    error (m/x/q), not the start of a name.
-- 最後の / のあとのフラグ。フラグでない文字はエラー
regexFlags :: String -> Parser String
regexFlags allowed = do
  flags <- many (oneOf allowed)
  o <- getOffset
  bad <- optional (lookAhead alphaNumChar)
  case bad of
    Nothing -> pure flags
    Just c  -> do
      setOffset o
      -- 「知らないフラグです」
      fail ("unknown flag " <> show c <> " (use " <> allowed <> ")")

-- (?i) and so on, in front of the pattern.
-- (?i) などを、パターンの前に付ける
withFlags :: String -> Text -> Text
withFlags flags body
  | null flags = body
  | otherwise  = "(?" <> T.pack flags <> ")" <> body

-- Compile the pattern here, so that a mistake is found before the
-- program runs; the error points at the start of the pattern.
-- ここでコンパイルしてみる。まちがいは、パターンの頭を指すエラーに
compiledAt :: Int -> Text -> Parser Regex
compiledAt start source = case compileRegex source of
  Right regex -> pure regex
  Left msg    -> do
    setOffset start
    fail (T.unpack ("this regex does not compile: " <> msg))

-- The regex as an expression:  $regex "...".
-- 式としての正規表現
regexValue :: Regex -> Expr
regexValue regex = App (Var "$regex") (Lit (LString (regexSource regex)))
-- ANCHOR_END: matchRegex

-- ANCHOR: substRegex
-- 🐪 s/pattern/replacement/flags is a function String -> String:
--      s/\d+/#/g    is  Regex.replaceAll re"\d+" "#"
--      s/\d+/#/     is  Regex.replace re"\d+" "#"   (only the first)
--    In the replacement, {last} is the group named last, and {1} the
--    group 1, as in f"...": with them, it opens into Regex.replaceWith
--    (or replaceFirstWith) and a function. The groups are known here,
--    so a group that is not in the regex is an error of the parser.
--    {{ and }} are the letters { and }; \/ is /, \n a new line.
--    ! # or % may stand for the three /, as in m: s#/usr#/opt#.
-- s/パターン/置き換え/フラグ は String -> String の関数。置き換えの中の
-- {last} は last という名前のグループ、{1} は1番のグループ（f"…" と同じ
-- 書き方）。そのときは replaceWith と関数に開く。グループはここでわかる
-- ので、正規表現にないグループはパーサーのエラーになる。
-- m と同じく、3つの / のかわりに ! # % も使える（s#/usr#/opt#）
substRegex :: Parser Expr
substRegex = (<?> "a substitution") $ lexeme $ do
  (start, body, pieces) <- try $ do
    _ <- char 's'
    d <- oneOf delimiters
    o <- getOffset
    b <- upTo d
    ps <- manyTill (replacementPiece d) (char d)
    pure (o, b, ps)
  flags <- regexFlags "gimsx"
  regex <- compiledAt start (withFlags (filter (/= 'g') flags) body)
  mapM_ (checkGroup regex) [ (o, ref) | Right (o, ref) <- pieces ]
  let global = 'g' `elem` flags
      plain  = T.pack [ c | Left c <- pieces ]
      found  = Var "$m"
      piece  = either Left (\(_, ref) ->
                 Right (call "$group" [Lit (LString ref), found]))
  pure $ if all isLeftPiece pieces
    then call (if global then "Regex.replaceAll" else "Regex.replace")
              [regexValue regex, Lit (LString plain)]
    else call (if global then "Regex.replaceWith"
                         else "Regex.replaceFirstWith")
              [regexValue regex, Lam "$m" (joinPieces (map piece pieces))]
  where
    isLeftPiece = either (const True) (const False)

-- One letter of the replacement, or a group {name} with where it is.
-- d is the delimiter: \/ (or \!) is the letter itself.
-- 置き換えの1文字か、グループ {name}（と、その場所）。d は区切りの文字
replacementPiece :: Char -> Parser (Either Char (Int, Text))
replacementPiece d = choice
  [ Left '{' <$ try (string "{{")
  , Left '}' <$ try (string "}}")
  , do _ <- char '{'
       o <- getOffset
       ref <- takeWhile1P (Just "a group") (\c -> isAlphaNum c || c == '_')
       _ <- char '}'
       pure (Right (o, ref))
  , char '\\' *> choice
      [ Left d    <$ char d
      , Left '/'  <$ char '/'
      , Left '\\' <$ char '\\'
      , Left '\n' <$ char 'n'
      , Left '\t' <$ char 't' ]
  , Left <$> noneOf [d, '\n'] ]

-- {1} must be a group of the regex (0 is the whole), and {last} one of
-- its names.
-- {1} は正規表現のグループ（0 は全体）、{last} はその名前のどれか
checkGroup :: Regex -> (Int, Text) -> Parser ()
checkGroup regex (o, ref)
  | T.all isDigit ref, read (T.unpack ref) <= regexGroups regex = pure ()
  | ref `elem` map fst (regexNames regex) = pure ()
  | otherwise = do
      setOffset o
      -- 「この正規表現に、そのグループはありません」
      fail (T.unpack ("this regex has no group " <> ref))
-- ANCHOR_END: substRegex

-- ANCHOR: formatString
-- 🪄 f"Hello, {name}! You are {age:>3}."  is sugar for
--      "Hello, " ++ display name ++ "! You are "
--        ++ padLeft 3 (display age) ++ "."
--    The evaluator and the type checker never see f"...".
-- f-string は糖衣構文。++ と、ふつうの関数の呼び出しに開く。
-- 評価器も型検査器も、f"..." を知らない
formatString :: Parser Expr
formatString = formatStringWith field

-- 🪄 The same, with the parser of a field given (for the training).
-- 同じだが、フィールドのパーサーを渡す（トレーニングで使う）
formatStringWith :: Parser Expr -> Parser Expr
formatStringWith oneField = (<?> "an f-string") $ lexeme $ do
  _      <- try (string "f\"")
  pieces <- manyTill piece (char '"')
  pure (joinPieces pieces)
  where
    piece = choice
      [ Left '{' <$ try (string "{{")       -- {{ is the letter {
      , Left '}' <$ try (string "}}")       -- }} is the letter }
      , Right <$> oneField
      , do o <- getOffset
           _ <- char '}'
           setOffset o
           -- 「} だけを書くときは、}} と書きます」
           fail "a single } must be written }}"
      , Left <$> stringChar ]

-- 🧩 The pieces, side by side, joined with ++. The letters next to
--    each other become one string. A field that is itself a ++ (as
--    {x=} is: "x=" ++ show x) is opened, so that its string joins
--    the letters around it.
-- 部品を ++ でつなぐ。並んだ文字は、1つの文字列にまとめる。
-- それ自身が ++ のフィールド（{x=} は "x=" ++ show x）は開いて、
-- その文字列も、まわりの文字とまとめる
joinPieces :: [Either Char Expr] -> Expr
joinPieces pieces = case foldr add [] pieces of
  []    -> Lit (LString "")
  parts -> foldr1 (BinOp Concat) parts
  where
    add piece rest = case (piece, rest) of
      (Right (BinOp Concat a b), _) -> add (Right a) (add (Right b) rest)
      (Left c, Lit (LString s) : more) -> Lit (LString (T.cons c s)) : more
      (Left c, _) -> Lit (LString (T.singleton c)) : rest
      (Right (Lit (LString s)), Lit (LString t) : more) ->
        Lit (LString (s <> t)) : more
      (Right e, _) -> e : rest
-- ANCHOR_END: formatString

-- ANCHOR: field
-- 🎯 One field:  {expr}, {expr=}, {expr:formatSpec}, {expr=:formatSpec}.
--    {x=} shows the expression too, as written:  x=42.
-- フィールド1つ。{x=} は、書いたとおりの式も見せる
field :: Parser Expr
field = do
  _ <- char '{'
  (source, e) <- match (spaces *> formatExpr)
  debug <- optional (char '=' *> takeWhileP Nothing (== ' '))
  format <- optional (char ':' *> formatSpec)
  _ <- char '}' <?> "}"
  let shown = case (debug, format) of
        -- {x=} with no format: the value as the REPL shows it, like
        -- Python's repr. With a format, the value for people.
        -- {x=} で書式がなければ、REPL と同じ形（Python の repr と同じ）
        (Just _, Nothing) -> call "show" [e]
        _                 -> applySpec (fromMaybe noSpec format) e
  pure $ case debug of
    Nothing     -> shown
    Just spaced ->
      BinOp Concat (Lit (LString (source <> "=" <> spaced))) shown

-- 🌳 The expression in a field: \, let, if, case and do must be in
--    parentheses (they would reach over the : and the }).
-- フィールドの式。\、let、if、case、do は括弧で包む（: や } まで
-- 飲みこんでしまうため）
formatExpr :: Parser Expr
formatExpr = do
  o <- getOffset
  wide <- optional (lookAhead (symbol "\\" <|> choice (map ((<$) "" . keyword)
            ["let", "if", "case", "do"])))
  case wide of
    Just _  -> do
      setOffset o
      -- 「{ } の中では、\ let if case do を ( ) で包みます」
      fail "inside { }, put \\, let, if, case and do in ( )"
    Nothing -> makeExprParser application formatOperators
-- ANCHOR_END: field

-- ANCHOR: formatSpec
-- 📐 A format, a small part of Python's:  [< > ^][0][width][,]
--      <8   to the left, in 8 letters     >8   to the right
--      ^8   in the middle                 05   0 in front of a number
--      ,    a comma every 3 digits        >12, both
-- 書式。Python の書式の小さな一部
data Align = ToLeft | ToRight | ToCenter
  deriving (Eq, Show)

data FormatSpec = FormatSpec
  { specAlign :: Maybe Align
  , specZero  :: Bool
  , specWidth :: Maybe Integer
  , specComma :: Bool
  }
  deriving (Eq, Show)

noSpec :: FormatSpec
noSpec = FormatSpec Nothing False Nothing False

-- 📐 Read a format. A mistake is an error here, before Lam runs.
-- 書式を読む。まちがいは、Lam が動く前に、ここでエラーになる
formatSpec :: Parser FormatSpec
formatSpec = do
  -- 「書式は : のすぐ後ろに書きます（リストの : は ( ) で包みます）」
  _     <- notFollowedBy (char ' ') <|> fail
             "a format comes right after : (put the : of lists in ( ))"
  o1    <- getOffset
  align <- optional (choice [ ToLeft   <$ char '<'
                            , ToRight  <$ char '>'
                            , ToCenter <$ char '^' ])
  o2    <- getOffset
  zero  <- isJust <$> optional (char '0')
  o3    <- getOffset
  width <- optional (L.decimal <?> "a width")
  o4    <- getOffset
  comma <- isJust <$> optional (char ',')
  let failAt o msg = setOffset o >> fail msg
  case (align, zero, width) of
    -- 「< の後ろには、幅が要ります」
    (Just _, _, Nothing)      -> failAt o2 "a width must follow < > or ^"
    -- 「0 は、< > ^ と一緒には使えません」
    (Just _, True, _)         -> failAt o2 "0 cannot come after < > or ^"
    -- 「0 の後ろには、幅が要ります」
    (Nothing, True, Nothing)  -> failAt o3 "a width must follow 0"
    -- 「幅の前に、< > ^ のどれか（数なら 0）が要ります」
    (Nothing, False, Just _)  ->
      failAt o1 "a width needs < > or ^ before it (or 0, for a number)"
    _ | zero && comma         -> failAt o4 "0 and , cannot be used together"
      | otherwise             -> pure (FormatSpec align zero width comma)

-- 🍬 The function calls a format opens into: first the value as text,
--    then the text lined up.
--      {n:,}    groupDigits n         {n:05}  zeroPad 5 n
--      {x:>8}   padLeft 8 (display x)
-- 書式を、関数の呼び出しに開く。まず値を文字にし、次に桁をそろえる
applySpec :: FormatSpec -> Expr -> Expr
applySpec s e = pad s (textOf s e)

-- 🔢 The value as text:  groupDigits n,  zeroPad 5 n,  or  display x.
-- 値を文字にする
textOf :: FormatSpec -> Expr -> Expr
textOf (FormatSpec _ zero width comma) e
  | comma     = call "groupDigits" [e]
  | zero      = call "zeroPad" [widthOf width, e]
  | otherwise = call "display" [e]

-- 📏 Line the text up:  > is padLeft (the spaces go on the left).
-- 文字の桁をそろえる。> は padLeft（空白を左に足す）
pad :: FormatSpec -> Expr -> Expr
pad (FormatSpec align _ width _) text = case align of
  Just ToLeft   -> call "padRight" [widthOf width, text]
  Just ToRight  -> call "padLeft"  [widthOf width, text]
  Just ToCenter -> call "center"   [widthOf width, text]
  Nothing       -> text

widthOf :: Maybe Integer -> Expr
widthOf = Lit . LInt . fromMaybe 0

-- 📞 f a b, as a tree.
-- f a b を木にする
call :: Name -> [Expr] -> Expr
call f = foldl App (Var f)
-- ANCHOR_END: formatSpec

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
declaration :: Parser Decl
declaration = choice
  [ DefData <$> dataDecl
  , try (uncurry DefValue <$> definition <* notFollowedBy (keyword "in")) ]
-- ANCHOR_END: program
-- ANCHOR_END: run
-- ANCHOR_END: all
