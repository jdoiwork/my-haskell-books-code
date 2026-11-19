{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Read Lam source code into an AST, with megaparsec.
-- Lam のソースコードを、megaparsec で AST に読む。
module Lam.Parser
  ( Parser
  , Context (..)
  , runLamParser
  , parseExpr
  , parseLam
  , expr
  , definition
  , topDefinition
  , signature
  , signatureHead
  , aliasDecl
  , classDecl
  , instanceDecl
  , Binding (..)
  , letBinding
  , program
  , shebang
  , here
  , atom
  , mapLiteral
  , qualifiedName
  , qualifiedCon
  , moduleHeader
  , importLine
  , exposingList
  , fmtName
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
  , infixDecl
  , programHead
  , fixitiesOf
  , operatorText
  , opLetters
  , spaces
  , symbol
  , keyword
  , keywords
  , formatString
  , doExpr
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
  ) where

import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
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
type Parser = ParsecT Void Text (Reader Context)

-- 📚 What a parser can read (ask): the column, and the operators of
--    the file (its own, and the ones it imported).
-- パーサーが読めるもの：列と、ファイルの演算子（自分のものと、import
-- したもの）
data Context = Context
  { contextColumn    :: Pos
  , contextOperators :: Operators
  }

-- ▶️ Run a parser with these operators. At first, the column is 1:
--    a word at column 1 starts a new definition.
-- この演算子でパーサーを動かす。はじめの列は 1：1桁目の言葉は新しい
-- 定義の始まり
runLamParser
  :: Operators -> Parser a -> FilePath -> Text
  -> Either (ParseErrorBundle Text Void) a
runLamParser ops p file source =
  runReader (runParserT p file source) (Context pos1 ops)
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
  , "type", "where"
  -- ANCHOR_END: keywordsRecords
  -- ANCHOR: keywordsClasses
  , "class", "instance"
  -- ANCHOR_END: keywordsClasses
  -- ANCHOR: keywordsModules
  , "module", "import"
  -- ANCHOR_END: keywordsModules
  -- ANCHOR: keywordsDo
  , "do", "ado"
  -- ANCHOR_END: keywordsDo
  -- ANCHOR: keywordsDefer
  , "defer"
  -- ANCHOR_END: keywordsDefer
  -- ANCHOR: keywordsInfix
  , "infixl", "infixr", "infix" ]
  -- ANCHOR_END: keywordsInfix
-- ANCHOR_END: keywordList

-- ANCHOR: keyword
-- 🔑 A keyword, not followed by more letters ("in" but not "inside").
-- キーワード。後ろに文字が続かない（"in" だが "inside" ではない）
--    try: if "if" is the start of "iffy", read nothing, so that the
--    name iffy can be read next.
-- try：「if」が「iffy」の頭なら、何も読まなかったことにする。
-- そうすれば、次に名前の iffy を読める
keyword :: Text -> Parser ()
keyword w =
  lexeme (try (string w *> notFollowedBy (alphaNumChar <|> char '_')))
-- ANCHOR_END: keyword

-- 🏷️ A name: a small letter, then letters, digits, _ or '.
-- 名前：小文字で始まり、文字・数字・_・' が続く
name :: Parser Name
name = (<?> "a name") $ lexeme $ try rawName

-- 🏷️ A name, with no spaces after it. An f right before " is not
--    one: it is the start of f"...".
-- 名前（後ろの空白は読まない）。" のすぐ前の f は名前ではなく、
-- f"..." の始まり
rawName :: Parser Name
rawName = do
  -- ANCHOR: notFormat
  notFollowedBy (string "f\"")
  -- ANCHOR_END: notFormat
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

-- ANCHOR: number
-- 🔢 A number: 42 is a whole number; 1.5, 1e3 and 2.5e-3 are Floats
--    (a point with digits after it, or an e).
-- 数。42 は整数。1.5、1e3、2.5e-3 は Float（点と後ろの数字か、e がある）
number :: Parser Literal
number = (<?> "a number") $ lexeme $ do
  whole    <- some digitChar
  fraction <- optional (try (char '.' *> some digitChar))
  power    <- optional (try (char 'e' *> powerOfTen))
  pure $ case (fraction, power) of
    (Nothing, Nothing) -> LInt (read whole)
    _ -> LFloat (read (whole <> "." <> fromMaybe "0" fraction
                             <> "e" <> show (fromMaybe 0 power)))
  where
    -- e3, e+3, e-3
    powerOfTen :: Parser Integer
    powerOfTen = do
      sign <- optional (char '+' <|> char '-')
      n    <- L.decimal
      pure (if sign == Just '-' then negate n else n)
-- ANCHOR_END: number

-- ANCHOR: negative
-- ➖ A minus right before a value, with no space between: -7, -1.5,
--    -x, -(a + b).  -7 and -1.5 are numbers; -x is  negate x.
--    "- 7" (with a space) is not this: that minus is the one of a - b.
-- 値の直前の、空白をはさまないマイナス。-7 と -1.5 は数、-x は negate x。
-- 空白をはさむ「- 7」はこれではない。a - b のマイナス
negative :: Parser Expr
negative = (<?> "a negative number") $ do
  _ <- try (char '-' <* lookAhead (satisfy startsValue))
  e <- atom
  pure $ case e of
    Lit (LInt n)   -> Lit (LInt (negate n))
    Lit (LFloat x) -> Lit (LFloat (negate x))
    _              -> App (Var "negate") e

-- The letters a value can start with, right after a minus.
-- マイナスのすぐ後ろで、値が始まれる文字
startsValue :: Char -> Bool
startsValue c = isDigit c || isAlpha c || c == '_' || c == '('
-- ANCHOR_END: negative

-- ANCHOR: qualifiedName
-- 🏷️ A name with Float. in front, with no spaces:  Float.isNaN.
--    A capital letter, a dot, then a small letter: a constructor has
--    no fields, so this cannot be  Float  and a field  isNaN.
--    (The chapter on modules makes Float a real module.)
-- 前に Float. が付いた名前（空白なし）。大文字・点・小文字。構成子は
-- フィールドを持たないので、Float とフィールドの isNaN とは読まない
-- （モジュールの章で、Float を本物のモジュールにする）
qualifiedName :: Parser Name
qualifiedName = (<?> "a name") $ lexeme $ try $ do
  first <- upperChar
  rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
  _     <- char '.'
  x     <- rawName
  pure (T.pack (first : rest) <> "." <> x)
-- ANCHOR_END: qualifiedName

-- ANCHOR: qualifiedCon
-- 🏷️ A constructor with a module in front: Json.JInt, S.Circle.
--    Like qualifiedName, with a capital letter after the dot.
-- 前にモジュールの名前が付いた構成子。qualifiedName と同じ形で、
-- 点の後ろが大文字
qualifiedCon :: Parser Name
qualifiedCon = (<?> "a constructor") $ lexeme $ try $ do
  first <- upperChar
  rest  <- many (alphaNumChar <|> char '_')
  _     <- char '.'
  big   <- upperChar
  more  <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure (T.pack (first : rest <> ('.' : big : more)))
-- ANCHOR_END: qualifiedCon
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
--    something in ( ) (an expression, a tuple, an operator, a
--    section), or a list (or a list comprehension).
-- いちばん小さな部品：数、true・false、文字列、名前、( ) の中身（式、
-- タプル、演算子、セクション）、リスト（とリスト内包表記）
-- <?> gives a part a name for error messages: "expecting a value".
-- <?> は、エラーの文で使う部品の名前：「値が来るはず」
-- ANCHOR: atomOnly
atom :: Parser Expr
atom = choice
  [ Lit <$> number
  , negative
  , Lit (LBool True)  <$ keyword "true"
  , Lit (LBool False) <$ keyword "false"
  , Lit . LString <$> stringLiteral
  , formatString
  , nameWithFields
  , accessor
  , Var <$> qualifiedName
  , Var <$> qualifiedCon
  , Var <$> conName
  , parenthesized
  , listOrComprehension
  , recordLiteral
  , mapLiteral
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
  -- ANCHOR: unitExpr
  -- () is the tuple of nothing: the answer of putStrLn.
  -- () は何も入っていないタプル。putStrLn の答え
  inside <- (Tuple [] <$ char ')') <|> operatorFirst <|> expressionFirst
  -- ANCHOR_END: unitExpr
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
      -- ANCHOR: minusInParens
      -- (-1) is the number -1, and (-x) is negate x: not sections.
      -- (- 1), with a space, is still the section \x -> x - 1.
      -- (-1) は数の -1、(-x) は negate x で、セクションではない。
      -- 空白のある (- 1) は、これまでどおりセクション
      notFollowedBy (char '-' *> satisfy startsValue)
      -- ANCHOR_END: minusInParens
      (_, o) <- bareOperator
      choice
        [ operatorFunction o <$ try (spaces *> char ')')
        , do
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
  _ <- try (symbol "{" <* lookAhead (hidden name *> afterName))
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
    -- Named for error messages: {name} says "'=' (a record)".
    -- エラーの文のための名前：{name} なら「'='（レコード）」
    afterName = (bar <|> equals) <?> "'=' (a record)"
    recordField = do
      x <- name
      _ <- equals
      e <- expr
      pure (x, e)
-- ANCHOR_END: recordLiteral

-- ANCHOR: mapLiteral
-- 🗺️ {"Python": 82, "TS": 75}  is sugar for
--    Map.fromList [("Python", 82), ("TS", 75)].  {} is the empty map.
--    A key is a string or a number written as it is, like JSON and
--    Python: {1: "one"}. When a string key looks like a name, the
--    quotes may be left out, like JavaScript: {name: "Lam"} is
--    {"name": "Lam"} (name is NOT a variable here). To use the value
--    of an expression as the key, put it in [ ]: {[name]: "Lam"} uses
--    the value of the variable name, and {[(1, 2)]: "a"} a tuple.
-- Map.fromList の糖衣構文。キーは、JSON や Python と同じく、そのまま
-- 書いた文字列か数。名前の形の文字列なら、JavaScript と同じく " を
-- 省ける。{name: "Lam"} は {"name": "Lam"} のこと（ここの name は
-- 変数ではない）。式の値をキーにするときは、[ ] で包む
mapLiteral :: Parser Expr
mapLiteral = do
  entries <- between (symbol "{") (symbol "}") (entry `sepBy` symbol ",")
  pure (App (Var "Map.fromList")
            (List [ Tuple [k, v] | (k, v) <- entries ]))
  where
    entry = do
      k <- choice
        [ Lit . LString <$> stringLiteral
        , numberKey
        , Lit . LString <$> bareKey
        , between (symbol "[") (symbol "]") expr ]
      -- Named for error messages: "':' (a Map)".
      -- エラーの文のための名前：「':'（Map）」
      _ <- symbol ":" <?> "':' (a Map)"
      v <- expr
      pure (k, v)
    -- 1, 1.5 and -1, with no space after the minus.
    -- 空白のないマイナスも
    numberKey = do
      minus <- optional (char '-')
      n <- number
      pure $ Lit $ case (minus, n) of
        (Just _, LInt i)   -> LInt (negate i)
        (Just _, LFloat x) -> LFloat (negate x)
        _                  -> n
    -- A key without quotes: letters, digits, _ and ', not starting
    -- with a digit. Keywords are fine here ({if: 1}).
    -- " のないキー：文字・数字・_・'（数字では始まらない）。
    -- ここでは、キーワードも使える
    bareKey = (<?> "a key") $ lexeme $ do
      first <- letterChar <|> char '_'
      -- hidden: the letters that may go on are not listed in errors.
      -- hidden：続けてよい文字は、エラーの文に並べない
      rest  <- many (hidden (alphaNumChar <|> char '_' <|> char '\''))
      pure (T.pack (first : rest))
-- ANCHOR_END: mapLiteral

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
bareOperator = do
  o  <- getOffset
  s  <- operatorText
  ops <- asks contextOperators
  case Map.lookup s ops of
    Just fixity -> pure (s, operatorOf fixity)
    Nothing     -> (,) s . operatorOf <$> unknownOperatorAt o s
-- ANCHOR_END: operatorSymbol

-- ANCHOR: application
-- 📞 Calling a function: atoms side by side.
--    f x y means (f x) y.
-- 関数を呼ぶ：部品を並べる。f x y は (f x) y
--    A call gets the mark of its place (At), for the errors.
-- 呼び出しには、エラーのために、場所の印（At）を付ける
application :: Parser Expr
application = do
  loc     <- here
  (f, sp) <- withSpaceAfter atom
  args    <- arguments sp
  pure $ case args of
    [] -> f                          -- only an atom: no call, no mark
    _  -> At loc (foldl App f args)
  where
    -- ANCHOR: arguments
    -- The arguments, one by one. A minus right before a value is a
    -- negative argument only after a space: f -7 is f (-7), but in
    -- n-1 (no space before -) the minus is the one of n - 1.
    -- 引数を1つずつ。値の直前のマイナスが負の引数になるのは、前に空白が
    -- あるときだけ。f -7 は f (-7)。n-1（- の前に空白なし）は n - 1
    arguments afterSpace = do
      next <- optional (continuing *> withSpaceAfter (argument afterSpace))
      case next of
        Nothing        -> pure []
        Just (a, sp')  -> (a :) <$> arguments sp'
    argument afterSpace
      | afterSpace = atom
      | otherwise  = notFollowedBy (char '-') *> atom
    -- ANCHOR_END: arguments

-- 📏 Read p, and tell if it ended with a space (or a line break, or a
--    comment): the spaces after it are read by lexeme.
-- p を読み、空白（改行、コメント）で終わったかを答える。後ろの空白は
-- lexeme が読んでいる
withSpaceAfter :: Parser a -> Parser (a, Bool)
withSpaceAfter p = do
  input  <- getInput
  start  <- getOffset
  a      <- p
  end    <- getOffset
  let used = T.take (end - start) input
  pure (a, not (T.null used) && isSpace (T.last used))
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
  limit  <- asks contextColumn
  if column > limit then pure () else empty
-- ANCHOR_END: continuing
-- ANCHOR_END: atom

-- ANCHOR: operators
-- ANCHOR: opLetters
-- 🔣 The letters that operators are made of. An operator is the
--    longest run of them: <+> is one operator, not < and +>.
-- 演算子を作る文字。演算子は、この文字が続くかぎりの長さ。<+> は
-- 1つの演算子で、< と +> ではない
opLetters :: String
opLetters = "!$%&*+-/:<=>?^|~"

operatorText :: Parser Text
operatorText = takeWhile1P (Just "an operator") (`elem` opLetters)
-- ANCHOR_END: opLetters

-- ANCHOR: reserved
-- 🚫 Runs of operator letters that belong to the language itself.
--    They end an expression: = in let, | and -> in case, <- in a
--    comprehension and a do, ? at the end of a line of a do. No file
--    can declare them.
-- 言語そのものが使う、演算子の文字の並び。式はここで終わる（let の
-- =、case の | と ->、内包表記と do の <-、do の行の終わりの ?）。
-- どのファイルも宣言できない
reservedSymbols :: [Text]
reservedSymbols = ["=", "|", "->", "<-", "?"]
-- ANCHOR_END: reserved

-- 🧮 Calls and operators together, with all the operators the file
--    knows.
-- 関数の呼び出しと、演算子。ファイルが知っている演算子を全部使う
operatorExpr :: Parser Expr
operatorExpr = operatorExprWith (const True)

-- ANCHOR: formatOperators
-- 🪡 Inside { } of f"...", a : ends the expression (the format comes
--    after it), so the operators that start with : are left out there.
--    Write (x : xs) in parentheses, as Python wants lambda in them.
-- f"..." の { } の中では、: で式が終わる（後ろに書式が来る）ので、
-- : で始まる演算子は外す。Python の lambda と同じく、括弧で包む
formatOperators :: Text -> Bool
formatOperators s = not (":" `T.isPrefixOf` s)
-- ANCHOR_END: formatOperators

-- ANCHOR: operatorLevels
-- 🪜 Read the operators level by level, from the loosest (0) to the
--    tightest (9): each level is a chain  x op y op z  whose parts
--    are made of the tighter levels. The operators of a chain must
--    lean the same way; a NoneA one (==) is never chained.
-- 演算子を、弱い段（0）から強い段（9）へ読む。どの段も
-- x op y op z の鎖で、部品はもっと強い段でできている。鎖の演算子は、
-- 同じ向きに寄っていないといけない。NoneA のもの（==）は鎖にならない
operatorExprWith :: (Text -> Bool) -> Parser Expr
operatorExprWith usable = do
  ops <- Map.filterWithKey (\s _ -> usable s) <$> asks contextOperators
  foldl (level ops) application [9, 8 .. 0 :: Int]
  where
    level ops tighter n = do
      first <- tighter
      rest  <- many ((,) <$> operatorAt ops n <*> tighter)
      chain first rest
    -- One operator of level n. An operator followed by ) is not
    -- read here: that is a section (10 -). One nobody declared is
    -- read at level 9, after noting the error (see unknownOperatorAt).
    -- The letters of the language (=, ->, ...) and the ones left out
    -- (: in f"...") end the expression.
    -- 段 n の演算子を1つ。) が続くものは読まない（セクションの (10 -)）。
    -- 誰も宣言していないものは、エラーを書き留めてから、段 9 で読む。
    -- 言語の文字（=、-> など）と、外したもの（f"..." の :）は、式の終わり
    -- ANCHOR: operatorPlace
    -- The operator keeps the place of its symbol (loc): the error of
    -- 10 // 0 points at the //.
    -- 演算子は、記号の場所（loc）を持っておく。10 // 0 のエラーは // を指す
    operatorAt ops n = try $ do
      _ <- continuing
      loc <- here
      o <- getOffset
      s <- lexeme operatorText
      notFollowedBy (char ')')
      case Map.lookup s ops of
        Just f | fixityLevel f == n -> pure (o, s, f, loc)
        Nothing | n == 9, usable s, s `notElem` reservedSymbols ->
          (\f -> (o, s, f, loc)) <$> unknownOperatorAt o s
        _ -> empty
    -- ANCHOR_END: operatorPlace
-- ANCHOR_END: operatorLevels

-- ANCHOR: chain
-- 🔗 Build the tree of one chain.
-- 鎖を1つ、木にする
chain :: Expr -> [((Int, Text, Fixity, Loc), Expr)] -> Parser Expr
chain first rest = case nub [ fixityAssoc f | ((_, _, f, _), _) <- rest ] of
  []       -> pure first
  [LeftA]  -> pure (foldl (\l (link, r) -> build link l r) first rest)
  [RightA] -> pure (foldRight first rest)
  [NoneA] | [(link, r)] <- rest -> pure (build link first r)
  [NoneA] | _ : ((o, s, _, _), _) : _ <- rest -> do
    -- 「== は続けて書けません。括弧を使ってください」
    noteErrorAt o (T.unpack s <> " cannot be chained (it is infix):"
                   <> " use parentheses")
    pure (foldl (\l (link, r) -> build link l r) first rest)
  _ | ((_, s1, f1, _), _) : more <- rest
    , (o, s) : _ <- [ (o, s) | ((o, s, f, _), _) <- more
                             , fixityAssoc f /= fixityAssoc f1 ] -> do
    -- 「>> と << は、寄る向きがちがうので、括弧なしでは混ぜられません」
    noteErrorAt o ("cannot mix " <> T.unpack s1 <> " and " <> T.unpack s
                   <> " without parentheses (they lean different ways)")
    pure (foldl (\l (link, r) -> build link l r) first rest)
  _ -> pure first
  where
    -- The tree, with the mark of the place of the symbol.
    -- 木に、記号の場所の印を付ける
    build (_, _, f, loc) l r = let Operator b = operatorOf f in At loc (b l r)
    foldRight x []                 = x
    foldRight x ((link, y) : more) = build link x (foldRight y more)

-- ANCHOR_END: chain

-- ANCHOR: noteError
-- 🚩 An error at an offset of the input: stop here.
-- 入力の、ある場所のエラー。ここで止まる
stopAt :: Int -> String -> Parser a
stopAt o msg = parseError (FancyError o (Set.singleton (ErrorFail msg)))

-- 📝 An error at an offset, noted down: the parser reads on, and says
--    it at the end. Stopping here would not do: a definition is read
--    with try, so its error would be thrown away, and the parser would
--    say something else ("unexpected =").
-- 入力の、ある場所のエラーを書き留める。パーサーは読み続け、最後に
-- 知らせる。ここで止まるのではだめ：定義は try で読むので、そのエラーは
-- 捨てられ、別のこと（「= は予想外」）を言ってしまう
noteErrorAt :: Int -> String -> Parser ()
noteErrorAt o msg =
  registerParseError (FancyError o (Set.singleton (ErrorFail msg)))

-- ❓ An operator nobody declared: note it, and read it on as one at
--    level 9 (it never runs: the file has an error).
-- 誰も宣言していない演算子：書き留めて、段 9 のものとして読み続ける
-- （ファイルにはエラーがあるので、動くことはない）
-- 「演算子 <+> は知りません（infixl の行で宣言するか、import します）」
unknownOperatorAt :: Int -> Text -> Parser Fixity
unknownOperatorAt o s = do
  noteErrorAt o ("unknown operator " <> T.unpack s
                 <> " (declare it with infixl, infixr or infix,"
                 <> " or import it)")
  pure (Fixity LeftA 9 "$unknown" False)
-- ANCHOR_END: noteError
-- ANCHOR_END: operators

-- ANCHOR: expr
-- 🌳 A whole expression. \, let and if reach to the right end.
-- 式全体。\、let、if は右の端まで続く
expr :: Parser Expr
expr = choice [lambda, letIn, ifThenElse, caseOf, doExpr, adoExpr, operatorExpr]

-- ANCHOR: doExpr
-- 📝 do, then lines one under another, all at the same column (or
--    on one line, with ; between them). Inside, an expression goes on
--    only to the right of that column (see continuing).
-- do のあとに、同じ列から行を縦に並べる（1行なら ; で区切る）。中の式は、
-- その列より右にだけ続く（continuing）
doExpr :: Parser Expr
doExpr = do
  keyword "do"
  column <- L.indentLevel
  lines' <- local (\c -> c { contextColumn = column })
              (statement `sepBy1` nextLine column)
  case last lines' of
    DoRun _    -> pure (doBlock lines')
    -- 「do の最後の行は答えそのものなので、? なしで書きます」
    DoTryRun _ ->
      fail "the last line of do is the answer itself: write it without ?"
    -- 「do の最後の行は、式でなければなりません」
    _ -> fail "the last line of do must be an expression"
  where
    nextLine column = (() <$ symbol ";") <|> do
      now <- L.indentLevel
      if now == column then pure () else empty

-- ANCHOR: adoExpr
-- 🧩 ado: lines as in a do, and the last one is pure, then an
--    expression. adoBlock opens it, or says why it cannot.
-- ado：行は do と同じで、最後の行は pure と式。adoBlock が開くか、
-- 開けないわけを言う
adoExpr :: Parser Expr
adoExpr = do
  keyword "ado"
  column <- L.indentLevel
  lines' <- local (\c -> c { contextColumn = column })
              (statement `sepBy1` nextLine column)
  either (fail . T.unpack) pure (adoBlock lines')
  where
    nextLine column = (() <$ symbol ";") <|> do
      now <- L.indentLevel
      if now == column then pure () else empty
-- ANCHOR_END: adoExpr

-- 📝 One line: let x = e,  defer e,  p <- e,  or an expression. A
--    let followed by in is an expression. The last two may end with ?.
-- 1行：let x = e、defer e、p <- e、または式。in が続く let は式。
-- 最後の2つは、? で終わってもよい
statement :: Parser Statement
statement = choice
  [ do binding <- try (letBinding <* notFollowedBy (keyword "in"))
       pure $ case binding of
         Named x e        -> DoLet x e
         Destructured p e -> DoLetPattern p e
  , DoDefer <$> (keyword "defer" *> expr)
  , do p <- try (pattern <* symbol "<-")
       e <- expr
       marked <- tryMark
       pure (if marked then DoTry p e else DoBind p e)
  , do e <- expr
       marked <- tryMark
       pure (if marked then DoTryRun e else DoRun e) ]
-- ANCHOR_END: doExpr

-- ANCHOR: tryMark
-- ❓ A ? at the end of a line of do. It is not ?: (or-else), nor /?.
--    It stands alone, to the right of the column of the do.
-- do の行の終わりの ?。?:（なければ）でも /? でもない。do の列より右に、
-- 1つだけで立つ
tryMark :: Parser Bool
tryMark = option False $ True <$
  try (lexeme (continuing *> char '?' <* notFollowedBy (oneOf opLetters)))
-- ANCHOR_END: tryMark

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
  -- ANCHOR: definedName
  -- Map.insertWith may be defined too (the prelude does).
  -- Map.insertWith も定義できる（プレリュードが使う）
  f      <- qualifiedName <|> name
  -- ANCHOR_END: definedName
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
    f      <- qualifiedName <|> name
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
whereBlock = block afterLet

-- ANCHOR: block
-- 📎 where, then items one under another, all at the same column.
--    A where of a definition, a class and an instance all use this.
-- where のあとに、同じ列から項目を縦に並べる。定義の where も、
-- クラスとインスタンスも、これを使う
block :: Parser a -> Parser [a]
block item = do
  try (continuing *> keyword "where")
  column <- L.indentLevel
  local (\c -> c { contextColumn = column })
        (item `sepBy1` atColumn column)
  where
    atColumn column = do
      now <- L.indentLevel
      if now == column then pure () else empty
-- ANCHOR_END: block
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
  conds <- conditions
  t     <- typeExpr
  pure (DefSig x (Signature conds t))

-- 🎓 The classes before =>, if any:  Comparable a =>  or
--    (Describe a, Comparable b) =>
-- => の前のクラス（あれば）
conditions :: Parser [(Name, Name)]
conditions = option [] (try (classes <* symbol "=>"))
  where
    classes =
      between (symbol "(") (symbol ")") (condition `sepBy1` symbol ",")
        <|> fmap pure condition
    condition = (,) <$> conName <*> name
-- ANCHOR_END: signature

-- ANCHOR: classDecl
-- 🎓 class Describe a where
--      describe : a -> String
--      describeAll : [a] -> String
--      describeAll xs = ...
--    Under where: the types of the methods, and the defaults (written
--    as definitions). A class may have no methods at all.
-- where の下に、メソッドの型と、既定のメソッド（定義の形）。
-- メソッドが1つもないクラスもよい
classDecl :: Parser ClassDecl
classDecl = do
  keyword "class"
  supers <- conditions
  c      <- conName
  a      <- name
  items  <- option [] (block item)
  pure (ClassDecl supers c a
          [ (m, sig) | Left (m, sig) <- items ]
          [ (m, e)   | Right (m, e)  <- items ])
  where
    -- A method's type has a : after its name; a default has its
    -- arguments and = instead.
    -- メソッドの型は名前のあとに :、既定のメソッドは引数と =
    item = (Left <$> try methodType) <|> (Right <$> afterLet)
    methodType = do
      m <- name
      _ <- lexeme (char ':' <* notFollowedBy (oneOf opLetters))
      t <- typeExpr
      pure (m, Signature [] t)

-- 🧩 instance Describe Shape where
--      describe s = ...
--    instance Describe a => Describe (Maybe a) where ...
--    The type is one word (Int, Shape) or in brackets: (Maybe a), [a].
-- 型は1語（Int、Shape）か、括弧の中：(Maybe a)、[a]
instanceDecl :: Parser InstanceDecl
instanceDecl = do
  keyword "instance"
  context <- conditions
  c       <- conName
  t       <- typeAtom
  methods <- option [] (block afterLet)
  pure (InstanceDecl context c t methods)
-- ANCHOR_END: classDecl

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
  loc <- here
  keyword "case"
  scrutinee <- expr
  keyword "of"
  At loc <$> branchesOf scrutinee

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
      lexeme (try (char ':' *> notFollowedBy (oneOf opLetters)))

-- ANCHOR: patternApp
-- 🧩 A constructor with its patterns (Just x, Rect w h), or a small one.
--    The constructor may have a module in front: S.Circle r.
-- 構成子と、その引数のパターン。または小さなパターン。構成子の前には
-- モジュールの名前が付いてもよい
patternApp :: Parser Pattern
patternApp =
  (PCon <$> constructorName <*> many (continuing *> patternAtom))
    <|> patternAtom
  where
    constructorName = qualifiedCon <|> conName
-- ANCHOR_END: patternApp

patternAtom :: Parser Pattern
patternAtom = choice
  [ PWild <$ lexeme (try (char '_' <* notFollowedBy nameChar))
  , PVar <$> name
  , (\c -> PCon c []) <$> (qualifiedCon <|> conName)
  , PLit . LInt <$> integer
  -- -1: a negative number (no space after the minus)
  -- 負の数（マイナスのあとに空白なし）
  , PLit . LInt . negate
      <$> (try (char '-' <* lookAhead digitChar) *> integer)
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
      -- () too: sepBy, not sepBy1 / () も読む
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
typeApp = choice
  [ TyCon <$> conName <*> many (continuing *> typeAtom)
  -- ANCHOR: typeVarApp
  -- f a: a type variable with types after it (f is a hole, like List)
  -- f a：型変数のあとに型が続く（f は List のような穴）
  , do f    <- name
       args <- many (continuing *> typeAtom)
       pure (if null args then TyVar f else TyApp f args)
  -- ANCHOR_END: typeVarApp
  , typeAtom ]

typeAtom :: Parser TypeExpr
typeAtom = choice
  [ (\c -> TyCon c []) <$> conName
  , TyVar <$> name
  , TyList <$> between (symbol "[") (symbol "]") typeExpr
  -- (a, b), and () (sepBy, not sepBy1) / () も読む
  , do ts <- between (symbol "(") (symbol ")") (typeExpr `sepBy` symbol ",")
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
  debug  <- optional (char '=' *> takeWhileP Nothing (== ' '))
  format <- optional (char ':' *> formatSpec)
  closeField
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

-- 🚪 The } at the end of a field. A letter there is most likely the
--    type letter of Python (.1f, 5d): Lam has none.
-- フィールドの終わりの }。そこに文字があれば、たぶん Python の型の
-- 文字（.1f、5d）。Lam にはない
closeField :: Parser ()
closeField = do
  o <- getOffset
  letter <- optional (lookAhead (satisfy isAlpha))
  case letter of
    Just c -> do
      setOffset o
      -- 「書式に型の文字はありません（.1f ではなく .1 と書きます）」
      fail ("a format has no type letter like " <> [c]
            <> " (write {x:.1}, not {x:.1f})")
    Nothing -> () <$ (char '}' <?> "}")

-- 🌳 The expression in a field: \, let, if and case must be in
--    parentheses (they would reach over the : and the }).
-- フィールドの式。\、let、if、case は括弧で包む（: や } まで
-- 飲みこんでしまうため）
formatExpr :: Parser Expr
formatExpr = do
  o <- getOffset
  wide <- optional (lookAhead (symbol "\\" <|> choice (map ((<$) "" . keyword)
            ["let", "if", "case"])))
  case wide of
    Just _  -> do
      setOffset o
      -- 「{ } の中では、\ let if case を ( ) で包みます」
      fail "inside { }, put \\, let, if and case in ( )"
    Nothing -> operatorExprWith formatOperators
-- ANCHOR_END: field

-- ANCHOR: formatSpec
-- 📐 A format, a small part of Python's:  [< > ^][0][width][,][.digits]
--      <8   to the left, in 8 letters     >8   to the right
--      ^8   in the middle                 05   0 in front of a number
--      ,    a comma every 3 digits        .2   2 digits after the point
--      >12,.2   all of them
-- 書式。Python の書式の小さな一部
data Align = ToLeft | ToRight | ToCenter
  deriving (Eq, Show)

data FormatSpec = FormatSpec
  { specAlign  :: Maybe Align
  , specZero   :: Bool
  , specWidth  :: Maybe Integer
  , specComma  :: Bool
  , specDigits :: Maybe Integer
  }
  deriving (Eq, Show)

noSpec :: FormatSpec
noSpec = FormatSpec Nothing False Nothing False Nothing

-- 📐 Read a format. A mistake is an error here, before Lam runs.
-- 書式を読む。まちがいは、Lam が動く前に、ここでエラーになる
formatSpec :: Parser FormatSpec
formatSpec = do
  -- 「書式は : のすぐ後ろに書きます（リストの : は ( ) で包みます）」
  _      <- notFollowedBy (char ' ') <|> fail
              "a format comes right after : (put the : of lists in ( ))"
  o1     <- getOffset
  align  <- optional (choice [ ToLeft   <$ char '<'
                             , ToRight  <$ char '>'
                             , ToCenter <$ char '^' ])
  o2     <- getOffset
  zero   <- isJust <$> optional (char '0')
  o3     <- getOffset
  width  <- optional (L.decimal <?> "a width")
  o4     <- getOffset
  comma  <- isJust <$> optional (char ',')
  o5     <- getOffset
  dot    <- isJust <$> optional (char '.')
  digits <- if dot then optional (L.decimal <?> "digits") else pure Nothing
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
      -- 「. の後ろには、小数点以下のけたの数が要ります」
      | dot && digits == Nothing ->
          failAt (o5 + 1) "a number of digits must follow ."
      | otherwise -> pure (FormatSpec align zero width comma digits)

-- 🍬 The function calls a format opens into: first the value as text,
--    then the text lined up.
--      {n:,}    groupDigits (showNumber n)    {x:.2}  Float.fixed 2 x
--      {n:05}   zeroPad 5 (showNumber n)      {x:>8}  padLeft 8 (display x)
-- 書式を、関数の呼び出しに開く。まず値を文字にし、次に桁をそろえる
applySpec :: FormatSpec -> Expr -> Expr
applySpec s e = pad s (textOf s e)

-- 🔢 The value as text. With , 0 or .digits, it is a number: its text
--    from showNumber or Float.fixed, with commas or 0s put in.
--    Otherwise, display x.
-- 値を文字にする。, 0 .けた のどれかがあれば数で、showNumber か
-- Float.fixed の文字に、カンマか 0 を入れる。なければ display x
textOf :: FormatSpec -> Expr -> Expr
textOf (FormatSpec _ zero width comma digits) e
  | comma     = call "groupDigits" [asNumber]
  | zero      = call "zeroPad" [widthOf width, asNumber]
  | isJust digits = asNumber
  | otherwise = call "display" [e]
  where
    asNumber = case digits of
      Just d  -> call "Float.fixed" [Lit (LInt d), e]
      Nothing -> call "showNumber" [e]

-- 📏 Line the text up:  > is padLeft (the spaces go on the left).
-- 文字の桁をそろえる。> は padLeft（空白を左に足す）
pad :: FormatSpec -> Expr -> Expr
pad s text = case specAlign s of
  Just ToLeft   -> call "padRight" [widthOf (specWidth s), text]
  Just ToRight  -> call "padLeft"  [widthOf (specWidth s), text]
  Just ToCenter -> call "center"   [widthOf (specWidth s), text]
  Nothing       -> text

widthOf :: Maybe Integer -> Expr
widthOf = Lit . LInt . fromMaybe 0

-- ANCHOR: call
-- 📞 f a b, as a tree. f is a hidden name of the prelude's f
--    ($fmt.padLeft): a program cannot write it, so no import and no
--    definition of its own can change what f"..." calls.
-- f a b を木にする。f は、プレリュードの f の隠した名前（$fmt.padLeft）。
-- プログラムには書けないので、import も自分の定義も、f"..." が呼ぶ
-- ものを変えられない
call :: Name -> [Expr] -> Expr
call f = foldl App (Var (fmtName f))

-- 🙈 padLeft  is  $fmt.padLeft
fmtName :: Name -> Name
fmtName f = "$fmt." <> f
-- ANCHOR_END: call
-- ANCHOR_END: formatSpec

-- ANCHOR: run
-- 📖 Read a whole program (spaces at the start, nothing left at the end).
--    Without the marks of places: this is for looking at trees (the
--    tests, GHCi), and for them the places are only noise.
-- プログラム全体を読む（先頭の空白から、最後まで読みきる）。
-- 場所の印は取る。木を見るため（テスト、GHCi）のもので、そこでは
-- 場所はじゃまなだけだから
parseExpr :: Operators -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr ops =
  runLamParser ops (stripLocations <$> (spaces *> expr <* eof)) "<input>"

-- 📣 The same, with an error message for people.
-- 同じだが、人に読めるエラーの文にする
parseLam :: Operators -> FilePath -> Text -> Either Text Expr
parseLam ops file source =
  case runLamParser ops (spaces *> expr <* eof) file source of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right e  -> Right e

-- ANCHOR: program
-- ANCHOR: programOnly
-- 📜 A program file: the module line and the imports (both can be
--    left out), declarations (let without in, and data), then an
--    expression to compute at the end (it can be left out too).
-- プログラムのファイル：module の行と import（どちらもなくてよい）、
-- 宣言（in のない let と、data）を並べ、最後に計算する式を1つ置く
-- （これもなくてよい）
program :: Parser Program
program = do
  _ <- optional shebang
  spaces
  header  <- optional moduleHeader
  imports <- many importLine
  decls   <- many declaration
  final   <- optional expr
  eof
  pure (Program header imports decls final)
-- ANCHOR_END: programOnly

-- ANCHOR: shebang
-- #️⃣ #!/usr/bin/env lam  on the first line, for the shell. Lam skips
--    it, but keeps the line break, so the line numbers stay right.
-- 1行目の #!/usr/bin/env lam は、シェルのためのもの。Lam は読み飛ばす。
-- 改行は残すので、行の番号はずれない
shebang :: Parser ()
shebang = do
  _ <- string "#!"
  _ <- takeWhileP (Just "the rest of the #! line") (/= '\n')
  pure ()
-- ANCHOR_END: shebang

-- ANCHOR: programHead
-- 🪪 Only the module line and the imports: they need no operators,
--    so they can be read first, to know what the file imports.
-- module の行と import だけ。演算子が要らないので、先に読んで、
-- ファイルが何を import するかを知る
programHead :: Parser (Maybe Header, [Import])
programHead = do
  _ <- optional shebang
  spaces
  header  <- optional moduleHeader
  imports <- many importLine
  pure (header, imports)
-- ANCHOR_END: programHead

-- ANCHOR: fixitiesOf
-- 🔍 The first reading: only the lines  infixl 7 addPrice as <+>.
--    Every other line is skipped. With them, the second reading knows
--    every operator of the file, even one used above its line.
-- 1回目の読みこみ：infixl 7 addPrice as <+> の行だけ。ほかの行は飛ばす。
-- これで、2回目の読みこみは、ファイルの演算子を全部知っている（宣言の
-- 行より上で使っていても）
fixitiesOf :: FilePath -> Text -> Either Text [(Loc, Text, Fixity)]
fixitiesOf file source =
  case runLamParser Map.empty (spaces *> many line <* eof) file source of
    Left err    -> Left (T.pack (errorBundlePretty err))
    Right found -> Right [ (loc, s, f) | Just (DefInfix loc s f) <- found ]
  where
    line = (Just <$> infixDecl) <|> (Nothing <$ skipLine)
    skipLine = (takeWhile1P Nothing (/= '\n') <|> T.singleton <$> newline)
                 *> spaces
-- ANCHOR_END: fixitiesOf

-- ANCHOR: infixDecl
-- ➕ infixl 7 addPrice as <+>:  <+> is another name for addPrice,
--    leaning left, at level 7.
-- <+> は addPrice の別名で、左に寄り、強さは 7
infixDecl :: Parser Decl
infixDecl = do
  loc <- here
  assoc <- choice [ LeftA <$ keyword "infixl", RightA <$ keyword "infixr"
                  , NoneA <$ keyword "infix" ]
  o <- getOffset
  level <- lexeme L.decimal
  -- 「強さは 0 から 9 までです」
  if level > 9 then stopAt o "the level of an operator is 0 to 9" else pure ()
  f <- name
  keyword "as"
  o' <- getOffset
  s <- lexeme operatorText
  -- 「= は言語が使う記号なので、演算子にできません」
  if s `elem` reservedSymbols
    then stopAt o' (T.unpack s <> " belongs to the language:"
                    <> " it cannot be an operator")
    else pure (DefInfix loc s (Fixity assoc level f False))
-- ANCHOR_END: infixDecl

-- ANCHOR: moduleHeader
-- 🪧 module Shapes exposing (Shape (..), area)
-- module の行
moduleHeader :: Parser Header
moduleHeader = do
  loc <- here
  keyword "module"
  m <- conName
  keyword "exposing"
  Header loc m <$> exposingList

-- 📥 import Shapes,  import Shapes as S,  import Shapes (area),
--    import Shapes as S (area). The list is the same as Haskell's, with
--    no word in front. It must be on the same line (or to the right of
--    column 1): a ( at column 1 starts the expression at the end.
-- import の1行。一覧は Haskell と同じく、前に言葉を付けない。同じ行に
-- （か、1桁目より右に）書く。1桁目の ( は、最後の式の始まり
importLine :: Parser Import
importLine = do
  loc <- here
  keyword "import"
  m <- conName
  alias' <- optional (keyword "as" *> conName)
  names  <- optional (continuing *> exposingList)
  pure (Import loc m alias' names)

-- 👀 (..)  or  (area, Shape, Shape (..), Json (JInt, JStr)). A class
--    is written like a type: Describe (..) shows its methods too.
-- 見せるものの一覧。クラスは型と同じ形：Describe (..) はメソッドも見せる
exposingList :: Parser Exposing
exposingList = between (symbol "(") (symbol ")") $
  (ExposeAll <$ symbol "..") <|> (ExposeSome <$> (item `sepBy1` symbol ","))
  where
    item = (ExposeOperator <$> try (symbol "(" *> lexeme operatorText
                                      <* symbol ")"))
       <|> (ExposeValue <$> name) <|> (ExposeType <$> conName <*> shown)
    shown = option NoConstructors $ between (symbol "(") (symbol ")") $
      (AllConstructors <$ symbol "..")
        <|> (TheseConstructors <$> ((conName <|> name) `sepBy1` symbol ","))

-- ANCHOR: here
-- 📍 Where the parser is now: the file, the line and the column.
-- パーサーがいま読んでいる場所：ファイル、行、列
here :: Parser Loc
here = do
  pos <- getSourcePos
  pure (Loc (sourceName pos)
             (unPos (sourceLine pos))
             (unPos (sourceColumn pos)))
-- ANCHOR_END: here
-- ANCHOR_END: moduleHeader

-- 📜 One declaration. A let followed by in is not one: it is the
--    expression at the end.
-- 宣言を1つ。in が続く let は宣言ではなく、最後の式
-- ANCHOR: declaration
declaration :: Parser Decl
declaration = choice
  [ DefData <$> dataDecl
  -- ANCHOR: declarationMore
  , DefAlias <$> aliasDecl
  -- ANCHOR: declarationClass
  , DefClass <$> classDecl
  , DefInstance <$> instanceDecl
  -- ANCHOR_END: declarationClass
  -- ANCHOR: declarationInfix
  , infixDecl
  -- ANCHOR_END: declarationInfix
  , signature
  -- ANCHOR_END: declarationMore
  , try (uncurry DefValue <$> topDefinition <* notFollowedBy (keyword "in")) ]
-- ANCHOR_END: declaration
-- ANCHOR_END: program
-- ANCHOR_END: run
-- ANCHOR_END: all
