-- ANCHOR: all
-- | Read an hsx template into a tree. This runs at compile time.
-- hsx のテンプレートを木に読む。コンパイルのときに動く。
module Hsx.Parse
  ( Node (..)
  , Attr (..)
  , AttrValue (..)
  , ParseError (..)
  , parseHsx
  , parseHsxAt
  , renderError
  , voidElements
  ) where

import Data.Char (isAlpha, isAlphaNum, isLower, isDigit, isSpace)
import Data.List (dropWhileEnd)

-- ANCHOR: tree
-- 🌳 The tree of a template.
-- テンプレートの木
data Node
  = Element String [Attr] [Node]  -- <p class="x">...</p>
  | TextNode String               -- Hello,
  | Splice [String]               -- {name} or {show level}
  deriving (Eq, Show)

-- 🏷️ One attribute: class="hero" or class={cls}
-- 属性ひとつ：class="hero" か class={cls}
data Attr = Attr String AttrValue
  deriving (Eq, Show)

data AttrValue
  = Static String     -- "hero"
  | Dynamic [String]  -- {cls}
  | Bare              -- disabled（値なし）
  deriving (Eq, Show)
-- ANCHOR_END: tree

-- ANCHOR: error
-- 🚨 What went wrong, and where (line and column).
-- 何が、どこ（行と桁）でまちがっていたか
data ParseError = ParseError
  { errLine    :: Int
  , errColumn  :: Int
  , errMessage :: String
  }
  deriving (Eq, Show)

-- 📣 The message GHC shows as a compile error.
-- GHC がコンパイルエラーとして見せる文
renderError :: ParseError -> String
renderError e =
  "hsx: line " <> show (errLine e)
    <> ", column " <> show (errColumn e)
    <> ": " <> errMessage e
-- ANCHOR_END: error

-- ANCHOR: parser
-- ⚙️ A tiny parser: the rest of the input and the position.
-- 小さなパーサー：残りの入力と、いまの位置
data Input = Input
  { rest   :: String
  , line   :: Int
  , column :: Int
  }

newtype Parser a = Parser (Input -> Either ParseError (a, Input))

runParser :: Parser a -> Input -> Either ParseError (a, Input)
runParser (Parser p) = p

instance Functor Parser where
  fmap f p = Parser (\i -> case runParser p i of
    Left e        -> Left e
    Right (a, i') -> Right (f a, i'))

instance Applicative Parser where
  pure a    = Parser (\i -> Right (a, i))
  pf <*> pa = do
    f <- pf
    a <- pa
    pure (f a)

instance Monad Parser where
  p >>= f = Parser (\i -> case runParser p i of
    Left e        -> Left e
    Right (a, i') -> runParser (f a) i')

-- ❌ Stop with a message at the current position.
-- いまの位置で、メッセージを付けて止まる
failHere :: String -> Parser a
failHere msg =
  Parser (\i -> Left (ParseError (line i) (column i) msg))

-- ❌ Stop with a message at a position we remembered.
-- 覚えておいた位置で、メッセージを付けて止まる
failAt :: (Int, Int) -> String -> Parser a
failAt (l, c) msg = Parser (\_ -> Left (ParseError l c msg))

-- 👀 Look at the next character without taking it.
-- 次の1文字を、取らずに見る
peek :: Parser (Maybe Char)
peek = Parser (\i -> case rest i of
  []      -> Right (Nothing, i)
  (c : _) -> Right (Just c, i))

-- ✋ Take one character, moving the position.
-- 1文字取って、位置を進める
next :: Parser Char
next = Parser (\i -> case rest i of
  [] -> Left (ParseError (line i) (column i) "unexpected end")
  (c : cs)
    | c == '\n' -> Right (c, Input cs (line i + 1) 1)
    | otherwise -> Right (c, Input cs (line i) (column i + 1)))

-- 📍 The current position.
-- いまの位置
position :: Parser (Int, Int)
position = Parser (\i -> Right ((line i, column i), i))

-- 🔍 Is the rest starting with this string?
-- 残りがこの文字列で始まる？
startsWith :: String -> Parser Bool
startsWith s = Parser (\i -> Right (take (length s) (rest i) == s, i))

-- Take characters while they match.
-- 条件に合うあいだ、文字を取る
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP ok = do
  mc <- peek
  case mc of
    Just c | ok c -> do
      _  <- next
      cs <- takeWhileP ok
      pure (c : cs)
    _ -> pure []

-- Take exactly this character, or fail.
-- ちょうどこの文字を取る。違えば止まる
expect :: Char -> Parser ()
expect c = do
  mc <- peek
  if mc == Just c
    then () <$ next
    else failHere ("expected " <> show c <> foundText mc)

foundText :: Maybe Char -> String
foundText Nothing  = ", but the template ended"
foundText (Just c) = ", but found " <> show c

skipSpaces :: Parser ()
skipSpaces = () <$ takeWhileP isSpace
-- ANCHOR_END: parser

-- ANCHOR: parseHsx
-- 📖 Read the whole template.
-- テンプレート全体を読む
parseHsx :: String -> Either ParseError [Node]
parseHsx = parseHsxAt (1, 1)

-- 📍 The same, but counting lines and columns from a given place
--    (where the template starts in the source file).
-- 同じだが、行と桁を、指定した位置（ソースファイルの中で
-- テンプレートが始まる位置）から数える
parseHsxAt :: (Int, Int) -> String -> Either ParseError [Node]
parseHsxAt (l, c) src =
  case runParser (nodes Nothing) (Input src l c) of
    Left e        -> Left e
    Right (ns, _) -> Right (cleanNodes ns)
-- ANCHOR_END: parseHsx

-- ANCHOR: nodes
-- 🔁 Read nodes until the closing tag of the parent
--    (or the end of the template when there is no parent).
-- 親の閉じタグまで（親がなければ最後まで）ノードを読む
nodes :: Maybe (String, (Int, Int)) -> Parser [Node]
nodes parent = do
  mc      <- peek
  closing <- startsWith "</"
  case (mc, parent) of
    (Nothing, Nothing) -> pure []
    (Nothing, Just (tag, (l, c))) ->
      failHere ("<" <> tag <> "> opened at line " <> show l
                <> ", column " <> show c <> " is never closed")
    _ | closing -> case parent of
          Nothing -> failHere "a closing tag with no opening tag"
          Just _  -> pure []
    (Just '<', _) -> (:) <$> element <*> nodes parent
    (Just '{', _) -> (:) <$> splice  <*> nodes parent
    _             -> (:) <$> textNode <*> nodes parent

-- 📝 Plain text until the next tag or {.
-- 次のタグか { までの、ふつうの文字
textNode :: Parser Node
textNode = TextNode <$> takeWhileP (\c -> c /= '<' && c /= '{')

-- 🔌 {name} or {show level}: names separated by spaces.
-- {name} や {show level}：空白で区切った名前
splice :: Parser Node
splice = Splice <$> spliceBody

spliceBody :: Parser [String]
spliceBody = do
  expect '{'
  skipSpaces
  ws <- identifiers
  expect '}'
  if null ws
    then failHere "empty {}"
    else pure ws

identifiers :: Parser [String]
identifiers = do
  w <- takeWhileP isIdentChar
  skipSpaces
  if null w
    then pure []
    else (w :) <$> identifiers

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c `elem` ("_'." :: String)
-- ANCHOR_END: nodes

-- ANCHOR: element
-- 🏗️ <tag attrs>children</tag>, <tag/> or a void tag like <br>.
-- <tag 属性>子</tag>、<tag/>、または <br> のような空要素
element :: Parser Node
element = do
  start <- position
  expect '<'
  tag   <- tagName
  attrs <- attributes
  selfClosing <- startsWith "/>"
  if selfClosing
    then do
      expect '/' >> expect '>'
      pure (Element tag attrs [])
    else do
      expect '>'
      if tag `elem` voidElements
        then pure (Element tag attrs [])
        else do
          children <- nodes (Just (tag, start))
          closeTag tag start
          pure (Element tag attrs children)

-- 🔚 The closing tag must have the same name.
-- 閉じタグは、同じ名前でなければならない
closeTag :: String -> (Int, Int) -> Parser ()
closeTag tag (l, c) = do
  here <- position
  expect '<' >> expect '/'
  name <- tagName
  if name == tag
    then skipSpaces >> expect '>'
    else failAt here ("</" <> name <> "> does not match <" <> tag
                   <> "> opened at line " <> show l
                   <> ", column " <> show c)

tagName :: Parser String
tagName = do
  name <- takeWhileP (\c -> isLower c || isDigit c || c == '-')
  if null name
    then failHere "a tag name must be lowercase letters"
    else pure name

-- Elements that never have children.
-- 子を持たない要素
voidElements :: [String]
voidElements = ["br", "hr", "img", "input", "meta", "link"]
-- ANCHOR_END: element

-- ANCHOR: attributes
-- 🏷️ name="value", name={expr} or just name.
-- name="値"、name={式}、または name だけ
attributes :: Parser [Attr]
attributes = do
  skipSpaces
  mc <- peek
  case mc of
    Just c | isAlpha c -> (:) <$> attribute <*> attributes
    _                  -> pure []

attribute :: Parser Attr
attribute = do
  name <- takeWhileP (\c -> isAlphaNum c || c == '-')
  mc   <- peek
  case mc of
    Just '=' -> do
      _ <- next
      Attr name <$> attrValue
    _ -> pure (Attr name Bare)

attrValue :: Parser AttrValue
attrValue = do
  mc <- peek
  case mc of
    Just '"' -> do
      _ <- next
      v <- takeWhileP (/= '"')
      expect '"'
      pure (Static v)
    Just '{' -> Dynamic <$> spliceBody
    _ -> failHere ("an attribute value must be \"...\" or {...}"
                   <> foundText mc)
-- ANCHOR_END: attributes

-- ANCHOR: clean
-- 🧹 Whitespace, like JSX: text with a line break is trimmed
--    line by line, and empty lines disappear.
-- JSX と同じ空白の扱い：改行のある文字は行ごとに削り、
-- 空になった行は消える
cleanNodes :: [Node] -> [Node]
cleanNodes = concatMap clean
  where
    clean (TextNode t) = case cleanText t of
      "" -> []
      t' -> [TextNode t']
    clean (Element tag attrs children) =
      [Element tag attrs (cleanNodes children)]
    clean s = [s]

cleanText :: String -> String
cleanText t
  | '\n' `notElem` t = t
  | otherwise        = unwords' (filter (not . null) trimmed)
  where
    ls      = lines' t
    trimmed = zipWith trimLine [0 :: Int ..] ls
    lastIx  = length ls - 1
    trimLine i l
      | i == 0      = dropWhileEnd isSpace l
      | i == lastIx = dropWhile isSpace l
      | otherwise   = dropWhileEnd isSpace (dropWhile isSpace l)
    unwords' = foldr1' (\a b -> a <> " " <> b)
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs

-- Like lines, but keeps a last empty line ("a\n" -> ["a", ""]).
-- lines と同じだが、最後の空の行も残す
lines' :: String -> [String]
lines' s = case break (== '\n') s of
  (l, [])       -> [l]
  (l, _ : more) -> l : lines' more
-- ANCHOR_END: clean
-- ANCHOR_END: all
