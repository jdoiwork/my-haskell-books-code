-- ANCHOR: all
-- | A small language to pick cards by my own attributes and states:
--   連載の状態 = 連載中 and ★ >= 4 and 推し度 >= 3
--   Read it, check it against the definitions, then use it.
-- 自分で決めた属性と状態でカードを選ぶ、小さな言葉。
-- 読んで、定義に照らして型を確かめてから、使う
-- ANCHOR: header
module Tsundoku.Query
  ( -- * Reading / 読む
    Op (..)
  , Query (..)
  , Parser
  , parseQuery
  , term
  , op
    -- * Checking / 型を確かめる
  , Column (..)
  , Cond (..)
  , checkQuery
  , checkTerm
  , showCond
  , symbol
    -- * Using / 使う
  , holds
  , search
  ) where

-- text, and parsing with megaparsec
-- 文字列、megaparsec で読む
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec, anySingle, choice, eof, errorBundlePretty, many, manyTill
  , runParser, takeWhile1P, (<|>) )
import Text.Megaparsec.Char (char, space, space1, string)

import Tsundoku.Card (Card (..), stateOf)
import Tsundoku.Custom
import Tsundoku.Field
import Tsundoku.Rating (RatingStyle (..), Score, parse, render)
import Tsundoku.Types
import Tsundoku.Validation
-- ANCHOR_END: header

-- ANCHOR: syntax
-- ⚖️ How to compare.
-- 比べ方
data Op = Eq | Ne | Lt | Le | Gt | Ge
  deriving (Show, Eq, Enum, Bounded)

-- 📝 What was typed, read but not checked yet: names and values are
--    still just text.
-- 打ちこまれたもの。読んだだけで、まだ確かめていない。
-- 名前も値も、ただの文字のまま
data Query
  = Compare Text Op Text    -- 推し度 >= 3
  | And     Query Query     -- … and …
  deriving (Show, Eq)
-- ANCHOR_END: syntax

-- ANCHOR: parser
type Parser = Parsec Void Text

-- 📖 Read a whole search: terms joined by "and".
-- 絞り込みの言葉の全体を読む："and" でつないだ項
parseQuery :: Text -> Checked Query
parseQuery typed =
  case runParser (space *> query <* eof) "search" typed of
    Left  bundle -> Left (CannotParse (T.pack (errorBundlePretty bundle)))
    Right q      -> Right q
  where
    query = do
      first <- term
      rest  <- many (string "and" *> space1 *> term)
      pure (foldl And first rest)

-- 🧱 One term: a name, a way to compare, and a value.
-- 項を1つ：名前、比べ方、値
term :: Parser Query
term = do
  name  <- word
  how   <- op
  value <- quoted <|> word
  pure (Compare name how value)

-- 🔤 A word: anything up to a space or a way to compare.
-- 単語：空白か、比べ方の記号の手前まで
word :: Parser Text
word = lexeme (takeWhile1P (Just "a name or a value") wordChar)

-- 💬 A value with spaces in it goes in quotes: "神保町の古本市".
-- 空白を含む値は、引用符でくくる："神保町の古本市"
quoted :: Parser Text
quoted = lexeme (T.pack <$> (char '"' *> manyTill anySingle (char '"')))

-- ⚖️ The ways to compare. Longer ones first, so ">=" is not read as ">".
-- 比べ方の記号。">=" を ">" と読まないよう、長いものから試す
op :: Parser Op
op = lexeme (choice
  [ Ge <$ string ">=", Le <$ string "<=", Ne <$ string "!="
  , Eq <$ string "=" , Gt <$ string ">" , Lt <$ string "<"  ])

-- 🧹 Read something, then the spaces after it.
-- 何かを読んで、その後ろの空白も読む
lexeme :: Parser a -> Parser a
lexeme p = p <* space
-- ANCHOR_END: parser

-- ANCHOR: typed
-- 🗂️ Something on a card to compare, and the type of its values.
-- カードの上の、比べられるもの。その値の型も持つ
data Column a where
  AttrCol  :: Field a -> Column a     -- an attribute / 自分で決めた属性
  StateCol :: Flow    -> Column Text  -- a state / 自分で決めた状態
  StarsCol ::            Column Score -- ★ (the rating) / 作品全体の評価

-- ✅ A search that has been checked: in every Test, the value has
--    the same type as the column. Nothing is just text any more.
-- 確かめ終えた絞り込み。どの Test でも、値の型は列の型とそろっている。
-- もう、ただの文字はない
data Cond where
  Test :: Column a -> Op -> a -> Cond
  Both :: Cond -> Cond -> Cond
-- ANCHOR_END: typed

-- ANCHOR: check
-- 🔎 Check a search against the definitions. Every problem is
--    reported, as in the form.
-- 絞り込みを、定義に照らして確かめる。まずい所は、フォームと同じく全部
checkQuery :: Schema -> Query -> Checked Cond
checkQuery schema = checked BadAttrs . go
  where
    go (And a b)              = Both <$> go a <*> go b
    go (Compare name how raw) = checkTerm schema name how raw

-- 🔎 One term. The name says which column; the column says how to read
--    the value, and whether it can be bigger or smaller.
-- 項を1つ。名前で列が決まり、列で値の読み方と、大小で比べてよいかが決まる
checkTerm :: Schema -> Text -> Op -> Text -> Validation [AttrProblem] Cond
checkTerm schema name how raw
  | name == "★"                      = stars
  | Just def  <- findAttr schema name = attribute def
  | Right flow <- findFlow schema name = state flow
  | otherwise                        = problem (UnknownName name)
  where
    -- ★4 or 4, read as in chapter 1. Ratings can be bigger or smaller.
    -- ★4 か 4。1章目と同じく読む。評価は大小で比べられる
    stars = case parse FiveStars raw of
      Right s -> Valid (Test StarsCol how s)
      Left _  -> problem (Unreadable name raw)
    -- The definition's kind reads the value; the GADT keeps its type.
    -- 定義の種類で値を読む。その型を、GADT が持ち続ける
    attribute def = case kindOf (attrKind def) of
      SomeKind kind ->
        case readValue (attrKind def) raw >>= fromValue kind of
          Nothing -> problem (Unreadable name raw)
          Just x
            | ordered (attrKind def) || not (sizes how) ->
                Valid (Test (AttrCol (Field name kind)) how x)
            | otherwise -> problem (Unordered name)
    -- A state must be one of the flow's. States are only = or !=.
    -- 状態は、その流れの中のもの。比べ方は = か != だけ
    state flow
      | raw `notElem` flowStates flow = problem (Unreadable name raw)
      | sizes how                     = problem (Unordered name)
      | otherwise                     = Valid (Test (StateCol flow) how raw)

-- 📏 Only numbers and dates can be bigger or smaller.
-- 大小で比べられるのは、数と日付だけ
ordered :: AttrKind -> Bool
ordered kind = case kind of
  NumberKind -> True
  DateKind   -> True
  _          -> False

-- 📏 The ways that ask "bigger or smaller?".
-- 「大きい？小さい？」を聞く比べ方
sizes :: Op -> Bool
sizes how = how `elem` [Lt, Le, Gt, Ge]
-- ANCHOR_END: check

-- ANCHOR: holds
-- 🧪 Does a card pass? A card with no value for the column does not.
-- カードは通る？ その列の値がないカードは、通らない
holds :: Cond -> Card -> Bool
holds (Both a b)        card = holds a card && holds b card
holds (Test col how x)  card = case valueIn col card of
  Nothing -> False
  Just v  -> passes how (orderIn col v x)

-- 🔍 The value of a column on a card, with the column's type.
-- カードの、その列の値。列の型で
valueIn :: Column a -> Card -> Maybe a
valueIn (AttrCol f)    card = get f card
valueIn (StateCol flow) card = stateOf flow card
valueIn StarsCol       card = rating card

-- ⚖️ How to compare two values of a column.
-- 列の値を2つ比べる
orderIn :: Column a -> a -> a -> Ordering
orderIn (AttrCol (Field _ kind)) = compareIn kind
orderIn (StateCol _)             = compare
orderIn StarsCol                 = compare

-- ✔️ Does the result of a comparison pass the way to compare?
-- 比べた結果が、その比べ方で「はい」か
passes :: Op -> Ordering -> Bool
passes how o = case how of
  Eq -> o == EQ
  Ne -> o /= EQ
  Lt -> o == LT
  Le -> o /= GT
  Gt -> o == GT
  Ge -> o /= LT
-- ANCHOR_END: holds

-- ANCHOR: search
-- 🔎 Read, check, and pick: the cards that pass a search.
-- 読む → 確かめる → 選ぶ：絞り込みを通るカード
search :: Schema -> Text -> [(WorkId, Card)] -> Checked [(WorkId, Card)]
search schema typed cards = do
  q    <- parseQuery typed
  cond <- checkQuery schema q
  pure [ (wid, card) | (wid, card) <- cards, holds cond card ]
-- ANCHOR_END: search

-- ANCHOR: showCond
-- 🖨️ A checked search, written back as text (to show what we searched).
-- 確かめた絞り込みを、文字に書き戻す（何で絞ったかを見せるため）
showCond :: Cond -> Text
showCond (Both a b)       = showCond a <> " and " <> showCond b
showCond (Test col how x) = T.unwords [nameOf col, symbol how, shown col x]
  where
    nameOf :: Column a -> Text
    nameOf (AttrCol (Field name _)) = name
    nameOf (StateCol flow)          = flowName flow
    nameOf StarsCol                 = "★"
    shown :: Column a -> a -> Text
    shown (AttrCol (Field _ kind)) v = inQuotes (showValue (toValue kind v))
    shown (StateCol _)             v = inQuotes v
    shown StarsCol                 v = render FiveStars v
    -- A value that is not one word goes in quotes, so it reads back.
    -- 1つの単語でない値は、引用符でくくる（読み戻せるように）
    inQuotes v
      | not (T.null v) && T.all wordChar v = v
      | otherwise                          = "\"" <> v <> "\""

-- ⚖️ The symbol of a way to compare, as it is typed.
-- 比べ方の記号。打ちこむときと同じ形
symbol :: Op -> Text
symbol how = case how of
  Eq -> "="
  Ne -> "!="
  Lt -> "<"
  Le -> "<="
  Gt -> ">"
  Ge -> ">="
-- ANCHOR_END: showCond
-- ANCHOR_END: all
