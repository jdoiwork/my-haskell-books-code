-- ANCHOR: all
-- ANCHOR: head
-- | Training, step 3: "or", "not", and parentheses in the search.
--   The terms are read and checked by the chapter's code.
-- トレーニングのステップ3：絞り込みの言葉に or・not・カッコを足す。
-- 項を読むのと確かめるのは、章のコードに任せる
module ShelfAnswers.OrNot
  ( Search (..)
  , parseSearch
  , Pick (..)
  , checkSearch
  , picks
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
  (between, eof, errorBundlePretty, runParser, try, (<|>))
import Text.Megaparsec.Char (space, space1, string)

import Tsundoku.Card (Card)
import Tsundoku.Query
import Tsundoku.Types
import Tsundoku.Validation
-- ANCHOR_END: head

-- ANCHOR: search
-- 📝 A search with and, or, not, read but not checked yet.
-- and・or・not のある絞り込み。読んだだけで、まだ確かめていない
data Search
  = Term  Query            -- 推し度 >= 3
  | AllOf Search Search    -- … and …
  | AnyOf Search Search    -- … or …
  | Not   Search           -- not …
  deriving (Show, Eq)

-- 📖 Read it. not binds tighter than and, and tighter than or:
--    "a or b and not c" is "a or (b and (not c))".
-- 読む。not が and より、and が or より強く結びつく：
-- "a or b and not c" は "a or (b and (not c))"
parseSearch :: Text -> Checked Search
parseSearch typed =
  case runParser (space *> expr <* eof) "search" typed of
    Left  bundle -> Left (CannotParse (T.pack (errorBundlePretty bundle)))
    Right s      -> Right s
  where
    expr = makeExprParser leaf
      [ [ Prefix (Not   <$ keyword "not") ]
      , [ InfixL (AllOf <$ keyword "and") ]
      , [ InfixL (AnyOf <$ keyword "or" ) ]
      ]
    leaf = between (paren "(") (paren ")") expr <|> (Term <$> term)
    keyword w = try (string w <* space1)
    paren p   = string p <* space

-- ✅ Checked: every term is a Cond of the chapter.
-- 確かめたもの：どの項も、章の Cond
data Pick
  = Pass    Cond
  | AllPass Pick Pick
  | AnyPass Pick Pick
  | NotPass Pick

-- 🔎 Check every term against the definitions (all problems at once).
-- どの項も、定義に照らして確かめる（まずい所は全部まとめて）
checkSearch :: Schema -> Search -> Checked Pick
checkSearch schema = checked BadAttrs . go
  where
    go (Term (Compare n how v)) = Pass    <$> checkTerm schema n how v
    go (Term (And a b))         = AllPass <$> go (Term a) <*> go (Term b)
    go (AllOf a b)              = AllPass <$> go a <*> go b
    go (AnyOf a b)              = AnyPass <$> go a <*> go b
    go (Not a)                  = NotPass <$> go a

-- 🧪 Does a card pass?
-- カードは通る？
picks :: Pick -> Card -> Bool
picks (Pass cond)    card = holds cond card
picks (AllPass a b)  card = picks a card && picks b card
picks (AnyPass a b)  card = picks a card || picks b card
picks (NotPass a)    card = not (picks a card)
-- ANCHOR_END: search
-- ANCHOR_END: all
