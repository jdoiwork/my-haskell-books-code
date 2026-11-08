-- ANCHOR: pragmas
{-# LANGUAGE UndecidableInstances #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Expressions with types, turned into SQL with ? placeholders.
-- 型を持った式。? の穴の付いた SQL に変える
module MiniSql.Expr
  ( Expr (..)
  , lit
  , col
  , (.==), (./=), (.<), (.>), (.<=), (.>=)
  , (.&&), (.||)
  , (.+)
  , render
  ) where

-- ANCHOR: imports
import Data.Text (Text)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple (SQLData)
import GHC.OverloadedLabels (IsLabel (..))

import MiniSql.Column
-- ANCHOR_END: imports

-- ANCHOR: expr
-- 🧮 An expression over table t, whose value has type a.
-- 表 t の上の式。値の型は a
data Expr t a where
  Lit     :: ToField a => a -> Expr t a
  Column  :: Col t a -> Expr t a
  Compare :: Text -> Expr t a -> Expr t a -> Expr t Bool
  Logic   :: Text -> Expr t Bool -> Expr t Bool -> Expr t Bool
  Plus    :: Expr t Int -> Expr t Int -> Expr t Int
-- ANCHOR_END: expr

-- ANCHOR: ops
lit :: ToField a => a -> Expr t a
lit = Lit

col :: Col t a -> Expr t a
col = Column

-- 🔖 A label can be an expression too: #level :: Expr Adventurer Int.
-- 札は、式にもなれる：#level :: Expr Adventurer Int
instance HasColumn t name a => IsLabel name (Expr t a) where
  fromLabel = Column (fromLabel @name)

-- ⚖️ Both sides must have the same type: no comparing names with numbers.
-- 両側は同じ型でなければならない：名前と数は比べられない
(.==), (./=), (.<), (.>), (.<=), (.>=) :: Expr t a -> Expr t a -> Expr t Bool
(.==) = Compare "="
(./=) = Compare "<>"
(.<)  = Compare "<"
(.>)  = Compare ">"
(.<=) = Compare "<="
(.>=) = Compare ">="

(.&&), (.||) :: Expr t Bool -> Expr t Bool -> Expr t Bool
(.&&) = Logic "AND"
(.||) = Logic "OR"

(.+) :: Expr t Int -> Expr t Int -> Expr t Int
(.+) = Plus

infix  4 .==, ./=, .<, .>, .<=, .>=
infixr 3 .&&
infixr 2 .||
infixl 6 .+
-- ANCHOR_END: ops

-- ANCHOR: render
-- 🖨️ To SQL text and the values for its ? holes, in order.
--    Values never go into the text: no SQL injection.
-- SQL の文と、? の穴に入る値（順番どおり）に。
-- 値は文に入れない：SQL インジェクションは起きない
render :: Expr t a -> (Text, [SQLData])
render expr = case expr of
  Lit v              -> ("?", [toField v])
  Column c           -> (colName c, [])
  Compare op l r     -> binary op l r
  Logic op l r       -> binary op l r
  Plus l r           -> binary "+" l r
  where
    binary :: Text -> Expr t b -> Expr t c -> (Text, [SQLData])
    binary op l r =
      let (lt, lv) = render l
          (rt, rv) = render r
      in  ("(" <> lt <> " " <> op <> " " <> rt <> ")", lv ++ rv)
-- ANCHOR_END: render
-- ANCHOR_END: all
