-- ANCHOR: pragmas
{-# LANGUAGE FunctionalDependencies #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Columns that know their table and the type they hold.
-- 自分の表と、しまう値の型を知っている列
module MiniSql.Column
  ( Col
  , HasColumn
  , colName
  ) where

-- ANCHOR: imports
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
-- ANCHOR_END: imports

-- ANCHOR: col
-- 🏷️ A column of table t, holding values of type a. Only the name
--    is kept; t and a are promises on the type level.
-- 表 t の、型 a の値をしまう列。持っているのは名前だけ。
-- t と a は、型の世界の約束
newtype Col t a = Col Text
  deriving (Show, Eq)

colName :: Col t a -> Text
colName (Col name) = name

-- 📒 "Table t has a column called name, of type a."
--    Each table writes one instance per column.
-- 「表 t には、name という名前の、型 a の列がある」。
-- 表ごとに、列1つにつき1つのインスタンスを書く
class KnownSymbol name => HasColumn t (name :: Symbol) a | t name -> a

-- 🔖 #level becomes Col "level", if the table has that column.
-- #level は Col "level" になる。表に、その列があれば
instance HasColumn t name a => IsLabel name (Col t a) where
  fromLabel = Col (T.pack (symbolVal (Proxy @name)))
-- ANCHOR_END: col
-- ANCHOR_END: all
