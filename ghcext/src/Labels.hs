-- ANCHOR: pragmas
{-# LANGUAGE OverloadedLabels #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | What #level really is: fromLabel @"level".
-- #level の正体：fromLabel @"level"
module Labels
  ( Column (..)
  , Hero (..)
  , lambda
  , columns
  , heroHp
  , heroName
  ) where

-- ANCHOR: imports
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy (..))
-- ANCHOR_END: imports

-- ANCHOR: column
-- 🏷️ A column name. #level becomes Column "level".
-- 列の名前。#level は Column "level" になる
newtype Column = Column String
  deriving (Show, Eq)

-- The label's name is a string on the type level (a Symbol);
-- symbolVal brings it down to an ordinary String.
-- 札の名前は、型の世界の文字列（Symbol）。symbolVal で、ふつうの String に
instance KnownSymbol label => IsLabel label Column where
  fromLabel = Column (symbolVal (Proxy @label))

columns :: [Column]
columns = [#name, #level]
-- ANCHOR_END: column

-- ANCHOR: getter
-- 🔍 A label that reads a field: #hp is a function Hero -> Int.
--    If Hero has no such field, there is no HasField, and no compile.
-- フィールドを読む札：#hp は Hero -> Int の関数。
-- Hero にそのフィールドがなければ HasField がなく、コンパイルできない
data Hero = Hero { name :: String, hp :: Int }
  deriving (Show, Eq)

instance HasField label Hero a => IsLabel label (Hero -> a) where
  fromLabel = getField @label

lambda :: Hero
lambda = Hero { name = "Lambda", hp = 30 }

heroHp :: Int
heroHp = #hp lambda

heroName :: String
heroName = #name lambda
-- ANCHOR_END: getter
-- ANCHOR_END: all
