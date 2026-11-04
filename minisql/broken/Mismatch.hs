{-# LANGUAGE OverloadedLabels #-}
-- | This file must NOT compile: a name is compared with a number.
-- このファイルはコンパイルできないはず：名前を数と比べている
module Mismatch where

import Data.Function ((&))

import Guild
import MiniSql.Expr
import MiniSql.Query

-- ANCHOR: mismatch
nameIsThree :: Select Adventurer
nameIsThree = selectFrom adventurers & where_ (#name .== lit (3 :: Int))
-- ANCHOR_END: mismatch
