{-# LANGUAGE OverloadedLabels #-}
-- | This file must NOT compile: there is no column "levle".
-- このファイルはコンパイルできないはず：levle という列はない
module Typo where

import Data.Function ((&))

import Guild
import MiniSql.Expr
import MiniSql.Query

-- ANCHOR: typo
strongTypo :: Select Adventurer
strongTypo = selectFrom adventurers & where_ (#levle .>= lit 3)
-- ANCHOR_END: typo
