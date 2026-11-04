{-# LANGUAGE OverloadedLabels #-}
-- | This file must NOT compile: #hpp is not a field of Hero.
-- このファイルはコンパイルできないはず：#hpp は Hero のフィールドではない
module Typo where

import Labels

-- ANCHOR: typo
heroHpTypo :: Int
heroHpTypo = #hpp lambda
-- ANCHOR_END: typo
