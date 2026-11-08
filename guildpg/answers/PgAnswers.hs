-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}

-- 🏋️ Answers for the training.
-- トレーニングの解答
module PgAnswers (countAdventurers) where

import Database.PostgreSQL.Simple
import Data.Int (Int64)
-- ANCHOR_END: imports

-- ANCHOR: count
-- 🔢 How many adventurers are in the roster. count(*) is a bigint.
-- 名簿の人数。count(*) は bigint（64ビットの整数）
countAdventurers :: Connection -> IO Int64
countAdventurers conn = do
  rows <- query_ conn "SELECT count(*) FROM adventurers"
  case rows of
    [Only n] -> pure n
    _        -> pure 0
-- ANCHOR_END: count
-- ANCHOR_END: all
