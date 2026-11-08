-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏋️ Answers for the training.
-- トレーニングの解答
module Rel8Answers (byJob, countAdventurers, removeAdventurer) where

import Data.Int  (Int64)
import Data.Text (Text)
import Data.Functor.Contravariant ((>$<))
import Hasql.Decoders  qualified as D
import Hasql.Encoders  qualified as E
import Hasql.Statement qualified as Hasql
import Rel8

import Rel8Roster
-- ANCHOR_END: imports

-- ANCHOR: byJob
-- 🧭 Adventurers of this job, by name.
-- この職業の冒険者を、名前の順に
byJob :: Text -> Query (Adventurer Expr)
byJob jobName = orderBy ((.name) >$< asc) $ do
  a <- each adventurers
  where_ (a.job ==. lit jobName)
  pure a
-- ANCHOR_END: byJob

-- ANCHOR: count
-- 🔢 How many adventurers, with hasql. count(*) is a bigint (int8).
-- 人数（hasql で）。count(*) は bigint（int8）
countAdventurers :: Hasql.Statement () Int64
countAdventurers = Hasql.Statement sql E.noParams decoder True
  where
    sql     = "SELECT count(*) FROM adventurers"
    decoder = D.singleRow (D.column (D.nonNullable D.int8))
-- ANCHOR_END: count

-- ANCHOR: remove
-- 🚪 Remove an adventurer with rel8. Returns how many rows went.
-- rel8 で冒険者を外す。消えた行の数を返す
removeAdventurer :: Text -> Hasql.Statement () Int64
removeAdventurer who = runN $ delete Delete
  { from        = adventurers
  , using       = pure ()
  , deleteWhere = \_ a -> a.name ==. lit who
  , returning   = NoReturning
  }
-- ANCHOR_END: remove
-- ANCHOR_END: all
