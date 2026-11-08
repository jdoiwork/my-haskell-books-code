-- ANCHOR: all
-- ANCHOR: pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR_END: pragma

-- ANCHOR: imports
-- 🛡️ The roster with rel8: tables as types, queries as Haskell.
-- rel8 の名簿：表は型、問い合わせは Haskell の式
module Rel8Roster where

import Data.Int     (Int32, Int64)
import Data.Text    (Text)
import GHC.Generics (Generic)
import Data.Functor.Contravariant ((>$<))
import Hasql.Statement qualified as Hasql
import Rel8
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🧙 An adventurer. f decides what each column is.
-- 冒険者。f で、それぞれの列が何になるかが決まる
data Adventurer f = Adventurer
  { advId :: Column f Int32
  , name  :: Column f Text
  , job   :: Column f Text
  , level :: Column f Int32
  }
  deriving stock    Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (Adventurer f)

-- 📜 A quest, maybe taken by an adventurer.
-- 依頼（冒険者が受けているかもしれない）
data Quest f = Quest
  { questId :: Column f Int32
  , title   :: Column f Text
  , takenBy :: Column f (Maybe Int32)
  }
  deriving stock    Generic
  deriving anyclass Rel8able
-- ANCHOR_END: types

-- ANCHOR: schemas
-- 🗺️ Where each table and column is in the database.
-- 表と列が、データベースのどこにあるか
adventurers :: TableSchema (Adventurer Name)
adventurers = TableSchema
  { name    = "adventurers"
  , columns = Adventurer "id" "name" "job" "level"
  }

quests :: TableSchema (Quest Name)
quests = TableSchema
  { name    = "quests"
  , columns = Quest "id" "title" "taken_by"
  }
-- ANCHOR_END: schemas

-- ANCHOR: query
-- 💪 Adventurers of this level or more, the strongest first.
-- このレベル以上の冒険者を、強い順に
strong :: Int32 -> Query (Adventurer Expr)
strong lv = orderBy ((.level) >$< desc) $ do
  a <- each adventurers
  where_ (a.level >=. lit lv)
  pure a
-- ANCHOR_END: query

-- ANCHOR: insert
-- ➕ Add an adventurer, and get the id the database gave.
-- 冒険者を足して、データベースが付けた id を受け取る
addAdventurer :: Text -> Text -> Int32 -> Hasql.Statement () Int32
addAdventurer who jobName lv = run1 $ insert Insert
  { into       = adventurers
  , rows       = values [newRow]
  , onConflict = Abort
  , returning  = Returning (.advId)
  }
  where
    newRow = Adventurer unsafeDefault (lit who) (lit jobName) (lit lv)

-- 📜 Post a quest, maybe already taken.
-- 依頼を出す（もう誰かが受けているかもしれない）
postQuest :: Text -> Maybe Int32 -> Hasql.Statement () Int64
postQuest questTitle taker = runN $ insert Insert
  { into       = quests
  , rows       = values [Quest unsafeDefault (lit questTitle) (lit taker)]
  , onConflict = Abort
  , returning  = NoReturning
  }
-- ANCHOR_END: insert

-- ANCHOR: update
-- ⬆️ One level up. Returns how many rows changed.
-- レベルを1上げる。変わった行の数を返す
levelUp :: Text -> Hasql.Statement () Int64
levelUp who = runN $ update Update
  { target      = adventurers
  , from        = pure ()
  , set         = \_ a -> a { level = a.level + 1 }
  , updateWhere = \_ a -> a.name ==. lit who
  , returning   = NoReturning
  }
-- ANCHOR_END: update

-- ANCHOR: board
-- 📋 Each quest, and the name of who took it (if anyone).
-- 依頼ごとに、受けた冒険者の名前（いれば）
questBoard :: Query (Expr Text, MaybeTable Expr (Expr Text))
questBoard = do
  q     <- each quests
  taker <- optional $ do
    a <- each adventurers
    where_ (nullify a.advId ==. q.takenBy)
    pure a.name
  pure (q.title, taker)
-- ANCHOR_END: board

-- ANCHOR: count
-- 🔢 How many adventurers in each job.
-- 職業ごとの人数
jobCounts :: Query (Expr Text, Expr Int64)
jobCounts =
  aggregate1 ((,) <$> groupByOn (.job) <*> countStar) (each adventurers)
-- ANCHOR_END: count
-- ANCHOR_END: all
