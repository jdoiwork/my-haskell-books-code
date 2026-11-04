{-# LANGUAGE OverloadedLabels #-}
-- ANCHOR: all
-- | The guild's roster, described for MiniSql.
-- ギルドの名簿を、MiniSql 向けに書く
module Guild
  ( Adventurer (..)
  , adventurers
  , setup
  , insert
  , strong
  , mages
  , levelUpMages
  , retire
  ) where

import Data.Function ((&))
import Data.Text (Text)
import Database.SQLite.Simple

import MiniSql.Column
import MiniSql.Expr
import MiniSql.Query

-- ANCHOR: table
-- 🧙 A row of the roster.
-- 名簿の1行
data Adventurer = Adventurer
  { advId :: Int, name :: Text, job :: Text, level :: Int }
  deriving (Show, Eq)

instance FromRow Adventurer where
  fromRow = Adventurer <$> field <*> field <*> field <*> field

-- 📒 The columns, written once: their names and their types.
-- 列を1回だけ書く：名前と型
instance HasColumn Adventurer "id"    Int
instance HasColumn Adventurer "name"  Text
instance HasColumn Adventurer "job"   Text
instance HasColumn Adventurer "level" Int

adventurers :: Table Adventurer
adventurers = Table "adventurers" ["id", "name", "job", "level"]
-- ANCHOR_END: table

setup :: Connection -> IO ()
setup conn = execute_ conn
  "CREATE TABLE adventurers (id INTEGER PRIMARY KEY, \
  \name TEXT UNIQUE, job TEXT, level INTEGER)"

insert :: Connection -> Text -> Text -> Int -> IO ()
insert conn n j l = execute conn
  "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)" (n, j, l)

-- ANCHOR: queries
-- 💪 Adventurers at level n or above, by name.
-- レベル n 以上の冒険者を、名前の順に
strong :: Int -> Select Adventurer
strong n =
  selectFrom adventurers
    & where_ (#level .>= lit n)
    & orderBy #name Asc

-- 🧙 Mages, the strongest first, at most k of them.
-- 魔法使いを、強い順に、k 人まで
mages :: Int -> Select Adventurer
mages k =
  selectFrom adventurers
    & where_ (#job .== lit "mage")
    & orderBy #level Desc
    & limit k

-- ⬆️ Every mage gains a level.
-- 魔法使い全員のレベルを1つ上げる
levelUpMages :: Connection -> IO Int
levelUpMages conn =
  runUpdate conn adventurers
    [#level .= #level .+ lit 1]
    (#job .== lit "mage")

-- 👋 Remove an adventurer by name.
-- 名前で、冒険者を名簿から外す
retire :: Connection -> Text -> IO Int
retire conn who = runDelete conn adventurers (#name .== lit who)
-- ANCHOR_END: queries
-- ANCHOR_END: all
