-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

-- 🛡️ The roster with beam: the table is described by a type.
-- beam で書いた名簿：表を型で表す
module Main (main) where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (open, close, execute_)
import Data.Int  (Int32)
import Data.Text (Text)

-- ANCHOR: types
-- 🧙 The table of adventurers. f is filled in by beam.
-- 冒険者の表。f には、beam が場面に合わせた型を入れる
data AdventurerT f = Adventurer
  { advId :: C f Int32
  , name  :: C f Text
  , job   :: C f Text
  , level :: C f Int32
  }
  deriving (Generic, Beamable)

type Adventurer = AdventurerT Identity
deriving instance Show Adventurer

instance Table AdventurerT where
  data PrimaryKey AdventurerT f = AdventurerId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = AdventurerId . advId

-- 🏰 The whole database: just one table here.
-- データベース全体（ここでは表が1つ）
newtype GuildDb f = GuildDb
  { adventurers :: f (TableEntity AdventurerT) }
  deriving stock    Generic
  deriving anyclass (Database be)

guildDb :: DatabaseSettings be GuildDb
guildDb = defaultDbSettings
-- ANCHOR_END: types

-- ANCHOR: query
-- 💪 Adventurers of this level or more, the strongest first.
-- このレベル以上の冒険者を、強い順に
strong :: Int32 -> SqliteM [Adventurer]
strong lv =
  runSelectReturningList $ select $
    orderBy_ (\a -> desc_ (level a)) $
      filter_ (\a -> level a >=. val_ lv) $
        all_ (adventurers guildDb)
-- ANCHOR_END: query

main :: IO ()
main = do
  conn <- open ":memory:"
  -- beam turns the field advId into the column "id".
  -- beam は、フィールド advId を、列の名前 "id" にする
  execute_ conn "CREATE TABLE adventurers (id INTEGER PRIMARY KEY, \
                \name TEXT NOT NULL UNIQUE, job TEXT NOT NULL, \
                \level INTEGER NOT NULL)"
  runBeamSqlite conn $ do
    runInsert $ insert (adventurers guildDb) $ insertExpressions
      [ Adventurer default_ "Lambda" "Mage" 3
      , Adventurer default_ "Python" "Archer" 5
      , Adventurer default_ "Rust" "Knight" 1
      ]
    rows <- strong 3
    liftIO (mapM_ print rows)
  close conn
-- ANCHOR_END: all
