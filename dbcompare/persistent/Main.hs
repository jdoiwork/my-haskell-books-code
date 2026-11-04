-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- 🏛️ The roster with persistent: types, tables, and migrations
--    are all made from a short schema.
-- persistent で書いた名簿：短い定義から、型も表もマイグレーションも作る
module Main (main) where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

-- ANCHOR: schema
-- 📜 The schema. Template Haskell makes the type Adventurer from it.
-- 定義。Template Haskell が、ここから型 Adventurer を作る
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Adventurer
  name  Text
  job   Text
  level Int
  UniqueName name
  deriving Show
|]
-- ANCHOR_END: schema

-- ANCHOR: main
main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  insertMany_
    [ Adventurer "Lambda" "Mage" 3
    , Adventurer "Python" "Archer" 5
    , Adventurer "Rust" "Knight" 1
    ]
  rows <- selectList [AdventurerLevel >=. 3] [Desc AdventurerLevel]
  liftIO (mapM_ (print . entityVal) rows)
-- ANCHOR_END: main
-- ANCHOR_END: all
