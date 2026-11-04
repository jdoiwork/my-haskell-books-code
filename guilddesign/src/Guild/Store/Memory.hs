-- ANCHOR: all
{-# LANGUAGE OverloadedRecordDot #-}

-- 🧠 A store in memory: a Map in a box that can be rewritten.
-- メモリーの置き場：書き換えられる箱に入れた Map
module Guild.Store.Memory (newMemoryStore, newMemoryStoreDraft) where

import Data.IORef
import Data.List       (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text       (Text)

import Guild.Roster
import Guild.Store

-- ANCHOR: memory
-- 📦 What the box holds: the adventurers by name, and the next id.
-- 箱の中身：名前で引ける冒険者と、次に振る番号
data Memory = Memory
  { byName :: Map Text Adventurer
  , nextId :: Int
  }

-- 🧠 Make a new, empty store in memory.
-- メモリーに、空っぽの置き場を新しく作る
newMemoryStore :: IO RosterStore
newMemoryStore = newMemoryStoreWith add

-- 🧠 The same, with the given way to add.
-- 同じものを、渡された登録のしかたで作る
newMemoryStoreWith
  :: (IORef Memory -> NewAdventurer -> IO (Either AddError Int))
  -> IO RosterStore
newMemoryStoreWith addWith = do
  box <- newIORef (Memory { byName = Map.empty, nextId = 1 })
  pure RosterStore
    { addAdventurer    = addWith box
    , listAdventurers  = list box
    , findAdventurer   = \who -> Map.lookup who . (.byName) <$> readIORef box
    , levelUp          = \who -> change box who (Map.adjust up who)
    , removeAdventurer = \who -> change box who (Map.delete who)
    }
  where
    up a = a { level = a.level + 1 }

-- ➕ Add, unless the name is taken.
-- 名前が使われていなければ、足す
add :: IORef Memory -> NewAdventurer -> IO (Either AddError Int)
add box new = do
  memory <- readIORef box
  if Map.member new.newName memory.byName
    then pure (Left (NameTaken new.newName))
    else do
      let newId = memory.nextId
          entry = Adventurer newId new.newName new.newJob new.newLevel
      writeIORef box Memory
        { byName = Map.insert new.newName entry memory.byName
        , nextId = newId + 1
        }
      pure (Right newId)

-- 📋 Everyone, in the order they joined.
-- 全員を、登録した順に
list :: IORef Memory -> IO [Adventurer]
list box = do
  memory <- readIORef box
  pure (sortOn (.adventurerId) (Map.elems memory.byName))

-- 🔁 Change the roster, if the name is there. Did it change?
-- その名前がいれば、名簿を変える。変わった？
change :: IORef Memory -> Text -> (Map Text Adventurer -> Map Text Adventurer)
       -> IO Bool
change box who f = do
  memory <- readIORef box
  if Map.member who memory.byName
    then do
      writeIORef box memory { byName = f memory.byName }
      pure True
    else pure False
-- ANCHOR_END: memory

-- ANCHOR: draft
-- 🐞 A first draft: it forgets that names must be unique.
-- 下書き：名前がかぶってはいけないことを忘れている
newMemoryStoreDraft :: IO RosterStore
newMemoryStoreDraft = newMemoryStoreWith addDraft

addDraft :: IORef Memory -> NewAdventurer -> IO (Either AddError Int)
addDraft box new = do
  memory <- readIORef box
  let newId = memory.nextId
      entry = Adventurer newId new.newName new.newJob new.newLevel
  writeIORef box Memory
    { byName = Map.insert new.newName entry memory.byName
    , nextId = newId + 1
    }
  pure (Right newId)
-- ANCHOR_END: draft
-- ANCHOR_END: all
