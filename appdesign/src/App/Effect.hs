-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE TypeFamilies #-}
-- | The same roster as an effect, with effectful.
-- 同じ名簿を、effectful のエフェクトとして
module App.Effect
  ( Roster (..)
  , findAdventurerE
  , addNewE
  , enlistE
  , runRosterPure
  , runRosterSqlite
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import App.Env (Env (..), runApp)
import qualified App.Roster as Roster
import App.Types
-- ANCHOR_END: imports

-- ANCHOR: effect
-- 📜 The roster effect: the things it can do, as a GADT.
-- 名簿のエフェクト：できることを GADT で
data Roster :: Effect where
  FindAdventurer :: Text -> Roster m (Maybe Adventurer)
  AddNew         :: NewAdventurer -> Roster m Int

type instance DispatchOf Roster = Dynamic

findAdventurerE :: Roster :> es => Text -> Eff es (Maybe Adventurer)
findAdventurerE who = send (FindAdventurer who)

addNewE :: Roster :> es => NewAdventurer -> Eff es Int
addNewE new = send (AddNew new)
-- ANCHOR_END: effect

-- ANCHOR: service
-- 📝 The guild's work. The type says: it uses the roster, and nothing else.
-- ギルドの仕事。型が言う：名簿を使う。ほかには何もしない
enlistE :: Roster :> es => NewAdventurer -> Eff es (Either AddError Int)
enlistE new = do
  found <- findAdventurerE (newName new)
  case found of
    Just _  -> pure (Left (NameTaken (newName new)))
    Nothing -> Right <$> addNewE new
-- ANCHOR_END: service

-- ANCHOR: handlers
-- 🧪 Handle the roster with a Map in a State (pure, for tests).
-- 名簿を、State の中の Map で扱う（純粋。テスト用）
runRosterPure :: Eff (Roster : es) a -> Eff es (a, Map.Map Text Adventurer)
runRosterPure = reinterpret (runState Map.empty) $ \_ op -> case op of
  FindAdventurer who -> gets (Map.lookup who)
  AddNew new -> do
    roster <- get
    let newId = Map.size roster + 1
        adv   = Adventurer newId (newName new) (newJob new) (newLevel new)
    put (Map.insert (newName new) adv roster)
    pure newId

-- 🗃️ Handle the roster with SQLite (the App functions do the work).
-- 名簿を SQLite で扱う（仕事は App の関数がする）
runRosterSqlite :: IOE :> es => Connection -> Eff (Roster : es) a -> Eff es a
runRosterSqlite conn = interpret $ \_ op -> liftIO $ runApp env $ case op of
  FindAdventurer who -> Roster.findByName who
  AddNew new         -> Roster.addAdventurer new
  where
    env = Env { envConn = conn, envLog = \_ -> pure () }
-- ANCHOR_END: handlers
-- ANCHOR_END: all
