-- ANCHOR: all
-- | "What the roster can do" as a type class, so the guild's work can
--   run on SQLite or on a pure Map in the tests.
-- 「名簿ができること」を型クラスに。ギルドの仕事が、SQLite でも、
-- テスト用の純粋な Map でも動くように
module App.Class
  ( MonadRoster (..)
  , enlistM
  , Fake (..)
  , runFake
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import App.Env
import qualified App.Roster as Roster
import App.Types

-- ANCHOR: class
-- 📜 Anything that can find and add adventurers.
-- 冒険者を探せて、足せるもの
class Monad m => MonadRoster m where
  findAdventurer :: Text -> m (Maybe Adventurer)
  addNew         :: NewAdventurer -> m Int

-- 🗃️ The real application uses SQLite.
-- 本物のアプリは、SQLite を使う
instance MonadRoster App where
  findAdventurer = Roster.findByName
  addNew         = Roster.addAdventurer
-- ANCHOR_END: class

-- ANCHOR: service
-- 📝 The guild's work, written once, for any MonadRoster.
-- ギルドの仕事。どんな MonadRoster にも使えるように、1回だけ書く
enlistM :: MonadRoster m => NewAdventurer -> m (Either AddError Int)
enlistM new = do
  found <- findAdventurer (newName new)
  case found of
    Just _  -> pure (Left (NameTaken (newName new)))
    Nothing -> Right <$> addNew new
-- ANCHOR_END: service

-- ANCHOR: fake
-- 🧪 A pure stand-in for the tests: a Map in a State.
-- テスト用の、純粋な代役：State の中の Map
newtype Fake a = Fake (State (Map.Map Text Adventurer) a)
  deriving (Functor, Applicative, Monad)

instance MonadRoster Fake where
  findAdventurer who = Fake (gets (Map.lookup who))
  addNew new = Fake $ do
    roster <- get
    let newId = Map.size roster + 1
        adv   = Adventurer newId (newName new) (newJob new) (newLevel new)
    put (Map.insert (newName new) adv roster)
    pure newId

runFake :: Fake a -> (a, Map.Map Text Adventurer)
runFake (Fake m) = runState m Map.empty
-- ANCHOR_END: fake
-- ANCHOR_END: all
