-- ANCHOR: all
-- | Enlisting with ExceptT: stop at the first problem, without a
--   staircase of case expressions.
-- ExceptT で登録：case の階段なしに、最初の問題で止まる
module App.Enlist
  ( enlist
  , enlistCase
  ) where

-- ANCHOR: imports
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import qualified Data.Text as T

import App.Env
import App.Roster
import App.Types
-- ANCHOR_END: imports

-- ANCHOR: case
-- 🪜 Without ExceptT: one case for each check.
-- ExceptT なし：確かめるたびに case
enlistCase :: NewAdventurer -> App (Either AddError Int)
enlistCase new
  | T.null (newName new) = pure (Left EmptyName)
  | newLevel new < 1 || newLevel new > 99 =
      pure (Left (BadLevel (newLevel new)))
  | otherwise = do
      found <- findByName (newName new)
      case found of
        Just _  -> pure (Left (NameTaken (newName new)))
        Nothing -> Right <$> addAdventurer new
-- ANCHOR_END: case

-- ANCHOR: except
-- 🚪 With ExceptT: throwError leaves at once; lift runs an App step.
-- ExceptT で：throwError で、すぐに抜ける。lift で App の手順を動かす
enlist :: NewAdventurer -> App (Either AddError Int)
enlist new = runExceptT $ do
  when (T.null (newName new)) $
    throwError EmptyName
  when (newLevel new < 1 || newLevel new > 99) $
    throwError (BadLevel (newLevel new))
  found <- lift (findByName (newName new))
  when (found /= Nothing) $
    throwError (NameTaken (newName new))
  lift (withContext "enlist" (addAdventurer new))
-- ANCHOR_END: except
-- ANCHOR_END: all
