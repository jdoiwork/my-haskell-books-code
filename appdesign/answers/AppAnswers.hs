-- ANCHOR: all
-- | Answers to the training of "Monad transformers and application design".
-- 「モナド変換子とアプリケーション設計」のトレーニングの解答
module AppAnswers
  ( levelUp
  , promoteAll
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Database.SQLite.Simple

import App.Env
import App.Roster
import App.Types

-- ANCHOR: levelUp
-- ⬆️ Step 2: raise one adventurer's level, and log it.
-- ステップ2：1人のレベルを上げて、ログに書く
levelUp :: Text -> App Bool
levelUp who = do
  changed <- db $ \conn -> do
    execute conn
      "UPDATE adventurers SET level = level + 1 WHERE name = ?" (Only who)
    changes conn
  if changed == 1
    then logInfo ("level up: " <> who) >> pure True
    else pure False
-- ANCHOR_END: levelUp

-- ANCHOR: promoteAll
-- 🎉 Step 3: everybody gains a level, with a tag on every log line.
-- ステップ3：全員のレベルを上げる。ログの行には、札を付ける
promoteAll :: App ()
promoteAll = withContext "festival" $ do
  everyone <- listAll
  forM_ everyone $ \adv -> levelUp (advName adv)
-- ANCHOR_END: promoteAll
-- ANCHOR_END: all
