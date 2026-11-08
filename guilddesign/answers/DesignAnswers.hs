-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🏋️ Answers for the training.
-- トレーニングの解答
module DesignAnswers (withLogging, extraContract) where

import Data.Text    qualified as T
import Data.Text.IO qualified as T
import Test.Hspec

import Guild.Roster
import Guild.Store
-- ANCHOR_END: imports

-- ANCHOR: logging
-- 📣 Wrap any store: say each add and level up, then pass it on.
-- どの置き場でも包める：登録とレベル上げを知らせてから、中に渡す
withLogging :: RosterStore -> RosterStore
withLogging inner = inner
  { addAdventurer = \new -> do
      T.putStrLn ("[log] add " <> new.newName)
      inner.addAdventurer new
  , levelUp = \who -> do
      T.putStrLn ("[log] level up " <> who)
      inner.levelUp who
  }
-- ANCHOR_END: logging

-- ANCHOR: extra
-- 📜 One more promise for every store.
-- どの置き場にも、もう1つの約束
extraContract :: SpecWith RosterStore
extraContract =
  -- 「いない名前なら、Nothing」
  it "finds nobody for an unknown name" $ \store ->
    store.findAdventurer (T.pack "Nobody") `shouldReturn` Nothing
-- ANCHOR_END: extra
-- ANCHOR_END: all
