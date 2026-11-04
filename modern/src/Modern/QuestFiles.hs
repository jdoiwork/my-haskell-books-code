-- ANCHOR: all
-- | Load quests from a folder, one file per quest.
-- フォルダーから、1ファイルに1つずつ書かれた依頼を読む
module Modern.QuestFiles
  ( loadQuestsDraft
  , loadQuests
  ) where

import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath ((</>))

-- ANCHOR: draft
-- 😵 The first draft: read the files in the order the folder lists
--    them. That order is up to the OS and the file system.
-- 最初の下書き：フォルダーが並べた順に読む。その順番は、
-- OS とファイルシステムしだい
loadQuestsDraft :: FilePath -> IO [String]
loadQuestsDraft dir = do
  names <- listDirectory dir
  mapM (\name -> readQuest (dir </> name)) names
-- ANCHOR_END: draft

-- ANCHOR: sorted
-- 📂 Sort the names first, so every machine reads the same order.
-- 先に名前で並べる。どの機械でも、同じ順に読むように
loadQuests :: FilePath -> IO [String]
loadQuests dir = do
  names <- listDirectory dir
  mapM (\name -> readQuest (dir </> name)) (sort names)
-- ANCHOR_END: sorted

-- 📜 The first line of the file is the quest.
-- ファイルの1行目が、依頼
readQuest :: FilePath -> IO String
readQuest path = do
  text <- readFile path
  pure (takeWhile (/= '\n') text)
-- ANCHOR_END: all
