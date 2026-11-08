{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The quest board's tools for an AI chat, and the file that keeps it.
-- AI のチャットのための依頼板の道具と、依頼板をしまうファイル
module Modern.BoardMcp
  ( loadBoard
  , updateBoard
  , boardTools
  ) where

-- ANCHOR: imports
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Handle.Lock (LockMode (..), hLock)
import System.Directory (doesFileExist, renameFile)
import System.IO (IOMode (..), withFile)

import Modern.McpKit
import Modern.QuestBoard
-- ANCHOR_END: imports

-- ANCHOR: load
-- 📂 Read the board. No file yet means an empty board.
-- 依頼板を読む。まだファイルがなければ、空の依頼板
loadBoard :: FilePath -> IO (Either Text Board)
loadBoard path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right emptyBoard)
    else either (Left . T.pack) Right <$> eitherDecodeFileStrict path
-- ANCHOR_END: load

-- ANCHOR: update
-- ✍️ Read, apply one op, write. The new board goes to another file
--    first and then takes the old one's place, so nobody ever reads
--    half a file. The lock keeps two chats from writing at once.
-- 読んで、Op を1つ当てはめて、書く。新しい依頼板は、まず別のファイルに
-- 書いてから、古いファイルと入れかえる（書きかけを読まれないように）。
-- 錠で、2つのチャットが同時に書かないようにする
updateBoard :: FilePath -> BoardOp -> IO (Either Text Board)
updateBoard path op =
  withFile (path <> ".lock") AppendMode $ \lock -> do
    hLock lock ExclusiveLock
    loaded <- loadBoard path
    case loaded >>= applyOp op of
      Left err  -> pure (Left err)
      Right new -> do
        encodeFile (path <> ".tmp") new
        renameFile (path <> ".tmp") path
        pure (Right new)
-- ANCHOR_END: update

-- ANCHOR: tools
-- 🧰 The four tools. Three of them change the board; their names are the
--    names of the ops, so their arguments are read as an op.
-- 4つの道具。そのうち3つは依頼板を変える。名前が Op の名前と同じなので、
-- 引数を、そのまま Op として読める
boardTools :: FilePath -> [Tool]
boardTools path =
  [ Tool
      { toolName        = "list_quests"
      , toolDescription = "List the quests on the guild's quest board."
      , toolArguments   = []
      , toolRequired    = []
      , toolRun         = \_ -> reply <$> loadBoard path
      }
  , opTool "post_quest" "Post a new quest with a title and a reward in gold."
      [("title", text), ("reward", int)]
  , opTool "take_quest" "An adventurer takes an open quest, by its number."
      [("id", int), ("who", text)]
  , opTool "finish_quest" "Mark a taken quest as done, by its number."
      [("id", int)]
  ]
  where
    opTool name description arguments =
      Tool
        { toolName        = name
        , toolDescription = description
        , toolArguments   = arguments
        , toolRequired    = map (Key.toText . fst) arguments
        , toolRun         = \args ->
            let asOp = Object (KM.insert "op" (String name) args)
            in case parseEither parseJSON asOp of
                 Left err -> pure (Refuse (T.pack err))
                 Right op -> reply <$> updateBoard path op
        }
    -- The new board, or the refusal.
    -- 新しい依頼板か、断り
    reply (Right board) = Answer (describeBoard board)
    reply (Left err)    = Refuse err
    text = object ["type" .= ("string" :: Text)]
    int  = object ["type" .= ("integer" :: Text)]
-- ANCHOR_END: tools
-- ANCHOR_END: all
