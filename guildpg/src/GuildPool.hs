-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}

-- 🏊 A pool of connections, shared by many tasks.
-- たくさんの仕事で使い回す、接続のプール
module GuildPool (newGuildPool) where

import Database.PostgreSQL.Simple
import Data.ByteString (ByteString)
import Data.Pool
-- ANCHOR_END: imports

-- ANCHOR: pool
-- 🏊 At most 5 connections; an idle one is closed after 30 seconds.
-- 接続は最大5本。30秒使われなかった接続は閉じる
newGuildPool :: ByteString -> IO (Pool Connection)
newGuildPool url =
  newPool (setNumStripes (Just 1) config)
  where
    config = defaultPoolConfig open close 30 5
    -- Name the pool's connections, so that we can count them.
    -- 数えられるように、プールの接続に名前を付ける
    open = do
      conn <- connectPostgreSQL url
      _ <- execute_ conn "SET application_name = 'guild-pool'"
      pure conn
-- ANCHOR_END: pool
-- ANCHOR_END: all
