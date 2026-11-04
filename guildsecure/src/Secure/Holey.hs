-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🕳️ A small module with holes in it, on purpose (for the training).
-- わざと穴を空けた小さなモジュール（トレーニング用）
module Secure.Holey
  ( ApiToken (..)
  , questsByOwnerUnsafe
  , canDeleteDraft
  ) where

import Data.Text              (Text)
import Database.SQLite.Simple

import Secure.Authz

-- ANCHOR: token
-- 🕳️ Hole 1: show prints the token as it is.
-- 穴1：show が、トークンをそのまま出してしまう
newtype ApiToken = ApiToken Text
  deriving Show
-- ANCHOR_END: token

-- ANCHOR: sql
-- 🕳️ Hole 2: the owner's name is glued into the SQL.
-- 穴2：依頼主の名前を、SQL に貼り付けている
questsByOwnerUnsafe :: Connection -> Text -> IO [(Text, Text)]
questsByOwnerUnsafe conn who =
  query_ conn (Query sql)
  where
    sql = "SELECT title, owner FROM quests WHERE owner = '" <> who <> "'"
-- ANCHOR_END: sql

-- ANCHOR: delete
-- 🕳️ Hole 3: the rule is "only the guild master may delete a quest".
-- 穴3：決まりは「依頼を消せるのは、ギルドマスターだけ」
canDeleteDraft :: Member -> Quest -> Bool
canDeleteDraft m q = m.role == Master || m.name == q.owner
-- ANCHOR_END: delete
-- ANCHOR_END: all
