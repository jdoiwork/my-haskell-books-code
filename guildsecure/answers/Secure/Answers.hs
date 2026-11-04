-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- ✅ Answers for the training: the holes, filled.
-- トレーニングの解答：穴をふさいだもの
module Secure.Answers
  ( ApiToken
  , mkApiToken
  , questsByOwner
  , canDelete
  ) where

import Data.Text              (Text)
import Database.SQLite.Simple

import Secure.Authz

-- ANCHOR: token
-- ✅ Step 1: show never prints the token.
-- ステップ1：show はトークンを出さない
newtype ApiToken = ApiToken Text

instance Show ApiToken where
  show _ = "ApiToken <redacted>"

mkApiToken :: Text -> ApiToken
mkApiToken = ApiToken
-- ANCHOR_END: token

-- ANCHOR: sql
-- ✅ Step 2: the name goes into a ? hole, never into the SQL itself.
-- ステップ2：名前は ? の穴へ。SQL そのものには入れない
questsByOwner :: Connection -> Text -> IO [(Text, Text)]
questsByOwner conn who =
  query conn "SELECT title, owner FROM quests WHERE owner = ?" (Only who)
-- ANCHOR_END: sql

-- ANCHOR: delete
-- ✅ Step 3: only the guild master. The quest does not matter.
-- ステップ3：ギルドマスターだけ。依頼が誰のものかは関係ない
canDelete :: Member -> Quest -> Bool
canDelete m _ = m.role == Master
-- ANCHOR_END: delete
-- ANCHOR_END: all
