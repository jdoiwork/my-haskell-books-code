-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- 🛡️ Who may edit a quest? (Types can not decide this for us.)
-- 誰が依頼を書き換えてよい？（型は、これを決めてくれない）
module Secure.Authz
  ( Role (..)
  , Member (..)
  , Quest (..)
  , canEditDraft
  , canEdit
  ) where

import Data.Text (Text)
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🛡️ The roles in the guild.
-- ギルドでの立場
data Role = Guest | Adventurer | Master
  deriving (Show, Eq, Enum, Bounded)

-- 🧙 A member of the guild, and a quest on the board.
-- ギルドの一員と、掲示板の依頼
data Member = Member { name :: Text, role :: Role }
  deriving (Show, Eq)

data Quest = Quest { title :: Text, owner :: Text }
  deriving (Show, Eq)
-- ANCHOR_END: types

-- ANCHOR: draft
-- ❌ Draft: "guests can't edit" - but anyone else can edit anyone's quest!
-- 下書き：「ゲストは書き換えられない」。
-- でも、それ以外は誰の依頼でも書き換えられる！
canEditDraft :: Member -> Quest -> Bool
canEditDraft m _ = m.role /= Guest
-- ANCHOR_END: draft

-- ANCHOR: canEdit
-- 🛡️ The guild master may edit any quest;
--    an adventurer may edit only their own.
-- ギルドマスターはどの依頼でも、冒険者は自分の依頼だけ書き換えられる
canEdit :: Member -> Quest -> Bool
canEdit m q =
  case m.role of
    Master     -> True
    Adventurer -> m.name == q.owner
    Guest      -> False
-- ANCHOR_END: canEdit
-- ANCHOR_END: all
