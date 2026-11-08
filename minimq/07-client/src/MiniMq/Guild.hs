-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DeriveAnyClass #-}
-- | The guild's letters, with their types: quests and herbs.
-- ギルドの手紙と、その型：依頼と薬草
module MiniMq.Guild
  ( Quest (..)
  , Herb (..)
  , questBoard
  , dragonTeam
  , herbShop
  , herbShelf
  , graveyard
  , deadLetters
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import MiniMq.Typed
import MiniMq.Types
-- ANCHOR_END: header

-- ANCHOR: types
-- 📜 A quest, and a herb. Two different kinds of letter.
-- 依頼と薬草。2種類の、ちがう手紙
data Quest = Quest
  { title  :: Text
  , reward :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Herb = Herb
  { herbName :: Text
  , bundles  :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: types

-- ANCHOR: desks
-- 🗂️ Where quests go, and where herbs go.
-- 依頼の行き先と、薬草の行き先
questBoard :: Desk Quest
questBoard = Desk "quests" Topic json

dragonTeam :: Mailbox Quest
dragonTeam = Mailbox "dragon-team" settings json
  where
    settings = plainQueue { deadLetterTo = Just "graveyard"
                          , deliveryLimit = Just 3 }

herbShop :: Desk Herb
herbShop = Desk "herbs" Direct json

herbShelf :: Mailbox Herb
herbShelf = Mailbox "herb-shelf" plainQueue json

-- 🪦 Letters given up end here, whatever they were: kept as plain text.
-- あきらめた手紙は、何であれここへ。ただの文字列としてとっておく
graveyard :: Desk Text
graveyard = Desk "graveyard" Fanout plain

deadLetters :: Mailbox Text
deadLetters = Mailbox "dead-letters" plainQueue plain

plain :: Codec Text
plain = Codec id Right
-- ANCHOR_END: desks
-- ANCHOR_END: all
