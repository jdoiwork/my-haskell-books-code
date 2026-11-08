-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}

-- ANCHOR: module
module Bounty where

-- JSON: the classes, the generic helpers, and the options.
-- JSON の型クラス、Generic を使う関数、オプション
import Data.Aeson
  ( FromJSON (..)
  , Options (..)
  , ToJSON (..)
  , camelTo2
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )
import GHC.Generics (Generic)

-- Text.
-- テキスト
import Data.Text (Text)
-- ANCHOR_END: imports
-- ANCHOR_END: module

-- ANCHOR: bounty
-- 💰 A bounty posted on the guild's web site.
-- ギルドの Web サイトに貼り出された賞金首
data Bounty = Bounty
  { targetName :: Text
  , rewardGold :: Int
  , note       :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
-- ANCHOR_END: bounty

-- ANCHOR: options
-- ⚙️ How a Bounty looks in JSON:
--    targetName -> target_name, and no "note" when it is Nothing.
-- Bounty の JSON での形
-- targetName は target_name に。note が Nothing なら書かない
bountyOptions :: Options
bountyOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields  = True
  }

instance ToJSON Bounty where
  toJSON = genericToJSON bountyOptions

instance FromJSON Bounty where
  parseJSON = genericParseJSON bountyOptions
-- ANCHOR_END: options

-- ANCHOR: bounties
-- 💰 Some bounties.
-- 賞金首たち
kingSlime :: Bounty
kingSlime = Bounty
  { targetName = "King Slime"
  , rewardGold = 500
  , note       = Just "Watch out for the splash!"
  }

shadowWolf :: Bounty
shadowWolf = Bounty
  { targetName = "Shadow Wolf"
  , rewardGold = 300
  , note       = Nothing
  }
-- ANCHOR_END: bounties
-- ANCHOR_END: all
