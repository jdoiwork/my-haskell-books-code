-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- 📋 The data the server and the browser share.
-- サーバーとブラウザーで共有するデータ
module Board.Types
  ( Quest (..)
  , NewQuest (..)
  , ApiError (..)
  , checkNewQuest
  , tsDeclarations
  ) where

import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
-- ANCHOR_END: imports

-- ANCHOR: types
-- 📜 A quest on the board.
-- 依頼板の依頼
data Quest = Quest
  { questId :: Int
  , title   :: Text
  , reward  :: Int
  , done    :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📝 What the browser sends to post a quest.
-- 依頼を貼るとき、ブラウザーが送ってくるもの
data NewQuest = NewQuest
  { title  :: Text
  , reward :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 🚫 The JSON we send back when something is wrong.
-- まちがいがあったときに返す JSON
newtype ApiError = ApiError { message :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-- ANCHOR_END: types

-- ANCHOR: check
-- 🛂 The gate: a title of 1 to 60 letters, a reward of 0 to 10000 G.
-- 門番：題は 1〜60 文字、報酬は 0〜10000 G
checkNewQuest :: NewQuest -> Either Text NewQuest
checkNewQuest new
  | T.null (T.strip new.title)  = Left "the title is empty"
  | T.length new.title > 60     = Left "the title is too long"
  | new.reward < 0              = Left "the reward is negative"
  | new.reward > 10000          = Left "the reward is too large"
  | otherwise                   = Right new
-- ANCHOR_END: check

-- ANCHOR: ts
-- 🟦 The same types for TypeScript, made from the Haskell types.
-- 同じ型を TypeScript でも。Haskell の型から作る
$(deriveTypeScript defaultOptions ''Quest)
$(deriveTypeScript defaultOptions ''NewQuest)
$(deriveTypeScript defaultOptions ''ApiError)

tsDeclarations :: [TSDeclaration]
tsDeclarations =
  getTypeScriptDeclarations (Proxy @Quest)
    <> getTypeScriptDeclarations (Proxy @NewQuest)
    <> getTypeScriptDeclarations (Proxy @ApiError)
-- ANCHOR_END: ts
-- ANCHOR_END: all
