-- ANCHOR: all
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- 🧙 The data the API sends and receives, as JSON.
-- API がやりとりするデータ（JSON で）
module Guild.Types
  ( Adventurer (..)
  , NewAdventurer (..)
  , ApiError (..)
  ) where

import Data.Aeson         (FromJSON, ToJSON, defaultOptions)
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi       (ToSchema)
import Data.Text          (Text)
import GHC.Generics       (Generic)

-- ANCHOR: types
-- 🧙 An adventurer in the roster.
-- 名簿の冒険者
data Adventurer = Adventurer
  { adventurerId :: Int
  , name         :: Text
  , job          :: Text
  , level        :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ✍️ What a client sends to add an adventurer.
-- 冒険者を登録するときに、クライアントが送るもの
data NewAdventurer = NewAdventurer
  { name  :: Text
  , job   :: Text
  , level :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- 🚫 The body of an error response: {"message": "..."}
-- エラーの応答の中身：{"message": "..."}
newtype ApiError = ApiError { message :: Text }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
-- ANCHOR_END: types

-- ANCHOR: typescript
-- 🟦 Also make TypeScript types for the same data (for the React side).
-- 同じデータの TypeScript の型も作る（React 側のために）
$(deriveTypeScript defaultOptions ''Adventurer)
$(deriveTypeScript defaultOptions ''NewAdventurer)
$(deriveTypeScript defaultOptions ''ApiError)
-- ANCHOR_END: typescript
-- ANCHOR_END: all
