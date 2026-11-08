-- ANCHOR: pragmas
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | Deriving strategies, and DerivingVia.
-- deriving の作戦と、DerivingVia
module Money
  ( Gold (..)
  , Best (..)
  , Loot (..)
  ) where

-- ANCHOR: imports
import Data.Aeson (ToJSON)
import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..))
import GHC.Generics (Generic)
-- ANCHOR_END: imports

-- ANCHOR: strategies
-- 🎒 Three strategies, written out.
-- 3つの作戦を、はっきり書く
data Loot = Loot { item :: String, count :: Int }
  deriving stock    (Show, Eq, Generic)  -- GHC writes it / GHC が書く
  deriving anyclass (ToJSON)             -- the class's default / 既定の中身
-- ANCHOR_END: strategies

-- ANCHOR: via
-- 💰 Gold adds up: borrow Semigroup and Monoid from Sum Int.
-- ゴールドは足し算でまとめる：Sum Int から Semigroup と Monoid を借りる
newtype Gold = Gold Int
  deriving stock   (Show)
  deriving newtype (Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Int)

-- 🏆 A best score keeps the larger one: borrow from Max Int.
-- 最高記録は大きいほうを残す：Max Int から借りる
newtype Best = Best Int
  deriving stock   (Show)
  deriving newtype (Eq)
  deriving (Semigroup, Monoid) via (Max Int)
-- ANCHOR_END: via
-- ANCHOR_END: all
