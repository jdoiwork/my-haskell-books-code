-- ANCHOR: all
{-# LANGUAGE DeriveAnyClass #-}

-- 📒 The loot book: the items, reading them, and the pure core.
-- 戦利品の目録：品物、読み込み、純粋な核
module Loot.Book
  ( Item (..)
  , BookError (..)
  , parseBook
  , totalValue
  , topItems
  ) where

import Data.Aeson           (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.List            (sortOn)
import Data.Ord             (Down (..))
import Data.Text            (Text)
import GHC.Generics         (Generic)

import Loot.Gold

-- ANCHOR: item
-- 💎 One kind of item in the book: its price, and how many we have.
-- 目録の品物1種類：値段と、持っている数
data Item = Item
  { name  :: Text
  , price :: Gold
  , count :: Int
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- 🚫 Why a book could not be read.
-- 目録を読めなかった理由
data BookError
  = BadJson String
  | NegativeCount Text
  deriving (Show, Eq)
-- ANCHOR_END: item

-- ANCHOR: parse
-- 📒 Read the book: first the JSON, then check every count.
--    Either's do stops at the first Left.
-- 目録を読む：まず JSON、それから数を1つずつ確かめる。
-- Either の do は、最初の Left で止まる
parseBook :: BL.ByteString -> Either BookError [Item]
parseBook bytes = do
  items <- either (Left . BadJson) Right (eitherDecode bytes)
  mapM checkCount items

checkCount :: Item -> Either BookError Item
checkCount item
  | count item < 0 = Left (NegativeCount (name item))
  | otherwise      = Right item
-- ANCHOR_END: parse

-- ANCHOR: core
-- 💰 The value of the whole book.
-- 目録全体の値打ち
totalValue :: [Item] -> Gold
totalValue items = mconcat [timesGold (count i) (price i) | i <- items]

-- 🏆 The n most valuable kinds of items (by price times count).
-- 値打ち（値段×数）の高い品物を n 種類
topItems :: Int -> [Item] -> [Item]
topItems n items = take n (sortOn (Down . value) items)
  where
    value i = timesGold (count i) (price i)
-- ANCHOR_END: core
-- ANCHOR_END: all
