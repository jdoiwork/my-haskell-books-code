-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultilineStrings #-}

-- 「JSON を扱う」の章のトレーニングの解答
module JsonAnswers where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  )
import GHC.Generics (Generic)

import Data.ByteString.Lazy (ByteString)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Monster (Monster (..), goblin, slime)
-- ANCHOR_END: imports

-- ANCHOR: quest
-- 📜 A quest, with JSON from Generic.
-- クエスト。JSON は Generic におまかせ
data Quest
  = NotStarted
  | InProgress Int
  | Completed Int
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- 📜 A quest in progress, written in JSON.
-- JSON で書いた、進行中のクエスト
questJson :: ByteString
questJson = """
  {
    "tag": "InProgress",
    "contents": 3
  }
  """
-- ANCHOR_END: quest

-- ANCHOR: potion
-- 🧪 A potion. It heals from 1 to 100 HP.
-- ポーション。回復量は 1〜100
data Potion = Potion
  { potionName :: Text
  , heal       :: Int
  , rare       :: Bool
  }
  deriving stock (Show, Eq)

-- 📥 "rare" may be missing (then False).
--    A heal outside 1..100 is an error.
-- "rare" はなくてもよい（そのときは False）
-- 回復量が 1〜100 の外ならエラー
instance FromJSON Potion where
  parseJSON = withObject "Potion" $ \o -> do
    n      <- o .:  "name"
    amount <- o .:  "heal"
    isRare <- o .:? "rare" .!= False
    if amount >= 1 && amount <= 100
      then pure (Potion n amount isRare)
      else fail ("heal must be from 1 to 100, but got " ++ show amount)

-- 🧪 Potions written in JSON. The second one heals too much.
-- JSON で書いたポーション。2つ目は回復しすぎ
potionJson :: ByteString
potionJson = """
  {
    "name": "potion",
    "heal": 30
  }
  """

megaPotionJson :: ByteString
megaPotionJson = """
  {
    "name": "mega potion",
    "heal": 500
  }
  """
-- ANCHOR_END: potion

-- ANCHOR: elementCounts
-- 📊 How many monsters of each element?
-- 属性ごとに、モンスターは何匹？
elementCounts :: [Monster] -> Map Text Int
elementCounts monsters =
  let pairs = [ (T.pack (show m.element), 1) | m <- monsters ]
  in  Map.fromListWith (+) pairs

-- 👾 A small bestiary to try it on.
-- 試すための小さな図鑑
sampleDex :: [Monster]
sampleDex = [slime, goblin, slime { name = "Blue Slime" }]
-- ANCHOR_END: elementCounts
