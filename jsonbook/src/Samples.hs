-- ANCHOR: all
-- ANCHOR: pragma
-- Let us write JSON text as ByteString values.
-- JSON の文字列を ByteString の値として書けるようにする
{-# LANGUAGE OverloadedStrings #-}
-- Let us write strings between """ and """ (GHC 9.12 and later).
-- """ と """ で囲んだ文字列を書けるようにする（GHC 9.12 以降）
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR_END: pragma

-- ANCHOR: module
module Samples where

-- JSON arrives as bytes.
-- JSON はバイト列でやってくる
import Data.ByteString.Lazy (ByteString)
-- ANCHOR_END: module

-- ANCHOR: values
-- 🧩 Small pieces of JSON, for looking at Value.
-- Value を見るための、小さな JSON
smallObject :: ByteString
smallObject = """
  {
    "name": "Slime",
    "hp": 30
  }
  """

mixedArray :: ByteString
mixedArray = """[1, "two", true, null, 2.5]"""
-- ANCHOR_END: values

-- ANCHOR: good
-- 👾 A monster written in JSON.
-- JSON で書いたモンスター
slimeJson :: ByteString
slimeJson = """
  {
    "name": "Slime",
    "hp": 30,
    "element": "Water",
    "drops": ["jelly"]
  }
  """
-- ANCHOR_END: good

-- ANCHOR: bad
-- 😵 Monsters with something wrong.
-- どこかがおかしいモンスターたち

-- No "hp".
-- "hp" がない
noHp :: ByteString
noHp = """
  {
    "name": "Slime",
    "element": "Water",
    "drops": []
  }
  """

-- "hp" is a string.
-- "hp" が文字列
textHp :: ByteString
textHp = """
  {
    "name": "Slime",
    "hp": "thirty",
    "element": "Water",
    "drops": []
  }
  """

-- An element that does not exist.
-- 存在しない属性
thunder :: ByteString
thunder = """
  {
    "name": "Raiju",
    "hp": 40,
    "element": "Thunder",
    "drops": []
  }
  """

-- The second drop is a number.
-- 2つ目のドロップ品が数
numberDrop :: ByteString
numberDrop = """
  {
    "name": "Slime",
    "hp": 30,
    "element": "Water",
    "drops": ["jelly", 3]
  }
  """

-- The JSON itself is cut off.
-- JSON そのものが途中で切れている
cutOff :: ByteString
cutOff = """
  {
    "name": "Slime",
    "hp": 30
  """
-- ANCHOR_END: bad

-- ANCHOR: extra
-- 🎁 A field that Monster does not know about.
-- Monster の知らないフィールドがある
extraField :: ByteString
extraField = """
  {
    "name": "Slime",
    "hp": 30,
    "element": "Water",
    "drops": [],
    "level": 5
  }
  """
-- ANCHOR_END: extra

-- ANCHOR: bounties
-- 💰 Bounties from the guild's web site.
-- ギルドの Web サイトから届いた賞金首
wolfJson :: ByteString
wolfJson = """
  {
    "target_name": "Shadow Wolf",
    "reward_gold": 300
  }
  """

wolfNullJson :: ByteString
wolfNullJson = """
  {
    "target_name": "Shadow Wolf",
    "reward_gold": 300,
    "note": null
  }
  """

wolfCamelJson :: ByteString
wolfCamelJson = """
  {
    "targetName": "Shadow Wolf",
    "reward_gold": 300
  }
  """
-- ANCHOR_END: bounties

-- ANCHOR: items
-- 🧪 Items from the shop's price list.
-- 道具屋の値段表から届いた品物
potionJson :: ByteString
potionJson = """
  {
    "name": "potion",
    "kind": "medicine",
    "price": 50,
    "stock": 3
  }
  """

swordJson :: ByteString
swordJson = """
  {
    "name": "sword",
    "kind": "weapon",
    "price": 300
  }
  """
-- ANCHOR_END: items

-- ANCHOR: badItems
-- 😵 Items with something wrong.
-- どこかがおかしい品物
negativePrice :: ByteString
negativePrice = """
  {
    "name": "potion",
    "kind": "medicine",
    "price": -50
  }
  """

unknownKind :: ByteString
unknownKind = """
  {
    "name": "bread",
    "kind": "food",
    "price": 10
  }
  """

priceList :: ByteString
priceList = """
  [
    {"name": "potion", "kind": "medicine", "price": 50},
    {"name": "bomb",   "kind": "weapon",   "price": -1}
  ]
  """
-- ANCHOR_END: badItems

-- ANCHOR: actions
-- ⚔️ A battle log written in JSON.
-- JSON で書いたバトルログ
actionsJson :: ByteString
actionsJson = """
  [
    {"type": "attack", "value": 12},
    {"type": "defend"}
  ]
  """

jumpJson :: ByteString
jumpJson = """
  [
    {"type": "attack", "value": 12},
    {"type": "jump"}
  ]
  """
-- ANCHOR_END: actions

-- ANCHOR: save
-- 💾 A save file. We only want a little of it.
-- セーブデータ。ほしいのは、ほんの一部だけ
saveJson :: ByteString
saveJson = """
  {
    "version": 3,
    "hero": {
      "name": "Lambda",
      "level": 12,
      "items": ["potion"]
    },
    "flags": {
      "metKing": true,
      "openedGate": false
    }
  }
  """
-- ANCHOR_END: save
-- ANCHOR_END: all
