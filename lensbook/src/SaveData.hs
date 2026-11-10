-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Peeking into deep JSON with lens-aeson.
-- lens-aeson で、深い JSON をのぞく
module SaveData
  ( saveJson
  , heroLevel
  , itemNames
  , richer
  ) where

import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Text (Text)
-- ANCHOR_END: head

-- ANCHOR: sample
-- 💾 A save file: the hero, the party's items, and some flags.
-- セーブデータ：勇者、パーティーのアイテム、いくつかのフラグ
saveJson :: Text
saveJson = """
  {
    "version": 3,
    "hero": {
      "name": "Lambda",
      "stats": {
        "level": 12,
        "gold": 250
      }
    },
    "items": [
      {"name": "potion", "count": 3},
      {"name": "herb", "count": 5}
    ]
  }
  """
-- ANCHOR_END: sample

-- ANCHOR: peek
-- 🔍 Three levels down, in one path. Nothing if anything is missing.
-- 3段下を、1本の道順で。どこかがなければ Nothing
heroLevel :: Text -> Maybe Integer
heroLevel json = json ^? key "hero" . key "stats" . key "level" . _Integer

-- 📋 Every item's name.
-- アイテム全部の名前
itemNames :: Text -> [Text]
itemNames json = json ^.. key "items" . values . key "name" . _String

-- 💰 Add 100 gold, keeping everything else as it was.
-- ゴールドを 100 増やす。ほかは、そのまま
richer :: Text -> Maybe Value
richer json =
  over (key "hero" . key "stats" . key "gold" . _Integer) (+ 100)
    <$> (json ^? _Value)
-- ANCHOR_END: peek
-- ANCHOR_END: all
