{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of "lens".
-- 「lens」のトレーニングの解答
module LensAnswers
  ( potionForEveryone
  , bigItems
  ) where

import Control.Lens
import Data.Aeson.Lens (key, values, _Integer, _String)
import Data.Text (Text)

import Guild

-- ANCHOR: potions
-- 🧪 Step 2: give every member one more potion.
-- ステップ2：仲間全員に、ポーションを1つずつ
potionForEveryone :: Party -> Party
potionForEveryone =
  over (members . traversed . bag . at "potion" . non 0) (+ 1)
-- ANCHOR_END: potions

-- ANCHOR: filtered
-- 🔎 Step 3: names of the items we have more than 3 of.
-- ステップ3：3個より多く持っているアイテムの名前
bigItems :: Text -> [Text]
bigItems json =
  json ^.. key "items" . values
         . filtered (\item -> item ^? key "count" . _Integer > Just 3)
         . key "name" . _String
-- ANCHOR_END: filtered
-- ANCHOR_END: all
