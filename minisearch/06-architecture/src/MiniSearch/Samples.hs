-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | A few documents to try the engine in GHCi.
-- GHCi で試すための、いくつかの文書。
module MiniSearch.Samples
  ( sampleDocs
  , sampleIndex
  ) where

import MiniSearch.Index (buildIndex)
import MiniSearch.Types

-- 📜 The guild library: six short notes about monsters and places.
--    They are not files, so their ids are just "1" to "6".
-- ギルドの書庫：モンスターと場所についての、短い6つの覚え書き。
-- ファイルではないので、番号は "1" から "6" と手で付けた
sampleDocs :: [Document]
sampleDocs =
  -- ANCHOR: first
  [ Document (DocId "1") "Red Dragon"
      "The red dragon sleeps on a mountain of gold. \
      \Its fire melts iron, and its roar shakes the valley."
  -- ANCHOR_END: first
  , Document (DocId "2") "Slime"
      "A slime is soft and blue. It eats moss in the cave \
      \and splits in two when you hit it."
  , Document (DocId "3") "Ice Dragon"
      "The ice dragon lives at the top of the frozen mountain. \
      \Fire is its weakness."
  , Document (DocId "4") "Fire Cave"
      "Lava flows in the fire cave. Bring water and an ice shield."
  , Document (DocId "5") "Goblin Camp"
      "Goblins steal gold from travelers and hide it in the forest."
  , Document (DocId "6") "Dragon Tamer"
      "A dragon tamer speaks to a dragon with a flute. \
      \A calm dragon is a friend, an angry dragon is fire."
  ]

-- 📚 The index of the six notes, ready to search in GHCi.
-- 6つの覚え書きのインデックス（GHCi ですぐ検索できるように）
sampleIndex :: Index
sampleIndex = buildIndex sampleDocs
-- ANCHOR_END: all
