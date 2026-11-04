{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- A sketch of aeson's Value, written only with what we have learned.
-- The real Value is a little different (see the book).
-- aeson の Value を、これまでの知識だけで書いた見取り図
-- 本物の Value とは少し違う（本文を参照）
module ValueSketch where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text       (Text)

-- ANCHOR: value
-- 🧩 One JSON value: one of these six.
-- JSON の値1つ：この6つのどれか
data Value
  = Object (Map Text Value)
  | Array  [Value]
  | String Text
  | Number Double
  | Bool   Bool
  | Null
  deriving (Show, Eq)
-- ANCHOR_END: value

-- ANCHOR: example
-- 🐉 {"name": "Slime", "hp": 30, "drops": ["jelly"]} as a Value
-- {"name": "Slime", "hp": 30, "drops": ["jelly"]} を Value で書くと
slime :: Value
slime =
  Object (Map.fromList
    [ ("name" , String "Slime")
    , ("hp"   , Number 30)
    , ("drops", Array [String "jelly"])
    ])
-- ANCHOR_END: example
-- ANCHOR_END: all
