-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- ANCHOR: module
-- Gold without (..): other modules cannot write Gold (-5).
-- Gold には (..) を付けない：ほかのモジュールでは Gold (-5) と書けない
module Shop
  ( Gold
  , mkGold
  , Kind (..)
  , Item (..)
  ) where

-- JSON: the classes and the tools to write instances by hand.
-- JSON の型クラスと、instance を手で書くための道具
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , object
  , withObject
  , withText
  , (.!=)
  , (.:)
  , (.:?)
  , (.=)
  )

-- Text.
-- テキスト
import Data.Text (Text)
import qualified Data.Text as T
-- ANCHOR_END: module

-- ANCHOR: gold
-- 🪙 Gold. Only mkGold can make it, so it is never negative.
-- ゴールド。mkGold でしか作らないので、マイナスにならない
newtype Gold = Gold Int
  deriving stock (Show, Eq)

-- 🛡️ No negative gold.
-- マイナスのゴールドは作らない
mkGold :: Int -> Maybe Gold
mkGold n
  | n >= 0    = Just (Gold n)
  | otherwise = Nothing
-- ANCHOR_END: gold

-- ANCHOR: goldJson
-- 📥 Read Gold from a JSON number, through the gate of mkGold.
-- JSON の数から Gold を読む。mkGold の門を通す
instance FromJSON Gold where
  parseJSON value = do
    n <- parseJSON value
    case mkGold n of
      Just gold -> pure gold
      Nothing   -> fail ("gold must not be negative, but got " ++ show n)

-- 📤 Write Gold as a plain JSON number.
-- Gold は、ただの JSON の数として書く
instance ToJSON Gold where
  toJSON (Gold n) = toJSON n
-- ANCHOR_END: goldJson

-- ANCHOR: kind
-- 🏷️ What kind of item it is.
-- 道具の種類
data Kind = Medicine | Weapon
  deriving stock (Show, Eq)

-- 📥 In JSON, the kind is a lowercase string: "medicine" or "weapon".
-- JSON では、種類は小文字の文字列（"medicine" か "weapon"）
instance FromJSON Kind where
  parseJSON = withText "Kind" $ \t ->
    case t of
      "medicine" -> pure Medicine
      "weapon"   -> pure Weapon
      _          -> fail ("unknown kind: " ++ T.unpack t)

-- 📤 Write the kind back as a lowercase string.
-- 小文字の文字列に戻して書く
instance ToJSON Kind where
  toJSON Medicine = String "medicine"
  toJSON Weapon   = String "weapon"
-- ANCHOR_END: kind

-- ANCHOR: item
-- 🧪 An item in the shop.
-- 道具屋の品物
data Item = Item
  { name  :: Text
  , kind  :: Kind
  , price :: Gold
  , stock :: Int
  }
  deriving stock (Show, Eq)
-- ANCHOR_END: item

-- ANCHOR: itemFromJSON
-- 📥 Read an item. "stock" may be missing: then it is 1.
-- 品物を読む。"stock" はなくてもよい（そのときは 1）
instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    itemName  <- o .:  "name"
    itemKind  <- o .:  "kind"
    itemPrice <- o .:  "price"
    itemStock <- o .:? "stock" .!= 1
    pure (Item itemName itemKind itemPrice itemStock)
-- ANCHOR_END: itemFromJSON

-- ANCHOR: itemToJSON
-- 📤 Write an item.
-- 品物を書く
instance ToJSON Item where
  toJSON item = object
    [ "name"  .= item.name
    , "kind"  .= item.kind
    , "price" .= item.price
    , "stock" .= item.stock
    ]
-- ANCHOR_END: itemToJSON
-- ANCHOR_END: all
