{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The HTML value that hsx builds, and the escaping rules.
-- hsx が組み立てる HTML の値と、エスケープの決まり。
module Hsx.Html
  ( Html
  , renderHtml
  , text
  , raw
  , ToHtml (..)
  , ToAttr (..)
  , escapeText
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- ANCHOR: html
-- 🧱 A piece of HTML. The constructor is hidden:
--    you cannot make an Html from a Text without escaping it.
-- HTML のかけら。コンストラクターは隠してある。
-- エスケープせずに Text から Html は作れない
newtype Html = Html [Text]

instance Semigroup Html where
  Html a <> Html b = Html (a <> b)

instance Monoid Html where
  mempty = Html []

-- 🖨️ Turn Html into the final Text.
-- Html を、できあがりの Text にする
renderHtml :: Html -> Text
renderHtml (Html chunks) = T.concat chunks
-- ANCHOR_END: html

-- ANCHOR: escape
-- 🛡️ Replace the five dangerous characters.
-- 危ない5つの文字を置き換える
escapeText :: Text -> Text
escapeText = T.concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c    = T.singleton c

-- 📝 Text that is shown as text (escaped).
-- 文字として見せる Text（エスケープする）
text :: Text -> Html
text t = Html [escapeText t]

-- ⚠️ Text that is trusted as HTML (not escaped). Use with care.
-- HTML として信じる Text（エスケープしない）。気をつけて使う
raw :: Text -> Html
raw t = Html [t]
-- ANCHOR_END: escape

-- ANCHOR: tohtml
-- 🎁 Things that can be put inside {...} between tags.
-- タグの間の {...} に入れられるもの
class ToHtml a where
  toHtml :: a -> Html

instance ToHtml Html where
  toHtml h = h

instance ToHtml Text where
  toHtml = text

instance ToHtml Int where
  toHtml n = text (T.pack (show n))

-- With this, a String (a list of Char) works too.
-- これで String（Char のリスト）も入れられる
instance ToHtml Char where
  toHtml c = text (T.singleton c)

-- A list becomes its pieces, one after another.
-- リストは、中身を順に並べたものになる
instance ToHtml a => ToHtml [a] where
  toHtml xs = mconcat (map toHtml xs)

-- Nothing shows nothing.
-- Nothing なら、何も出さない
instance ToHtml a => ToHtml (Maybe a) where
  toHtml Nothing  = mempty
  toHtml (Just x) = toHtml x

-- 🏷️ Things that can be an attribute value: class={...}
-- 属性の値にできるもの：class={...}
class ToAttr a where
  toAttr :: a -> Text

instance ToAttr Text where
  toAttr = escapeText

instance ToAttr Int where
  toAttr n = T.pack (show n)
-- ANCHOR_END: tohtml
-- ANCHOR_END: all
