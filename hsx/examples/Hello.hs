-- ANCHOR: all
{-# LANGUAGE QuasiQuotes #-}
-- | The smallest hsx: a greeting.
-- いちばん小さな hsx：あいさつ
module Hello (greeting) where

import Data.Text (Text)

import Hsx

-- 👋 HTML with a hole for the name.
-- 名前の穴がある HTML
greeting :: Text -> Html
greeting name = [hsx|<p class="hello">Hello, {name}!</p>|]
-- ANCHOR_END: all
