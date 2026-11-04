{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
-- ANCHOR: all
-- | The guild roster as an HTML page, written with hsx.
-- ギルドの名簿を、hsx で HTML のページにする。
module GuildPage
  ( Adventurer (..)
  , rosterPage
  , adventurerRow
  ) where

import Data.Text (Text)

import Hsx

-- ANCHOR: types
-- 🧝 One adventurer.
-- 冒険者ひとり
data Adventurer = Adventurer
  { name  :: Text
  , job   :: Text
  , level :: Int
  , title :: Maybe Text
  }
-- ANCHOR_END: types

-- ANCHOR: row
-- 📋 One row of the table.
-- 表の1行
adventurerRow :: Adventurer -> Html
adventurerRow a =
  let rowClass = if level a >= 10 then "veteran" else "rookie" :: Text
      who      = name a
      lv       = level a
      badge    = titleBadge <$> title a
  in [hsx|
    <tr class={rowClass}>
      <td>{who}{badge}</td>
      <td>{job a}</td>
      <td>{lv}</td>
    </tr>
  |]

-- 🎖️ A small badge for a title.
-- 称号の小さなバッジ
titleBadge :: Text -> Html
titleBadge t = [hsx|<span class="badge">{t}</span>|]
-- ANCHOR_END: row

-- ANCHOR: page
-- 🏰 The whole page.
-- ページ全体
rosterPage :: Text -> [Adventurer] -> Html
rosterPage guild adventurers =
  let rows  = map adventurerRow adventurers
      count = length adventurers
  in raw "<!doctype html>" <> [hsx|
    <html lang="ja">
      <head>
        <meta charset="utf-8">
        <title>{guild}</title>
      </head>
      <body>
        <h1>{guild}</h1>
        <p>Members: {count}</p>
        <table>
          <tr><th>Name</th><th>Job</th><th>Level</th></tr>
          {rows}
        </table>
      </body>
    </html>
  |]
-- ANCHOR_END: page
-- ANCHOR_END: all
