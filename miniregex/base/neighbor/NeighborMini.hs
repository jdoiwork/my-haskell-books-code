-- ANCHOR: all
-- The jobs from the guild of the next town, with only the
-- import changed to MiniRegex.
-- 隣町のギルドの仕事。import だけを MiniRegex に変えた
-- This package turns OverloadedStrings on; here it is off again, so
-- that "…" is a String, as in the code of the next town.
-- このパッケージは OverloadedStrings を有効にしているが、ここでは外す。
-- 隣町のコードと同じく、"…" を String にするため
{-# LANGUAGE NoOverloadedStrings #-}
module NeighborMini where

-- ANCHOR: import
import Text.Regex.MiniRegex
-- ANCHOR_END: import

-- ANCHOR: jobs
-- 💰 Does the line say how much gold?
-- その行に、金貨の額が書いてあるか
hasGold :: String -> Bool
hasGold line = line =~ "[0-9]+ gold"

-- 📅 Every date in the line.
-- その行の日付を全部
datesOf :: String -> [String]
datesOf line = getAllTextMatches (line =~ "[0-9]{4}-[0-9]{2}-[0-9]{2}")

-- 🧾 The first sale: before it, the sale, after it, and the groups
--    (the seller and the number).
-- 最初の売り上げ：その前、売り上げ、そのうしろ、グループ（売った人と数）
firstSale :: String -> (String, String, String, [String])
firstSale line = line =~ "([A-Z][a-z]+) sold ([0-9]+)"
-- ANCHOR_END: jobs
-- ANCHOR_END: all
