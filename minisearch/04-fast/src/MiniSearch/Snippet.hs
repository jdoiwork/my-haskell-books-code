-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | A short piece of the body around the first hit.
-- 最初に当たったところのまわりの、本文の短い抜粋。
module MiniSearch.Snippet (snippet) where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types (Token)

-- ✂️ Up to `width` words before and after the first hit,
--    with the hit words in [brackets].
-- 最初に当たった単語の前後 width 語まで。当たった単語は [ ] で囲む
snippet :: Int -> [Token] -> Text -> Text
snippet width tokens body =
  let ws       = T.words body
      -- "Dragon," is a hit for the token "dragon".
      -- "Dragon," は、トークン "dragon" に当たる
      isHit w  = any (`elem` tokens) (tokenize w)
      firstHit = fromMaybe 0 (findIndex isHit ws)
      start    = max 0 (firstHit - width)
      window   = take (2 * width + 1) (drop start ws)
      mark w   = if isHit w then "[" <> w <> "]" else w
      before   = if start > 0 then "... " else ""
      after    = if start + length window < length ws then " ..." else ""
  in before <> T.unwords (map mark window) <> after
-- ANCHOR_END: all
