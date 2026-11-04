{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Cut a text into tokens.
-- 文章を、単語（トークン）に切り分ける。
module MiniSearch.Tokenize
  ( tokenize
  , stopWords
  ) where

import Data.Char (isAlphaNum)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import MiniSearch.Types (Token (..))

-- ANCHOR: tokenize
-- ✂️ "The red Dragon, of fire!" -> [red, dragon, fire]
tokenize :: Text -> [Token]
tokenize text =
  let lowered = T.toLower text
      -- Cut at every character that is not a letter or a digit.
      -- 文字でも数字でもないところで切る
      pieces  = T.split (not . isAlphaNum) lowered
      -- Cutting leaves empty pieces between ", ", so drop them.
      -- ", " のところには空の切れ端が残るので、捨てる
      words'  = filter (not . T.null) pieces
      useful  = filter (`Set.notMember` stopWords) words'
  in map Token useful

-- 🚫 Words too common to help a search.
-- よく出すぎて、検索の役に立たない単語
stopWords :: Set Text
stopWords = Set.fromList
  [ "a", "an", "and", "the", "of", "on", "in", "to"
  , "is", "it", "its", "at", "with", "for" ]
-- ANCHOR_END: tokenize
-- ANCHOR_END: all
