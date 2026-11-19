-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the design chapter.
-- 設計の章のトレーニングの解答。
module MiniSearch.Answers
  ( searchAny
  , documentFrequency
  , commonTokens
  ) where

import Data.List (nub, sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Data.Text (Text)

import MiniSearch.Index (postingsOf)
import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: any
-- 🌊 OR search: documents that have at least one of the tokens.
-- OR 検索：単語のどれか1つでも含む文書
searchAny :: Index -> Text -> [DocId]
searchAny index query =
  let tokens       = nub (tokenize query)
      postingsList = map (`postingsOf` index) tokens
  in Map.keys (Map.unions postingsList)
-- ANCHOR_END: any

-- ANCHOR: common
-- 🧾 In how many documents a token appears.
-- 単語が、いくつの文書に出てくるか
documentFrequency :: Token -> Index -> Int
documentFrequency token index = Map.size (postingsOf token index)

-- 🥇 The n tokens that appear in the most documents.
-- いちばん多くの文書に出てくる単語を n 個
commonTokens :: Int -> Index -> [(Token, Int)]
commonTokens n index =
  let counts = Map.toList (Map.map Map.size (indexPostings index))
  in take n (sortOn (\(t, c) -> (Down c, t)) counts)
-- ANCHOR_END: common
-- ANCHOR_END: all
