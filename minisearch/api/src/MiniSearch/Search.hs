-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Search the index and rank the results.
-- インデックスを引いて、結果に順位を付ける。
module MiniSearch.Search
  ( search
  , searchTokens
  , matchingDocs
  , termFrequency
  , inverseDocFrequency
  , score
  ) where

import Data.List (nub, sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Data.Text (Text)

import MiniSearch.Index (documentCount, postingsOf)
import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types

-- ANCHOR: matching
-- 🤝 Documents that have ALL the tokens (AND search).
-- すべての単語を含む文書（AND 検索）
matchingDocs :: [Token] -> Index -> [DocId]
matchingDocs []     _     = []
matchingDocs tokens index =
  let postingsList = map (`postingsOf` index) tokens
      -- Keep only the documents that are in every postings.
      -- どの単語の出現場所にもある文書だけを残す
      common       = foldr1 Map.intersection postingsList
  in Map.keys common
-- ANCHOR_END: matching

-- ANCHOR: tfidf
-- 📈 TF: how much of this document is this token.
--    3 times in 100 tokens -> 0.03
-- TF：この文書のうち、どれくらいがこの単語か
termFrequency :: Token -> DocId -> Index -> Double
termFrequency token doc index =
  let count  = Map.findWithDefault 0 doc (postingsOf token index)
      len    = Map.findWithDefault 1 doc (indexLengths index)
  in fromIntegral count / fromIntegral len

-- 💎 IDF: how rare this token is among the documents.
--    In every document -> small. In only one -> large.
-- IDF：この単語が、文書の中でどれくらい珍しいか。
-- 全部の文書にある -> 小さい。1つだけ -> 大きい
inverseDocFrequency :: Token -> Index -> Double
inverseDocFrequency token index =
  let n  = documentCount index
      df = Map.size (postingsOf token index)
  in log (1 + fromIntegral n / fromIntegral (max 1 df))

-- 🏅 The score of a document: TF times IDF, added up.
-- 文書の点数：TF と IDF の積を、単語ごとに足す
score :: [Token] -> DocId -> Index -> Double
score tokens doc index =
  sum [ termFrequency t doc index * inverseDocFrequency t index
      | t <- tokens ]
-- ANCHOR_END: tfidf

-- ANCHOR: search
-- 🔍 Search: find, score, and sort (high score first).
-- 検索：見つけて、点を付けて、並べる（点の高い順）
search :: Index -> Text -> [Hit]
search index query = searchTokens index (nub (tokenize query))

-- 🔍 The same, for tokens that are already cut.
-- 同じことを、もう切ってあるトークンで
searchTokens :: Index -> [Token] -> [Hit]
searchTokens index tokens =
  let docs = matchingDocs tokens index
      hits = [ Hit d (titleOf d) (score tokens d index) | d <- docs ]
  in sortOn (\h -> (Down (hitScore h), hitDoc h)) hits
  where
    titleOf d = maybe "" docTitle (Map.lookup d (indexDocs index))
-- ANCHOR_END: search
-- ANCHOR_END: all
