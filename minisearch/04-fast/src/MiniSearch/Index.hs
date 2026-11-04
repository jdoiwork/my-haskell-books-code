{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Build the inverted index (the faster version).
-- 転置インデックスを作る（速くした版）。
module MiniSearch.Index
  ( emptyIndex
  , addDocument
  , buildIndex
  , postingsOf
  , documentCount
  ) where

-- ANCHOR: import
-- The strict Map: values are computed when they are put in.
-- 正格な Map：値は、入れたときに計算される
import qualified Data.Map.Strict as Map
-- ANCHOR_END: import

import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types

-- 📭 An index with nothing in it.
-- 何も入っていないインデックス
emptyIndex :: Index
emptyIndex = Index
  { indexPostings = Map.empty
  , indexDocs     = Map.empty
  , indexLengths  = Map.empty
  }

-- 🔢 How many times each token appears.
-- それぞれの単語が何回出てくるか
countTokens :: [Token] -> Map.Map Token Int
countTokens tokens = Map.fromListWith (+) [ (t, 1) | t <- tokens ]

-- ANCHOR: add
-- ➕ Put one document into the index (for adding later, one by one).
-- 文書をひとつ入れる（あとから1つずつ足すとき用）
addDocument :: Document -> Index -> Index
addDocument doc index =
  let tokens = tokenize (docTitle doc <> " " <> docBody doc)
      mine   = Map.map (Map.singleton (docId doc)) (countTokens tokens)
  in Index
       { indexPostings =
           Map.unionWith Map.union mine (indexPostings index)
       , indexDocs     =
           Map.insert (docId doc) doc (indexDocs index)
       , indexLengths  =
           Map.insert (docId doc) (length tokens) (indexLengths index)
       }
-- ANCHOR_END: add

-- ANCHOR: build
-- 🏗️ Build the whole index at once: first collect every
--    (token, document, count), then group them by token.
-- インデックスを一度に作る：まず（単語、文書、回数）を全部集めて、
-- それから単語ごとにまとめる
buildIndex :: [Document] -> Index
buildIndex docs =
  let tokenized = [ (doc, tokenize (docTitle doc <> " " <> docBody doc))
                  | doc <- docs ]
      entries   = [ (t, [(docId doc, c)])
                  | (doc, tokens) <- tokenized
                  , (t, c) <- Map.toList (countTokens tokens) ]
      grouped   = Map.fromListWith (++) entries
  in Index
       { indexPostings = Map.map Map.fromList grouped
       , indexDocs     = Map.fromList [ (docId d, d) | d <- docs ]
       , indexLengths  =
           Map.fromList [ (docId d, length ts) | (d, ts) <- tokenized ]
       }
-- ANCHOR_END: build

-- 🔎 The postings of a token (empty if no document has it).
-- 単語の出てくる場所（どの文書にもなければ空）
postingsOf :: Token -> Index -> Postings
postingsOf token index =
  Map.findWithDefault Map.empty token (indexPostings index)

-- 🧮 How many documents are in the index.
-- インデックスに入っている文書の数
documentCount :: Index -> Int
documentCount index = Map.size (indexDocs index)
-- ANCHOR_END: all
