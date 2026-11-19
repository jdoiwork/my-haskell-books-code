-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Build the inverted index.
-- 転置インデックスを作る。
module MiniSearch.Index
  ( emptyIndex
  , addDocument
  , buildIndex
  , postingsOf
  , documentCount
  ) where

import qualified Data.Map as Map

import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types

-- ANCHOR: empty
-- 📭 An index with nothing in it.
-- 何も入っていないインデックス
emptyIndex :: Index
emptyIndex = Index
  { indexPostings = Map.empty
  , indexDocs     = Map.empty
  , indexLengths  = Map.empty
  }
-- ANCHOR_END: empty

-- ANCHOR: add
-- ➕ Put one document into the index.
-- 文書をひとつ、インデックスに入れる
addDocument :: Document -> Index -> Index
addDocument doc index =
  let tokens = tokenize (docTitle doc <> " " <> docBody doc)
      -- How many times each token appears in this document.
      -- この文書に、それぞれの単語が何回出てくるか
      counts = Map.fromListWith (+) [ (t, 1) | t <- tokens ]
      -- token -> (this document -> count)
      -- 単語 -> （この文書 -> 回数）
      mine   = Map.map (Map.singleton (docId doc)) counts
  in Index
       { indexPostings =
           Map.unionWith Map.union mine (indexPostings index)
       , indexDocs     =
           Map.insert (docId doc) doc (indexDocs index)
       , indexLengths  =
           Map.insert (docId doc) (length tokens) (indexLengths index)
       }

-- 🏗️ Put all the documents in, one by one.
-- 文書を全部、1つずつ入れる
buildIndex :: [Document] -> Index
buildIndex docs = foldr addDocument emptyIndex docs
-- ANCHOR_END: add

-- ANCHOR: lookup
-- 🔎 The postings of a token (empty if no document has it).
-- 単語の出てくる場所（どの文書にもなければ空）
postingsOf :: Token -> Index -> Postings
postingsOf token index =
  Map.findWithDefault Map.empty token (indexPostings index)

-- 🧮 How many documents are in the index.
-- インデックスに入っている文書の数
documentCount :: Index -> Int
documentCount index = Map.size (indexDocs index)
-- ANCHOR_END: lookup
-- ANCHOR_END: all
