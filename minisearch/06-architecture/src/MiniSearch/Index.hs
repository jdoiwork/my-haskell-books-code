{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Build the inverted index (the faster version).
-- 転置インデックスを作る（速くした版）。
module MiniSearch.Index
  ( emptyIndex
  , addDocument
  , addDocumentWith
  , buildIndex
  , buildIndexWith
  , buildIndexParallel
  , buildIndexParallelWith
  , postingsOf
  , documentCount
  ) where

import Control.Parallel.Strategies (parMap, rdeepseq)
-- The strict Map: values are computed when they are put in.
-- 正格な Map：値は、入れたときに計算される
import qualified Data.Map.Strict as Map

import MiniSearch.Tokenizer (Tokenizer (..), english)
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
addDocumentWith :: Tokenizer -> Document -> Index -> Index
addDocumentWith tok doc index =
  let tokens = cut tok (docTitle doc <> " " <> docBody doc)
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
buildIndexWith :: Tokenizer -> [Document] -> Index
buildIndexWith tok docs =
  let tokenized = [ (doc, cut tok (docTitle doc <> " " <> docBody doc))
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

-- ANCHOR: parallel
-- ⚡ Cut the documents into chunks, build an index for each chunk
--    on its own core, then put them together.
-- 文書をかたまりに分け、かたまりごとのインデックスを、
-- それぞれのコアで作ってから、合わせる
buildIndexParallelWith :: Tokenizer -> Int -> [Document] -> Index
buildIndexParallelWith tok chunkSize docs =
  let parts = parMap rdeepseq (buildIndexWith tok) (chunksOf chunkSize docs)
  in mconcat parts

-- ✂️ [1..7] in chunks of 3 -> [[1,2,3],[4,5,6],[7]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (front, rest) = splitAt n xs
                in front : chunksOf n rest
-- ANCHOR_END: parallel

-- ANCHOR: shortcuts
-- 🇬🇧 Shortcuts for English (used by the tests and the measuring).
-- 英語の近道（テストと測定で使う）
addDocument :: Document -> Index -> Index
addDocument = addDocumentWith english

buildIndex :: [Document] -> Index
buildIndex = buildIndexWith english

buildIndexParallel :: Int -> [Document] -> Index
buildIndexParallel = buildIndexParallelWith english
-- ANCHOR_END: shortcuts

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
