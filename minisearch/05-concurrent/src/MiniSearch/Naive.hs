{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The first, simple version of building and searching, kept as the
--   reference: the fast version must give the same answers.
-- 最初の素朴な版。お手本として残す：
-- 速い版は、同じ答えを返さなければならない。
module MiniSearch.Naive
  ( buildIndexNaive
  , searchNaive
  ) where

import Data.List (nub, sortOn)
import qualified Data.Map as Map
import Data.Ord (Down (..))
import Data.Text (Text)

import MiniSearch.Index (documentCount, emptyIndex, postingsOf)
import MiniSearch.Tokenize (tokenize)
import MiniSearch.Types

-- ➕ Put one document into the index (lazy Map).
-- 文書をひとつ入れる（遅延な Map）
addDocumentNaive :: Document -> Index -> Index
addDocumentNaive doc index =
  let tokens = tokenize (docTitle doc <> " " <> docBody doc)
      counts = Map.fromListWith (+) [ (t, 1) | t <- tokens ]
      mine   = Map.map (Map.singleton (docId doc)) counts
  in Index
       { indexPostings =
           Map.unionWith Map.union mine (indexPostings index)
       , indexDocs     =
           Map.insert (docId doc) doc (indexDocs index)
       , indexLengths  =
           Map.insert (docId doc) (length tokens) (indexLengths index)
       }

-- 🏗️ foldr over the documents.
-- 文書を foldr で入れる
buildIndexNaive :: [Document] -> Index
buildIndexNaive docs = foldr addDocumentNaive emptyIndex docs

-- 🔍 Score every match, sort them all.
-- 当たった文書すべてに点を付け、全部を並べる
searchNaive :: Index -> Text -> [Hit]
searchNaive index query =
  let tokens = nub (tokenize query)
      docs   = matching tokens
      hits   = [ Hit d (titleOf d) (score tokens d) | d <- docs ]
  in sortOn (\h -> (Down (hitScore h), hitDoc h)) hits
  where
    titleOf d = maybe "" docTitle (Map.lookup d (indexDocs index))
    matching [] = []
    matching ts = Map.keys (foldr1 Map.intersection
                                   (map (`postingsOf` index) ts))
    score ts d = sum [ tf t d * idf t | t <- ts ]
    tf t d =
      let count = Map.findWithDefault 0 d (postingsOf t index)
          len   = Map.findWithDefault 1 d (indexLengths index)
      in fromIntegral count / fromIntegral (len :: Int)
    idf t =
      let n  = documentCount index
          df = Map.size (postingsOf t index)
      in log (1 + fromIntegral n / fromIntegral (max 1 df))
-- ANCHOR_END: all
