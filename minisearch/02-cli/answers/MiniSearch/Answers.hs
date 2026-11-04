{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | Answers to the training of the CLI chapter.
-- CLI の章のトレーニングの解答。
module MiniSearch.Answers
  ( Stats (..)
  , indexStats
  , formatStats
  , smallPositiveInt
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative (ReadM, eitherReader)

import MiniSearch.Types

-- ANCHOR: stats
-- 📊 The size of an index.
-- インデックスの大きさ
data Stats = Stats
  { statDocuments :: Int  -- documents / 文書の数
  , statTokens    :: Int  -- different tokens / 単語の種類
  , statWords     :: Int  -- all tokens / 単語の総数
  }
  deriving (Eq, Show)

indexStats :: Index -> Stats
indexStats index = Stats
  { statDocuments = Map.size (indexDocs index)
  , statTokens    = Map.size (indexPostings index)
  , statWords     = sum (Map.elems (indexLengths index))
  }

-- 「8 個の文書、42 種類の単語、66 語」
formatStats :: Stats -> Text
formatStats s =
  T.pack (show (statDocuments s)) <> " documents, "
    <> T.pack (show (statTokens s)) <> " different tokens, "
    <> T.pack (show (statWords s)) <> " tokens\n"
-- ANCHOR_END: stats

-- ANCHOR: small
-- 🔢 A number from 1 to the given maximum.
-- 1 から、指定した上限までの数
smallPositiveInt :: Int -> ReadM Int
smallPositiveInt maxN = eitherReader $ \s ->
  case reads s of
    [(n, "")]
      | n < 1     -> Left ("not a positive number: " <> s)
      | n > maxN  -> Left ("too many (at most " <> show maxN <> "): " <> s)
      | otherwise -> Right n
    _ -> Left ("not a number: " <> s)
-- ANCHOR_END: small
-- ANCHOR_END: all
