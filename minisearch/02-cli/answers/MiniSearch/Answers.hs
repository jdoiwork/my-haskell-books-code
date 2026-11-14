-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Answers to the training of the CLI chapter.
-- CLI の章のトレーニングの解答。
module MiniSearch.Answers
  ( Stats (..)
  , indexStats
  , formatStats
  , smallPositiveInt
  , readDocumentsSafe
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Options.Applicative (ReadM, eitherReader)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import MiniSearch.Cli (wholeNumber)
import MiniSearch.Load (toDocument)
import MiniSearch.Types
-- ANCHOR_END: head

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

-- 「8 個の文書、62 種類の単語、91 語」
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
  case wholeNumber s of
    Just n
      | n < 1     -> Left ("not a positive number: " <> s)
      | n > maxN  -> Left ("too many (at most " <> show maxN <> "): " <> s)
      | otherwise -> Right n
    Nothing -> Left ("not a number: " <> s)
-- ANCHOR_END: small

-- ANCHOR: safe
-- 🛡️ Like readDocuments, but a problem is a Left with the file's name.
-- readDocuments と同じ。ただし、困ったときはファイル名つきの Left
readDocumentsSafe :: FilePath -> IO (Either String [Document])
readDocumentsSafe dir = do
  listed <- try (listDirectory dir)
  case listed of
    Left err    -> pure (Left (show (err :: IOException)))
    Right names -> do
      let txtNames = sort (filter (\n -> takeExtension n == ".txt") names)
      -- A folder named "sub.txt" is not a document.
      -- 「sub.txt」という名前のフォルダーは、文書ではない
      files    <- filterM (\n -> doesFileExist (dir </> n)) txtNames
      contents <- mapM (\n -> readUtf8 (dir </> n)) files
      pure (zipWith toDocument [1 ..] <$> sequence contents)

-- 📖 Read a file as UTF-8, now (not later, inside encode).
-- ファイルを UTF-8 として、いま読む（あとで encode の中で、ではなく）
readUtf8 :: FilePath -> IO (Either String Text)
readUtf8 path = do
  bytes <- try (BS.readFile path)
  pure $ case bytes of
    Left err -> Left (show (err :: IOException))
    Right bs -> case decodeUtf8' bs of
      Left _     -> Left (path <> ": not UTF-8")
      Right text -> Right text
-- ANCHOR_END: safe
-- ANCHOR_END: all
