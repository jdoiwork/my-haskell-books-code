-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the concurrency chapter.
-- 並行化する章のトレーニングの解答。
module MiniSearch.Answers
  ( duplicateIds
  , PrefixError (..)
  , lookupPrefix
  , reportingSource
  ) where

import Control.Concurrent.STM
import Control.Exception (IOException, displayException, throwIO, try)
import Data.Bits (shiftL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readHex)

import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: duplicates
-- 👯 The ids that more than one document has. Two different files
--    with the same id would make Map.union drop one of them.
-- 2つ以上の文書が持っている番号。ちがうファイルが同じ番号だと、
-- Map.union が片方を黙って捨ててしまう
duplicateIds :: [Document] -> [DocId]
duplicateIds docs =
  let counts = Map.fromListWith (+) [ (docId d, 1 :: Int) | d <- docs ]
  in Map.keys (Map.filter (> 1) counts)
-- ANCHOR_END: duplicates

-- ANCHOR: prefix
-- 🙅 Why a short id did not find one document.
-- 短い番号で、文書が1つに決まらなかったわけ
data PrefixError
  = BadPrefix          -- ^ not 4 to 16 hex digits / 16進の4〜16文字でない
  | NoMatch            -- ^ no id starts with it / それで始まる番号がない
  | Ambiguous [DocId]  -- ^ two or more do / 2つ以上ある
  deriving stock (Eq, Show)

-- 🔍 Find a document by the first hex digits of its id, like
--    git's short ids. The ids that start with "7e70" are the numbers
--    from 0x7e70000000000000 to 0x7e70ffffffffffff, so one range
--    query of the ordered Map finds them all.
-- git の短い ID のように、番号の頭の数文字で文書を探す。
-- "7e70" で始まる番号は、0x7e70000000000000 から 0x7e70ffffffffffff
-- までの数なので、順番のある Map の範囲を1回見れば、全部見つかる
lookupPrefix :: Text -> Map DocId a -> Either PrefixError (DocId, a)
lookupPrefix prefix m
  | len < 4 || len > 16 || not (T.all (`elem` hexDigits) prefix)
              = Left BadPrefix
  | otherwise =
      case Map.toList (Map.takeWhileAntitone (<= hi)
                         (Map.dropWhileAntitone (< lo) m)) of
        []      -> Left NoMatch
        [found] -> Right found
        many    -> Left (Ambiguous (map fst many))
  where
    len       = T.length prefix
    hexDigits = "0123456789abcdef" :: String
    -- The digits we do not have: 4 bits each.
    -- 足りない文字のぶんのビット（1文字4ビット）
    rest      = 4 * (16 - len)
    value     = case readHex (T.unpack prefix) of
                  [(n, "")] -> n
                  _         -> 0
    lo        = DocId (value `shiftL` rest)
    hi        = DocId ((value `shiftL` rest) + (1 `shiftL` rest - 1))
-- ANCHOR_END: prefix

-- ANCHOR: reporting
-- 📋 Wrap the source of documents, and remember how the last read
--    went: Nothing = it worked, Just message = it failed. The error
--    is thrown again, so the rebuild stops and the old index stays.
-- 文書の出どころを包んで、最後に読んだときの結果を覚えておく：
-- Nothing なら成功、Just メッセージなら失敗。エラーは投げ直すので、
-- 作り直しは止まり、古いインデックスが残る
reportingSource :: TVar (Maybe Text) -> IO [Document] -> IO [Document]
reportingSource status source = do
  result <- try source
  case result of
    Left err -> do
      let message = T.pack (displayException (err :: IOException))
      atomically (writeTVar status (Just message))
      throwIO err
    Right docs -> do
      atomically (writeTVar status Nothing)
      pure docs
-- ANCHOR_END: reporting
-- ANCHOR_END: all
