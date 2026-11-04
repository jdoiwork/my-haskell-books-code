{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The gatekeepers for what comes from outside: the query and the limit.
-- 外から来るものの門番：問い合わせと、件数。
module MiniSearch.Query
  ( SearchQuery
  , queryText
  , queryTokens
  , Limit
  , limitValue
  , QueryError (..)
  , parseSearchQuery
  , parseLimit
  , renderQueryError
  , maxQueryLength
  , maxQueryWords
  , maxLimit
  ) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import MiniSearch.Tokenizer (Tokenizer (..))
import MiniSearch.Types (Token)

-- ANCHOR: types
-- 🎫 A query that passed the gate. The constructor is not exported:
--    the only way to get one is parseSearchQuery.
-- 門を通った問い合わせ。コンストラクターは見せない：
-- 手に入れる方法は parseSearchQuery だけ
data SearchQuery = SearchQuery
  { queryText   :: Text
  , queryTokens :: [Token]
  }
  deriving (Eq, Show)

-- 🔢 How many results to return, from 1 to maxLimit.
-- 返す件数。1 から maxLimit まで
newtype Limit = Limit { limitValue :: Int }
  deriving (Eq, Show)

-- 🚨 Why a request was turned away.
-- 要求を断った理由
data QueryError
  = EmptyQuery
  | QueryTooLong Int
  | TooManyWords Int
  | NoSearchableWords
  | LimitOutOfRange Int
  deriving (Eq, Show)
-- ANCHOR_END: types

-- ANCHOR: limits
-- 📏 The upper limits.
-- 上限
maxQueryLength, maxQueryWords, maxLimit :: Int
maxQueryLength = 200
maxQueryWords  = 10
maxLimit       = 50
-- ANCHOR_END: limits

-- ANCHOR: parse
-- 🚪 The gate for the query text.
-- 問い合わせの文字の門番
parseSearchQuery :: Tokenizer -> Text -> Either QueryError SearchQuery
parseSearchQuery tok raw
  | T.null text                    = Left EmptyQuery
  | T.length text > maxQueryLength = Left (QueryTooLong (T.length text))
  | length tokens > maxQueryWords  = Left (TooManyWords (length tokens))
  | null tokens                    = Left NoSearchableWords
  | otherwise                      = Right (SearchQuery text tokens)
  where
    text   = T.strip raw
    tokens = nub (cut tok text)

-- 🚪 The gate for the limit. No limit given -> 10.
-- 件数の門番。指定がなければ 10
parseLimit :: Maybe Int -> Either QueryError Limit
parseLimit Nothing = Right (Limit 10)
parseLimit (Just n)
  | n < 1 || n > maxLimit = Left (LimitOutOfRange n)
  | otherwise             = Right (Limit n)
-- ANCHOR_END: parse

-- ANCHOR: render
-- 📣 A message for the client.
-- クライアントへのメッセージ
renderQueryError :: QueryError -> Text
renderQueryError err = case err of
  -- 「問い合わせ q が空です」
  EmptyQuery        -> "the query q is empty"
  -- 「問い合わせが長すぎます」
  QueryTooLong n    -> "the query is too long: " <> showT n
                         <> " characters (at most " <> showT maxQueryLength
                         <> ")"
  -- 「単語が多すぎます」
  TooManyWords n    -> "too many words: " <> showT n
                         <> " (at most " <> showT maxQueryWords <> ")"
  -- 「検索できる単語がありません」
  NoSearchableWords -> "no searchable words (only stop words?)"
  -- 「件数は 1 から 50 まで」
  LimitOutOfRange n -> "limit must be 1 to " <> showT maxLimit
                         <> ": " <> showT n
  where
    showT = T.pack . show
-- ANCHOR_END: render
-- ANCHOR_END: all
