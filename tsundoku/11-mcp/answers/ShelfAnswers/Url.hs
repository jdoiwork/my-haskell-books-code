-- ANCHOR: all
-- ANCHOR: head
-- | Training, step 1: a URL kind. Here only the reader; the places to
--   add it to are in the chapter.
-- トレーニングのステップ1：URL の種類。ここには読み方だけを置く。
-- 足す場所は、本文に書いた
module ShelfAnswers.Url (readUrl) where

import Data.Text (Text)
import Data.Text qualified as T
-- ANCHOR_END: head

-- ANCHOR: readUrl
-- 🔗 A URL: starts with http:// or https://, has something after it,
--    and no spaces.
-- URL：http:// か https:// で始まり、その後ろに何かがあり、空白がない
readUrl :: Text -> Maybe Text
readUrl typed
  | T.any (== ' ') typed      = Nothing
  | hasRest "https://"        = Just typed
  | hasRest "http://"         = Just typed
  | otherwise                 = Nothing
  where
    hasRest scheme = case T.stripPrefix scheme typed of
      Just rest -> not (T.null rest)
      Nothing   -> False
-- ANCHOR_END: readUrl
-- ANCHOR_END: all
