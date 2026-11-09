-- ANCHOR: all
-- | Training: read one row of CSV back into its fields: the other way of
--   csvField. A field in quotes may hold commas, and "" is one quote.
-- トレーニング：CSV の1行を、フィールドに読み戻す。csvField の逆。引用符で
-- くるんだフィールドはカンマを含んでよく、"" は引用符1つ
module ShelfAnswers.CsvIn (csvRow) where

import Data.Text (Text)
import Data.Text qualified as T

-- 📥 The fields of one row (without its line break).
-- 1行（改行は除く）のフィールド
csvRow :: Text -> [Text]
csvRow = go
  where
    go t = case T.uncons t of
      Just ('"', rest) -> quoted "" rest
      _ -> let (here, rest) = T.break (== ',') t
           in  here : next rest
    quoted acc t = case T.breakOn "\"" t of
      (piece, rest) -> case T.unpack (T.take 2 rest) of
        "\"\"" -> quoted (acc <> piece <> "\"") (T.drop 2 rest)
        _      -> (acc <> piece) : next (T.drop 1 rest)
    next rest = case T.uncons rest of
      Just (',', more) -> go more
      _                -> []
-- ANCHOR_END: all
