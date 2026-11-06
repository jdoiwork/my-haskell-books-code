-- | This file does not compile on purpose: a fourth way of handing out
--   was added to the type, but `matches` was not told about it.
-- わざとコンパイルできないファイル：配り方の型に4つ目を足したのに、
-- matches に教えていない
module HeadersKind where

import Data.Text (Text)

-- ANCHOR: forgot
data ExchangeKind = Fanout | Direct | Topic | Headers

matches :: ExchangeKind -> Text -> Text -> Bool
matches Fanout _       _   = True
matches Direct pattern key = pattern == key
matches Topic  pattern key = pattern == key   -- (short for the example)
-- ANCHOR_END: forgot
