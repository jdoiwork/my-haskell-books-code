-- ANCHOR: all
-- | Changing a Regex from outside: this does not compile, because
--   MiniRegex gives `source` as a plain function, not as the field of
--   the record (the field could make a Regex whose source and tree do
--   not agree).
-- 外から Regex を書き換える：これはコンパイルできない。MiniRegex は
-- source を、レコードのフィールドではなく、ふつうの関数として出すから
-- （フィールドだと、書いた文字列と木の食い違う Regex を作れてしまう）
module UpdateSource where

import MiniRegex

-- 🚫 compile "a", then try to say it was "[".
-- compile "a" したあとで、"[" だったことにしようとする
changed :: Either RegexError Regex
changed = do
  re <- compile "a"
  pure re { source = "[" }
-- ANCHOR_END: all
