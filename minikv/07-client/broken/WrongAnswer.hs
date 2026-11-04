-- | This file must NOT compile: the answer of GET may be missing.
-- このファイルはコンパイルできないはず：GET の答えは、ないかもしれない
module WrongAnswer where

import MiniKv.Client (Connection)
import MiniKv.Typed

-- ANCHOR: wrong
gold :: Key Int
gold = Key "gold"

doubled :: Connection -> IO (Either ClientError Int)
doubled conn = do
  answer <- run conn (Get gold)
  pure (fmap (* 2) answer)
-- ANCHOR_END: wrong
