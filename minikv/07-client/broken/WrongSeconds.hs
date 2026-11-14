-- | This file must NOT compile: a bare number where EXPIRE wants Seconds.
-- このファイルはコンパイルできないはず：
-- EXPIRE が Seconds を欲しい所に、ただの数
module WrongSeconds where

import Data.Text (Text)

import MiniKv.Client (Connection)
import MiniKv.Typed

-- ANCHOR: wrong
hero :: Key Text
hero = Key "hero"

forAMinute :: Connection -> IO (Either ClientError Bool)
forAMinute conn = run conn (Expire hero 60)
-- ANCHOR_END: wrong
