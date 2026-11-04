-- | This file must NOT compile: INCR on a key that holds text.
-- このファイルはコンパイルできないはず：文字列をしまうキーに INCR
module WrongKey where

import Data.Text (Text)

import MiniKv.Client (Connection)
import MiniKv.Typed

-- ANCHOR: wrong
hero :: Key Text
hero = Key "hero"

levelUp :: Connection -> IO (Either ClientError Int)
levelUp conn = run conn (Incr hero)
-- ANCHOR_END: wrong
