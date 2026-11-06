-- | This file does not compile on purpose: it ties the desk of quests to
--   the shelf of herbs.
-- わざとコンパイルできないファイル：依頼の仕分け台を、薬草の棚に結んでいる
module WrongMailbox where

import MiniMq.Guild
import MiniMq.Typed
import MiniMq.Wire

-- ANCHOR: wrong
openShop :: Conn -> IO ()
openShop conn = bindTo conn questBoard "quest.#" herbShelf
-- ANCHOR_END: wrong
