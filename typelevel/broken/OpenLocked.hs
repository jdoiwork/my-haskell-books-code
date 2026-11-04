-- | This file must NOT compile: the chest is still locked.
-- このファイルはコンパイルできないはず：宝箱は、まだ鍵がかかっている
module OpenLocked where

import Chest

-- ANCHOR: openLocked
grab :: [String]
grab = open (newChest ["potion", "sword"])
-- ANCHOR_END: openLocked
