-- | This file must NOT compile: an empty Vec has no head.
-- このファイルはコンパイルできないはず：空の Vec に先頭はない
module EmptyHead where

import Vec

-- ANCHOR: emptyHead
nobody :: String
nobody = vhead VNil
-- ANCHOR_END: emptyHead
