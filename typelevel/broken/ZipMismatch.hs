-- | This file must NOT compile: three heroes, but only two weapons.
-- このファイルはコンパイルできないはず：勇者は3人、武器は2つ
module ZipMismatch where

import Vec

-- ANCHOR: zipMismatch
armed :: Vec Three (String, String)
armed = vzip party ("sword" :> "staff" :> VNil)
-- ANCHOR_END: zipMismatch
