-- ANCHOR: all
-- ANCHOR: head
-- | Training, step 2: when a definition changes, which old values no
--   longer fit, and how to carry over the ones that do.
-- トレーニングのステップ2：定義が変わったら、どの古い値が合わなくなるか。
-- 合う値は、どう引き継ぐか
module ShelfAnswers.Redefine
  ( carryOver
  , misfits
  ) where

import Data.Map.Strict qualified as Map

import Tsundoku.Card (Card (..))
import Tsundoku.Custom (readValue, showValue)
import Tsundoku.Types
-- ANCHOR_END: head

-- ANCHOR: redefine
-- 🔁 Carry a value over to a new kind: show it as it would be typed,
--    and read it again with the new kind.
-- 値を、新しい種類に引き継ぐ：打ちこむときの文字にして、
-- 新しい種類で読み直す
carryOver :: AttrKind -> AttrValue -> Maybe AttrValue
carryOver kind value = readValue kind (showValue value)

-- 🚧 The works whose value would not fit the new definition.
-- 新しい定義に、値が合わなくなる作品
misfits :: AttrDef -> [(WorkId, Card)] -> [WorkId]
misfits def cards =
  [ wid
  | (wid, card) <- cards
  , Just value  <- [Map.lookup (attrName def) (attrs card)]
  , carryOver (attrKind def) value == Nothing
  ]
-- ANCHOR_END: redefine
-- ANCHOR_END: all
