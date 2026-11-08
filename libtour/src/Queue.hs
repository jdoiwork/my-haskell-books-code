-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- 🔄 The turn order of a battle: a Seq, fast at both ends.
-- バトルの行動順：両端の出し入れが速い Seq
module Queue where

import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
-- ANCHOR_END: imports

-- ANCHOR: queue
-- 👥 Who acts first, second, third...
-- 行動する順番
party :: Seq Text
party = Seq.fromList ["Lambda", "Python", "TS", "Rust"]

-- ⏭️ The next one acts, then goes to the back of the line.
-- 先頭の人が行動して、列の後ろに並び直す
nextTurn :: Seq a -> Maybe (a, Seq a)
nextTurn queue =
  case queue of
    first :<| rest -> Just (first, rest :|> first)
    Empty          -> Nothing
-- ANCHOR_END: queue

-- ANCHOR: lineInfo
-- 📋 Who is at the front, and who is at the back?
-- 列の先頭は誰で、最後は誰？
lineInfo :: Seq Text -> Text
lineInfo queue =
  case queue of
    -- 「誰も並んでいない」
    Empty                     -> "No one is in line."
    -- 「〇〇だけが並んでいる」
    only :<| Empty            -> only <> " is the only one."
    -- 「先頭は〇〇、最後は〇〇」
    front :<| (_ :|> back)    -> front <> " is first, " <> back <> " is last."
-- ANCHOR_END: lineInfo
-- ANCHOR_END: all
