-- ANCHOR: all
-- | The rules of reading: which moves are allowed from which state.
-- 読書の状態の決まり：どの状態から、どこへ進めるか
-- ANCHOR: header
module Tsundoku.Reading
  ( moves
  , allowed
  , advance
  ) where

import Tsundoku.Types (Event (..), Mistake (..), ReadingState (..))
-- ANCHOR_END: header

-- ANCHOR: moves
-- 🗺️ The map of reading: from each state, which event leads where.
--    No `_` case, so a new state is a warning until it gets its line.
-- 読書の地図：それぞれの状態から、どのできごとで、どこへ行けるか。
-- `_` の場合を書かないので、状態を足すと、その行を書くまで警告が出る
moves :: ReadingState -> [(Event, ReadingState)]
moves state = case state of
  Piled     -> [(Start , Reading )]
  Reading   -> [(Finish, Finished), (Stop, Paused  )]
  Paused    -> [(Resume, Reading )]
  Finished  -> [(Reread, Rereading)]
  Rereading -> [(Finish, Finished), (Stop, Finished)]
-- ANCHOR_END: moves

-- ANCHOR: advance
-- 🔘 What can I do now? (A screen can show just these buttons.)
-- いま、何ができる？（画面は、このボタンだけを出せばよい）
allowed :: ReadingState -> [Event]
allowed state = map fst (moves state)

-- ➡️ Take one step on the map, or say why not.
-- 地図の上を1歩進む。進めなければ、そのわけを返す
advance :: Event -> ReadingState -> Either Mistake ReadingState
advance event state =
  case lookup event (moves state) of
    Just next -> Right next
    Nothing   -> Left (NotAllowed event state)
-- ANCHOR_END: advance
-- ANCHOR_END: all
