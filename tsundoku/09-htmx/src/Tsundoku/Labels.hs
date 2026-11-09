-- ANCHOR: all
-- | A view: the words a person reads for reading states and events.
--   The command line and the web pages both use them.
-- ビュー：読書の状態とできごとを、人が読む言葉に。
-- コマンドの画面も、Web の画面も、これを使う
module Tsundoku.Labels
  ( stateLabel
  , eventLabel
  ) where

import Data.Text (Text)

import Tsundoku.Types

-- 🏷️ How a reading state is shown.
-- 読書の状態の見せ方
stateLabel :: ReadingState -> Text
stateLabel state = case state of
  Piled     -> "積読"
  Reading   -> "読書中"
  Finished  -> "読了"
  Paused    -> "中断"
  Rereading -> "再読"

-- 🔘 The word on the button of each event.
-- できごとのボタンに書く言葉
eventLabel :: Event -> Text
eventLabel event = case event of
  Start  -> "読み始める"
  Finish -> "読み終えた"
  Stop   -> "中断する"
  Resume -> "再開する"
  Reread -> "読み返す"
-- ANCHOR_END: all
