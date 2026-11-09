-- ANCHOR: head
-- | Training, step 3: color the bar for the terminal.
-- トレーニングのステップ3：進捗バーに、端末の色を付ける
module ShelfAnswers.ColorBar
  ( colorBar
  ) where

import Data.Text (Text)

import Tsundoku.Bar (bar)
import Tsundoku.Progress (Progress, percent)
import Tsundoku.Types (Amount)
-- ANCHOR_END: head

-- ANCHOR: color
-- 🎨 The same bar, in red, yellow, or green by how far I have read.
--    Only the view changes: the progress stays as it is.
-- 同じバーを、進み具合で赤・黄・緑に。変わるのはビューだけで、
-- 進み具合はそのまま
colorBar :: Amount u => Int -> Progress u -> Text
colorBar width p = color <> bar width p <> reset
  where
    pct   = percent p
    color
      | pct < 34  = "\ESC[31m"   -- red / 赤
      | pct < 67  = "\ESC[33m"   -- yellow / 黄
      | otherwise = "\ESC[32m"   -- green / 緑
    reset = "\ESC[0m"
-- ANCHOR_END: color
