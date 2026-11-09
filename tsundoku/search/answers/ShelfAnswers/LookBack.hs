-- ANCHOR: all
-- | Step 3: looking back at earlier pages in the middle of reading.
--   A copy of the states and the map, with one state and one event more.
-- ステップ3：読んでいる途中で、前のページを読み返す。
-- 状態と地図を写して、状態とできごとを1つずつ足したもの
-- ANCHOR: head
module ShelfAnswers.LookBack
  ( ReadingState (..)
  , Event (..)
  , moves
  , advance
  ) where
-- ANCHOR_END: head

-- ANCHOR: lookback
data ReadingState
  = Piled
  | Reading
  | LookingBack   -- looking back in the middle / 途中で読み返し中
  | Finished
  | Paused
  | Rereading
  deriving (Show, Eq, Ord, Enum, Bounded)

data Event
  = Start
  | Finish
  | Stop
  | Resume
  | Reread
  | LookBack      -- go back to earlier pages / 前のページに戻る
  deriving (Show, Eq, Ord, Enum, Bounded)

moves :: ReadingState -> [(Event, ReadingState)]
moves state = case state of
  Piled       -> [(Start , Reading )]
  Reading     -> [(Finish, Finished), (Stop, Paused), (LookBack, LookingBack)]
  LookingBack -> [(Resume, Reading )]
  Paused      -> [(Resume, Reading )]
  Finished    -> [(Reread, Rereading)]
  Rereading   -> [(Finish, Finished), (Stop, Finished)]

advance :: Event -> ReadingState -> Maybe ReadingState
advance event state = lookup event (moves state)
-- ANCHOR_END: lookback
-- ANCHOR_END: all
