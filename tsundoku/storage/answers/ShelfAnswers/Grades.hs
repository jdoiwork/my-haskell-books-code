-- ANCHOR: all
-- | Step 2: a new view, grades from A to F. The model does not change.
-- ステップ2：新しい見せ方、A〜F の成績。モデルは変えない
-- ANCHOR: head
module ShelfAnswers.Grades
  ( renderGrade
  , parseGrade
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Tsundoku.Rating (Score, score, scoreValue)
import Tsundoku.Types (Checked, Mistake (..))
-- ANCHOR_END: head

-- ANCHOR: grades
-- 🎓 Show a score as a grade: 90 and up is A, below 60 is F.
-- 点を成績で見せる：90 点以上は A、60 点より下は F
renderGrade :: Score -> Text
renderGrade s
  | n >= 90   = "A"
  | n >= 80   = "B"
  | n >= 70   = "C"
  | n >= 60   = "D"
  | otherwise = "F"
  where
    n = scoreValue s

-- 📥 A grade stands for many scores, so reading it back has to pick
--    one: the middle of its range.
-- 成績1つは、たくさんの点の影。読み戻すときは、1つ選ぶしかない。
-- その範囲の真ん中にする
parseGrade :: Text -> Checked Score
parseGrade typed = case T.toUpper (T.strip typed) of
  "A" -> score 95
  "B" -> score 85
  "C" -> score 75
  "D" -> score 65
  "F" -> score 30
  _   -> Left (BadRating typed)
-- ANCHOR_END: grades
-- ANCHOR_END: all
