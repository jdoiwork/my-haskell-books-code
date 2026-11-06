-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}

-- 🪧 The title of a quest, checked at the door.
-- 依頼の題名。門で確かめる
module Board.Title
  ( QuestTitle
  , parseQuestTitle
  , titleText
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Char  (isControl)
import Data.Text  (Text)
import Data.Text  qualified as T

-- 🪪 1 to 40 characters, with no control characters (like a newline).
--    The constructor is hidden: parseQuestTitle is the only door.
-- 1〜40文字で、改行などの制御文字を含まない。
-- コンストラクターは隠すので、入り口は parseQuestTitle だけ
newtype QuestTitle = QuestTitle Text
  deriving (Show, Eq)

parseQuestTitle :: Text -> Either String QuestTitle
parseQuestTitle raw
  | T.null title          = Left "the title is empty"
  | T.length title > 40   = Left "the title is too long"
  | T.any isControl title = Left "the title has a control character"
  | otherwise             = Right (QuestTitle title)
  where
    title = T.strip raw

titleText :: QuestTitle -> Text
titleText (QuestTitle t) = t

-- 📥 Reading JSON goes through the same door.
-- JSON から読むときも、同じ門を通る
instance FromJSON QuestTitle where
  parseJSON = withText "QuestTitle" (either fail pure . parseQuestTitle)

instance ToJSON QuestTitle where
  toJSON = toJSON . titleText
-- ANCHOR_END: all
