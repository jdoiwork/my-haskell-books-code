-- ANCHOR: header
-- Let us write Text values as "..." literals.
-- "..." の書き方で Text の値を書けるようにする
{-# LANGUAGE OverloadedStrings #-}

module QuestBoard where

-- Text: efficient text. T.xxx: functions for Text.
-- Text：効率のよい文字列。T.xxx：Text 用の関数
import Data.Text (Text)
import Data.Text qualified as T

-- readMaybe: read a number safely (Nothing if it is not a number).
-- readMaybe：数を安全に読む（数でなければ Nothing）
import Text.Read (readMaybe)
-- ANCHOR_END: header


-- ANCHOR: parseNotice
-- 🔍 Read one line of the board, like this:
--      "[Urgent] Defeat 3 Slimes | reward: 120 G"
--    The result is (title, reward, urgent or not).
--    A line we cannot read gives Nothing.
-- 掲示板の1行を読む。結果は（タイトル, 報酬, 緊急かどうか）の組
-- 読めない行なら Nothing
parseNotice :: Text -> Maybe (Text, Int, Bool)
parseNotice line =
  case T.splitOn "|" (T.strip line) of
    [left, right] ->
      let rawTitle = T.strip left
          isUrgent = "[Urgent]" `T.isPrefixOf` rawTitle
          cleaned  = T.strip (T.replace "[Urgent]" "" rawTitle)
      in  case parseReward right of
            Just g  -> Just (cleaned, g, isUrgent)
            Nothing -> Nothing
    _ -> Nothing

-- 💰 Read the reward part: " reward: 120 G" -> Just 120
-- 報酬の部分を読む：" reward: 120 G" → Just 120
parseReward :: Text -> Maybe Int
parseReward part =
  case T.words part of
    ["reward:", amount, "G"] -> readMaybe (T.unpack amount)
    _                        -> Nothing
-- ANCHOR_END: parseNotice

-- ANCHOR: formatNotice
-- 🖨️ One line for the board: urgent quests are shouted in capitals.
--    "DEFEAT 3 SLIMES ......... 120 G"
-- 掲示板に貼る1行。緊急のクエストは大文字で叫ぶ
formatNotice :: (Text, Int, Bool) -> Text
formatNotice (title, reward, isUrgent) =
  let shown = if isUrgent then T.toUpper title else title
      left  = T.justifyLeft 34 '.' (shown <> " ")
      right = T.justifyRight 5 ' ' (T.pack (show reward))
  in  left <> right <> " G"
-- ANCHOR_END: formatNotice
