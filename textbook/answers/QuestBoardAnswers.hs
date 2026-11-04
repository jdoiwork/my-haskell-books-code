{-# LANGUAGE OverloadedStrings #-}
module QuestBoardAnswers where

-- Text: efficient text. T.xxx: functions for Text.
-- Text：効率のよい文字列。T.xxx：Text 用の関数
import Data.Text (Text)
import Data.Text qualified as T

-- isDigit: is this character a digit?
-- isDigit：この文字は数字？
import Data.Char (isDigit)


-- ANCHOR: countUrgent
-- 🚨 How many urgent quests are there?
-- 緊急のクエストはいくつ？
countUrgent :: [(Text, Int, Bool)] -> Int
countUrgent notices = length (filter (\(_, _, isUrgent) -> isUrgent) notices)
-- ANCHOR_END: countUrgent

-- ANCHOR: totalReward
-- 💰 The total reward of all the quests.
-- すべてのクエストの報酬の合計
totalReward :: [(Text, Int, Bool)] -> Int
totalReward notices = sum (map (\(_, reward, _) -> reward) notices)
-- ANCHOR_END: totalReward

-- ANCHOR: titleList
-- 📋 All the titles in one line, separated by ", ".
-- すべてのタイトルを、", " で区切って1行にする
titleList :: [(Text, Int, Bool)] -> Text
titleList notices = T.intercalate ", " (map (\(title, _, _) -> title) notices)
-- ANCHOR_END: titleList

-- ANCHOR: removeDigits
-- 🧽 Remove every digit from a text.
-- テキストから数字をすべて取り除く
removeDigits :: Text -> Text
removeDigits text = T.filter (\c -> not (isDigit c)) text
-- ANCHOR_END: removeDigits
