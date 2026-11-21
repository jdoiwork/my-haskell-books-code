-- ANCHOR: all
-- Forgetting the type of the answer: =~ cannot know what to give back.
-- 答えの型を書き忘れた。=~ は、何を返せばよいかわからない
module NoAnswerType where

import Data.Text (Text)

import Text.Regex.MiniRegex

-- 💰 Is there gold in the line?
-- その行に金貨の話はあるか
hasGold :: Text -> IO ()
hasGold line = print (line =~ ("[0-9]+ gold" :: String))
-- ANCHOR_END: all
