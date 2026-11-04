{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module RecordsAnswers where

-- Text and its input and output
-- Text とその入出力
import Data.Text    (Text)
import Data.Text.IO qualified as TIO

import Records

-- ANCHOR: winsOf
-- 🥇 How many times has this hero won?
-- この勇者は何回勝った？
winsOf :: Text -> [Record] -> Int
winsOf name records =
  length [ r | r <- records, r.hero == name, r.outcome == Won ]
-- ANCHOR_END: winsOf

-- ANCHOR: clearRecords
-- 🧹 Empty the hall of fame by writing an empty text to the file.
-- 空のテキストを書き込んで、殿堂の記録を空っぽにする
clearRecords :: FilePath -> IO ()
clearRecords path = do
  TIO.writeFile path ""
  -- 「殿堂の記録は空っぽになりました」
  TIO.putStrLn "The hall of fame is now empty."
-- ANCHOR_END: clearRecords
