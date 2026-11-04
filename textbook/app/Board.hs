{-# LANGUAGE OverloadedStrings #-}

-- ANCHOR: board
module Main (main) where

-- Keep only the values that are Just; sort by a key.
-- Just の値だけを残す。キーで並べ替える
import Data.Maybe (mapMaybe)
import Data.List  (sortOn)
import Data.Ord   (Down (..))

-- Text and its input and output
-- Text とその入出力
import Data.Text    qualified as T
import Data.Text.IO qualified as TIO

import QuestBoard

-- 📋 Read the notices from standard input, and show them
--    with the biggest reward first.
-- 標準入力から告知を読み、報酬の多い順に表示する
main :: IO ()
main = do
  input <- TIO.getContents
  let notices = mapMaybe parseNotice (T.lines input)
      sorted  = sortOn (\(_, reward, _) -> Down reward) notices
  -- 「=== クエスト掲示板 ===」
  TIO.putStrLn "=== Quest Board ==="
  mapM_ (\n -> TIO.putStrLn (formatNotice n)) sorted
-- ANCHOR_END: board
