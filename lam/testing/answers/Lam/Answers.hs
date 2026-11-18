-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Tests and debugging: lam test".
-- 「テストとデバッグ：lam test」のトレーニングの解答。
module Lam.Answers
  ( matching
  , readMatch
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.TestRun (Report (..))

-- ANCHOR: matching
-- 🔎 Step 3: lam test --match text. Keep only the tests whose names
--    (the group names too, joined with " > ") have the text in them.
-- ステップ3：名前（組の名前も " > " でつないだもの）に text が入っている
-- テストだけを残す
matching :: Text -> [Report] -> [Report]
matching text = filter has
  where
    has r = T.toLower text `T.isInfixOf`
              T.toLower (T.intercalate " > " (reportNames r))

-- 🚩 Take --match text (or --match=text) out of the words of lam test.
--    The rest goes to readTestArgs as before.
-- lam test の語から --match を取り出す。残りは、これまでどおり
-- readTestArgs へ
readMatch :: [String] -> (Maybe Text, [String])
readMatch ws = case ws of
  [] -> (Nothing, [])
  "--match" : t : rest -> (Just (T.pack t), snd (readMatch rest))
  w : rest
    | Just t <- T.stripPrefix "--match=" (T.pack w) ->
        (Just t, snd (readMatch rest))
    | otherwise -> let (m, more) = readMatch rest in (m, w : more)
-- ANCHOR_END: matching
-- ANCHOR_END: all
