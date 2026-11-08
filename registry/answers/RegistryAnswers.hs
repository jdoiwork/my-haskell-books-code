-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- Answers for the training of the error handling chapter.
-- エラー処理の章のトレーニングの解答
module RegistryAnswers
  ( maybeToEither
  , validateJob2
  , readGold
  ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Read (readMaybe)

import Registry (Job, RegistrationError (..))
-- ANCHOR_END: imports

-- ANCHOR: maybeToEither
-- 🔁 Turn a Maybe into an Either: Nothing gets a reason.
-- Maybe を Either に変える。Nothing に理由を付ける
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing  = Left err
maybeToEither _   (Just x) = Right x

-- 🔍 validateJob, in one line with maybeToEither.
-- maybeToEither を使って、validateJob を1行で
validateJob2 :: Text -> Either RegistrationError Job
validateJob2 raw =
  maybeToEither (UnknownJob raw) (readMaybe (T.unpack (T.strip raw)))
-- ANCHOR_END: maybeToEither

-- ANCHOR: readGold
-- 💰 Read the amount of gold written in a file.
--    Both "the file cannot be read" and "it is not a number"
--    become a Left with a reason.
-- ファイルに書かれたゴールドの量を読む
-- 「ファイルが読めない」も「数ではない」も、理由付きの Left にする
readGold :: FilePath -> IO (Either Text Int)
readGold path = do
  result <- try (TIO.readFile path) :: IO (Either IOException Text)
  case result of
    Left err ->
      pure (Left ("cannot read " <> T.pack path <> ": " <> T.pack (show err)))
    Right contents ->
      case readMaybe (T.unpack (T.strip contents)) of
        Nothing -> pure (Left ("not a number: " <> T.strip contents))
        Just n  -> pure (Right n)
-- ANCHOR_END: readGold
-- ANCHOR_END: all
