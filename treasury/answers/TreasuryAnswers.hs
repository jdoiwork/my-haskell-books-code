{-# LANGUAGE OverloadedStrings #-}
-- 🏋️ The answers of the training.
-- トレーニングの解答
module TreasuryAnswers where

import Control.Exception
import Data.IORef
import Data.Text (Text)
import Data.Text.IO qualified as TIO

import Vault (VaultAccident (..))

-- ANCHOR: notice
-- 📜 Read today's notice. Could not read it? Say there is none.
-- 今日のお知らせを読む。読めなければ「お知らせなし」
readNotice :: FilePath -> IO Text
readNotice path = handle noNotice (TIO.readFile path)
  where
    noNotice :: IOException -> IO Text
    noNotice _ = pure "(no notice today)"
-- ANCHOR_END: notice

-- ANCHOR: lamp
-- 🏮 Light the lamp, do the work, and always put the lamp out.
-- ランプをつけて仕事をし、何があってもランプを消す
withLamp :: IO a -> IO a
withLamp work = bracket_ (putStrLn "lamp: on") (putStrLn "lamp: off") work
-- ANCHOR_END: lamp

-- ANCHOR: stiff
-- 🚪 A stiff door: it jams until the n-th try. (Given; the inside
--    uses an IORef, which you do not need to read now.)
-- 固い扉：n 回目まで動かない（問題に付けた道具。中で使っている
-- IORef は、いまは読まなくて大丈夫）
stiffDoor :: Int -> IO (IO ())
stiffDoor n = do
  tries <- newIORef (0 :: Int)
  pure $ do
    modifyIORef' tries (+ 1)
    t <- readIORef tries
    if t < n
      then throwIO DoorJammed
      else putStrLn ("door: open (try " <> show t <> ")")
-- ANCHOR_END: stiff

-- ANCHOR: retry
-- 🔁 Try the work up to `times` times, while the vault has accidents.
-- 宝物庫の事故が起きるあいだ、times 回まで試し直す
retry :: Int -> IO a -> IO a
retry times work
  | times <= 1 = work
  | otherwise  = do
      result <- try @VaultAccident work
      case result of
        Right a -> pure a
        Left e  -> do
          putStrLn ("  again, after " <> show e)
          retry (times - 1) work
-- ANCHOR_END: retry
