-- ANCHOR: all
-- | Scouts sent out at the same time: loose threads that get lost,
--   and a structured party that never leaves anyone behind.
-- 同時に送り出す偵察隊：迷子になるばらばらのスレッドと、
-- だれも置き去りにしない、構造化された偵察隊
module Modern.Scouts
  ( Report
  , scout
  , trap
  , looseParty
  , structuredParty
  , withLookout
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently, withAsync)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forever, void)
import Data.IORef (IORef, atomicModifyIORef')

-- ANCHOR: scout
-- 📒 Where the scouts write when they come back.
-- 偵察から戻った者が書きこむ帳面
type Report = IORef [String]

-- 🧭 A scout walks for some milliseconds, then reports.
-- 偵察：何ミリ秒か歩いてから、報告する
scout :: Report -> String -> Int -> IO String
scout report name millis = do
  threadDelay (millis * 1000)
  atomicModifyIORef' report (\names -> (name : names, ()))
  pure name

-- 🪤 A scout who falls into a trap after a while.
-- しばらくして、罠に落ちる偵察
trap :: Int -> IO String
trap millis = do
  threadDelay (millis * 1000)
  throwIO (userError "fell into a trap")
-- ANCHOR_END: scout

-- ANCHOR: loose
-- 🎈 Send everyone off with forkIO and return at once.
--    Nobody waits for them, and the trap's error goes nowhere.
-- forkIO で全員を送り出して、すぐに戻る。
-- だれも帰りを待たず、罠のエラーはどこにも届かない
looseParty :: Report -> IO ()
looseParty report = do
  _ <- forkIO (void (scout report "slime" 50))
  _ <- forkIO (void (trap 10))
  _ <- forkIO (void (scout report "goblin" 100))
  pure ()
-- ANCHOR_END: loose

-- ANCHOR: structured
-- 🧷 Send everyone off and wait for all of them. If one fails,
--    the others are called back (cancelled) and the error comes here.
-- 全員を送り出して、全員を待つ。1人が失敗したら、
-- ほかの者は呼び戻され（キャンセルされ）、エラーはここに届く
structuredParty :: Report -> IO (Either SomeException [String])
structuredParty report =
  try (mapConcurrently id
         [ scout report "slime" 50
         , trap 10
         , scout report "goblin" 100
         ])
-- ANCHOR_END: structured

-- ANCHOR: lookout
-- 🔭 A lookout keeps watching while we work; when the work ends,
--    withAsync calls the lookout back too.
-- 仕事のあいだ、見張りが見張り続ける。仕事が終わると、
-- withAsync が見張りも呼び戻す
withLookout :: Report -> IO a -> IO a
withLookout report work =
  withAsync lookout (\_ -> work)
  where
    lookout = forever $ do
      threadDelay 20000
      atomicModifyIORef' report (\names -> ("lookout" : names, ()))
-- ANCHOR_END: lookout
-- ANCHOR_END: all
