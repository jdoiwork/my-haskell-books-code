-- ANCHOR: all
-- ANCHOR: module
-- 🌙 The night watch: appraise treasures, within a time limit.
-- 夜の見張り：時間を区切って、宝物を鑑定する
module Watch where

import Control.Concurrent (threadDelay)
import Control.Exception
import System.Timeout (timeout)
-- ANCHOR_END: module

-- ANCHOR: appraise
-- 💎 Appraise one treasure. Each takes 0.1 seconds.
-- 宝物を1つ鑑定する。1つ 0.1 秒かかる
appraise :: Int -> IO ()
appraise i = do
  putStrLn ("appraising #" <> show i)
  threadDelay 100000

-- ⏳ Appraise ten treasures, but stop after 0.35 seconds.
--    Nothing means "the time ran out".
-- 10個を鑑定する。ただし、0.35 秒で打ち切る
-- Nothing は「時間切れ」
nightAppraisal :: (Int -> IO ()) -> IO (Maybe ())
nightAppraisal appraiseOne =
  timeout 350000 (mapM_ appraiseOne [1 .. 10])
-- ANCHOR_END: appraise

-- ANCHOR: stubborn
-- 🙈 "Nothing stops the watch!" Catch every exception and go on.
--    (Careless: it also catches the time limit and Ctrl-C.)
-- 「見張りは止まらない！」例外を全部つかまえて、続ける
-- （うっかり版：時間切れも Ctrl-C も、つかまえてしまう）
appraiseStubborn :: Int -> IO ()
appraiseStubborn i =
  appraise i `catch` \ (e :: SomeException) ->
    putStrLn ("  shrugged off: " <> displayException e)
-- ANCHOR_END: stubborn

-- ANCHOR: catchSync
-- 🛡️ Catch only the accidents from inside the work.
--    Requests from outside to stop (asynchronous exceptions)
--    are thrown again, as they were.
-- 仕事の中から起きた事故だけを受け止める
-- 外からの「止まれ」（非同期例外）は、そのまま投げ直す
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync work handler =
  work `catch` \ e ->
    case fromException e of
      Just (_ :: SomeAsyncException) -> throwIO e
      Nothing                        -> handler e

-- 🛡️ Go on after an accident, but stop when asked to.
-- 事故があっても続ける。でも、止まれと言われたら止まる
appraiseCareful :: Int -> IO ()
appraiseCareful i =
  appraise i `catchSync` \ e ->
    putStrLn ("  shrugged off: " <> displayException e)
-- ANCHOR_END: catchSync
-- ANCHOR_END: all
