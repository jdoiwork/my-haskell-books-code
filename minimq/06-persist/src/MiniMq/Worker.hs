-- ANCHOR: all
-- | A worker that does not try to be careful: it takes a letter, does the
--   job, and says ACK. If the job blows up, so does the worker.
-- 用心しない働き手：手紙を取り、仕事をして、ACK と言う。
-- 仕事が爆発したら、働き手もいっしょに倒れる
module MiniMq.Worker
  ( worker
  ) where

import Control.Monad (forever)
import Network.Socket (PortNumber)

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Types

-- ANCHOR: worker
-- 🧑‍🔧 No `try`, no `catch`: when `job` throws, the connection closes,
--    the broker puts the letter back (marked "again"), and the worker's
--    supervisor starts a fresh worker. Let it crash.
-- try も catch も書かない。job が例外を投げたら、つながりが閉じ、
-- ブローカーが手紙を（again の印を付けて）戻し、働き手の監督役が、
-- 新しい働き手を始める。倒れるなら、倒れればいい
worker :: PortNumber -> QueueName -> (Message -> IO ()) -> IO ()
worker port queue job = withClient port $ \conn -> do
  _ <- request conn (Consume queue 1)
  forever $ do
    delivery <- nextDelivery conn
    job (message delivery)
    answerWith Ack conn delivery
-- ANCHOR_END: worker
-- ANCHOR_END: all
