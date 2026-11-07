-- ANCHOR: all
-- | A worker that does not try to be careful: it takes a letter, does the
--   job, and says ACK. If the job blows up, so does the worker.
-- 用心しない働き手：手紙を取り、仕事をして、ACK と言う。
-- 仕事が爆発したら、働き手もいっしょに倒れる
-- ANCHOR: header
module MiniMq.Worker
  ( worker
  ) where

import Control.Monad (forever)
import Network.Socket (PortNumber)

import MiniMq.Client
import MiniMq.Protocol
import MiniMq.Types
-- ANCHOR_END: header

-- ANCHOR: worker
-- 🧑‍🔧 No `try`, no `catch`: when `job` throws, the connection closes,
--    the broker puts the letter back (marked "again"), and the worker's
--    supervisor starts a fresh worker. Let it crash. And when CONSUME is
--    refused (no such queue), fall over at once, out loud: a worker that
--    waits for letters that never come looks alive to its supervisor.
-- try も catch も書かない。job が例外を投げたら、つながりが閉じ、
-- ブローカーが手紙を（again の印を付けて）戻し、働き手の監督役が、
-- 新しい働き手を始める。倒れるなら、倒れればいい。CONSUME を断られたら
-- （そんな郵便受けはない）、すぐに、わかるように倒れる。来ない手紙を
-- 待ち続ける働き手は、監督役からは、元気に見えてしまう
worker :: PortNumber -> QueueName -> (Message -> IO ()) -> IO ()
worker port queue job = withClient port $ \conn -> do
  joined <- request conn (Consume queue 1)
  case joined of
    Ok    -> pure ()
    other -> fail ("cannot consume " <> show queue <> ": " <> show other)
  forever $ do
    delivery <- nextDelivery conn
    job (message delivery)
    answerWith Ack conn delivery
-- ANCHOR_END: worker
-- ANCHOR_END: all
