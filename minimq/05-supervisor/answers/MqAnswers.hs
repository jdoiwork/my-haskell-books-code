-- ANCHOR: all
-- | Answers to the training of "Supervisors".
-- 「監督役：落ちても立ち直る」のトレーニングの解答
module MqAnswers
  ( announced
  , patient
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef
import Data.Text (Text)

import MiniMq.Supervisor

-- ANCHOR: announced
-- 📣 Step 2: a child that says its name every time it starts. A child is
--    just a record with an `IO ()`, so we wrap the work in another one.
-- ステップ2：始まるたびに名乗る子。子は IO () を持ったレコードなので、
-- 仕事を、もう1つの IO () で包むだけ
announced :: (Text -> IO ()) -> Child -> Child
announced say child =
  child { work = say ("starting " <> childName child) >> work child }
-- ANCHOR_END: announced

-- ANCHOR: patient
-- 🐢 Step 3: wait a little before every start but the first, so that
--    a child that falls at once does not use up its restarts in a blink.
-- ステップ3：2回目からは、始める前に少し待つ。すぐに倒れる子が、
-- 一瞬で始め直しの回数を使い切らないように
patient :: Int -> Child -> IO Child
patient micros child = do
  startedRef <- newIORef False
  let work' = do
        again <- atomicModifyIORef' startedRef (\started -> (True, started))
        when again (threadDelay micros)
        work child
  pure child { work = work' }
-- ANCHOR_END: patient
-- ANCHOR_END: all
