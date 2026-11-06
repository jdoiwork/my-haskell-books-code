-- ANCHOR: all
-- | Answers to the training of "Persistence".
-- 「永続化：届いていないメッセージを失わない」のトレーニングの解答
module MqAnswers
  ( wasted
  , worthCompacting
  ) where

import MiniMq.Broker
import MiniMq.Types

-- ANCHOR: wasted
-- 🗑️ Step 2: how many lines would compacting throw away?
-- ステップ2：短く書き直すと、何行が要らなくなる？
wasted :: [Entry] -> Int
wasted entries = length entries - length (compact (replay entries))
-- ANCHOR_END: wasted

-- ANCHOR: worth
-- ⚖️ Step 3: compact when more than `share` of the journal is waste,
--    but not for a tiny journal.
-- ステップ3：日誌の share より多くがむだなら、書き直す。
-- ただし、小さな日誌は、そのままでよい
worthCompacting :: Double -> [Entry] -> Bool
worthCompacting share entries =
  length entries >= 100
    && fromIntegral (wasted entries) > share * fromIntegral (length entries)
-- ANCHOR_END: worth
-- ANCHOR_END: all
