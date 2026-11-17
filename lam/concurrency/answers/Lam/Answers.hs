-- ANCHOR: all
-- | The answers of the training of "Running at once: wait for them all".
-- 「並行に動かす：まとめて待つ」のトレーニングの解答。
module Lam.Answers
  ( gatherWorkers
  ) where

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Map as Map

-- ANCHOR: workers
-- 👷 Step 3: at most n at once, another way: n workers, and a list of
--    jobs to share. Each worker takes the next job, does it, and goes
--    back for more, until the list is empty. The answers are put back
--    in the order of the list by their numbers.
-- ステップ3：同時に多くて n 個を、別のやり方で。n 人の働き手と、分け合う
-- 仕事のリスト。働き手はそれぞれ次の仕事を取ってやり、リストが空になる
-- まで戻ってくる。答えは番号で、リストの順に並べ直す
gatherWorkers :: Int -> [IO r] -> IO [r]
gatherWorkers n jobs = do
  todo <- newMVar (zip [0 :: Int ..] jobs)
  done <- newMVar Map.empty
  let worker = do
        -- Take one job: only one worker at a time holds the list.
        -- 仕事を1つ取る：リストを握れるのは、一度に1人だけ
        next <- modifyMVar todo $ \js -> pure $ case js of
          []       -> ([], Nothing)
          j : rest -> (rest, Just j)
        case next of
          Nothing       -> pure ()
          Just (i, job) -> do
            r <- job
            modifyMVar_ done (pure . Map.insert i r)
            worker
  -- If a worker throws, the others are cancelled (async does it).
  -- 働き手が例外を投げたら、ほかは取り消される（async がやる）
  replicateConcurrently_ (max 1 n) worker
  Map.elems <$> readMVar done
-- ANCHOR_END: workers
-- ANCHOR_END: all
