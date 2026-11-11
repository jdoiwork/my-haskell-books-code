-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of "A client strict about types".
-- 「型に厳しいクライアント」のトレーニングの解答
module TypedAnswers
  ( addGold
  ) where

import Data.Maybe (fromMaybe)

import MiniKv.Client (Connection)
import MiniKv.Typed
-- ANCHOR_END: head

-- ANCHOR: addGold
-- 💰 Step 2: add to a number that may not be there yet.
--    (Two trips, so another client can sneak in between them.)
-- ステップ2：まだないかもしれない数に、足す。
-- （往復が2回なので、そのあいだに、ほかのクライアントが割りこめる）
addGold :: Connection -> Key Int -> Int -> IO (Either ClientError Int)
addGold conn key amount = do
  answer <- run conn (Get key)
  case answer of
    Left err  -> pure (Left err)
    Right old -> do
      let new = fromMaybe 0 old + amount
      saved <- run conn (Set key new)
      pure (fmap (const new) saved)
-- ANCHOR_END: addGold
-- ANCHOR_END: all
