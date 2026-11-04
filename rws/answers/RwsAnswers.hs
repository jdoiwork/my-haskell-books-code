-- ANCHOR: all
-- | Answers to the training of "Reader, Writer and State".
-- 「Reader・Writer・State」のトレーニングの解答
module RwsAnswers
  ( fresh
  , labelAll
  , asPlayer
  ) where

import Control.Monad.Reader
import Control.Monad.State

import Config

-- ANCHOR: label
-- 🏷️ Step 2: hand out numbers 1, 2, 3, ... one at a time.
-- ステップ2：番号 1, 2, 3, ... を1つずつ配る
fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  pure n

labelAll :: [String] -> [String]
labelAll names = evalState (traverse label names) 1
  where
    label name = do
      n <- fresh
      pure (name ++ "-" ++ show n)
-- ANCHOR_END: label

-- ANCHOR: asPlayer
-- 🎭 Step 3: run a part as somebody else.
-- ステップ3：一部分だけ、別の人として動かす
asPlayer :: String -> Reader Config a -> Reader Config a
asPlayer who = local (\cfg -> cfg { playerName = who })
-- ANCHOR_END: asPlayer
-- ANCHOR_END: all
