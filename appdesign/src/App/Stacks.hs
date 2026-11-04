-- ANCHOR: all
-- | The order of the layers matters: does the state survive an error?
-- 重ねる順番で、意味が変わる：エラーのあと、状態は残る？
module App.Stacks
  ( countThenFail
  , stateOutside
  , exceptOutside
  ) where

import Control.Monad.Except
import Control.Monad.State

-- ANCHOR: stacks
-- 💥 Count one, then fail. Written once, for any monad that can do both.
-- 1つ数えてから、失敗する。両方できるモナドなら何でもよい形で、1回だけ書く
countThenFail :: (MonadState Int m, MonadError String m) => m ()
countThenFail = do
  modify (+ 1)
  throwError "boom"

-- 🧅 State outside, Either inside: the error throws the count away.
-- State が外、Either が内：エラーで、数えた分も消える
stateOutside :: Either String ((), Int)
stateOutside = runStateT countThenFail 0

-- 🧅 Except outside, State inside: the count survives the error.
-- Except が外、State が内：エラーのあとも、数えた分は残る
exceptOutside :: (Either String (), Int)
exceptOutside = runState (runExceptT countThenFail) 0
-- ANCHOR_END: stacks
-- ANCHOR_END: all
