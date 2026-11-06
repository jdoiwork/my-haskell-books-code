-- ANCHOR: all
-- | Answers to the training of the chapter.
-- 章のトレーニングの解答
module MutableAnswers
  ( firstSeen
  , quizA
  , quizB
  , quizC
  ) where

import Control.Monad               (forM_, when)
import Control.Monad.ST            (runST)
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU

import Tally (monsterKinds)

-- ANCHOR: firstSeen
-- 👀 For each kind of monster, the turn we first met it (-1 if never).
-- モンスターの種類ごとに、初めて出会ったターン（出会わなければ -1）
firstSeen :: [Int] -> VU.Vector Int
firstSeen kinds = runST $ do
  seen <- MVU.replicate monsterKinds (-1)
  forM_ (zip [0 ..] kinds) $ \(turn, k) -> do
    before <- MVU.read seen k
    when (before == -1) $
      MVU.write seen k turn
  VU.freeze seen
-- ANCHOR_END: firstSeen

-- ANCHOR: quiz
-- A
quizA :: VU.Vector Int
quizA = runST $ do
  v <- MVU.replicate 3 0
  MVU.write v 0 1
  VU.unsafeFreeze v

-- B
quizB :: IO (VU.Vector Int, VU.Vector Int)
quizB = do
  v <- MVU.replicate 3 0
  first <- VU.unsafeFreeze v
  MVU.write v 0 1
  second <- VU.freeze v
  pure (first, second)

-- C
quizC :: VU.Vector Int -> VU.Vector Int
quizC xs = runST $ do
  v <- VU.thaw xs
  MVU.write v 0 1
  VU.unsafeFreeze v
-- ANCHOR_END: quiz
-- ANCHOR_END: all
