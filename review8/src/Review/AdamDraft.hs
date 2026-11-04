-- ANCHOR: all
-- | A first draft of Adam that forgot something. The tests find it.
-- 何かを忘れた、Adam の下書き。テストが見つける
module Review.AdamDraft (adamDraft) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import Review.Optim (AdamConfig (..), Optimizer (..))

-- ANCHOR: draft
-- 🦉 Adam, the first draft: m and v are used as they are.
-- Adam の下書き：m と v を、そのまま使う
adamDraft :: AdamConfig -> Optimizer
adamDraft cfg = go Map.empty Map.empty
  where
    go :: Map String NDArray -> Map String NDArray -> Optimizer
    go ms vs = Optimizer $ \ps grads ->
      let b1  = beta1 cfg
          b2  = beta2 cfg
          ms' = Map.mapWithKey (average b1 ms) grads
          vs' = Map.mapWithKey (average b2 vs) (Map.map (\g -> g * g) grads)
          move name p = case (Map.lookup name ms', Map.lookup name vs') of
            (Just m, Just v) ->
              p - Np.scalar (learningRate cfg) * m
                    / (sqrt v + Np.scalar (epsilon cfg))
            _ -> p
      in  (Map.mapWithKey move ps, go ms' vs')
-- ANCHOR_END: draft

average :: Double -> Map String NDArray -> String -> NDArray -> NDArray
average beta old name new = case Map.lookup name old of
  Nothing -> Np.scalar (1 - beta) * new
  Just o  -> Np.scalar beta * o + Np.scalar (1 - beta) * new
-- ANCHOR_END: all
