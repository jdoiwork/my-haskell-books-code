-- ANCHOR: all
-- | Answers to the training of "Type-level programming".
-- 「型レベルプログラミング入門」のトレーニングの解答
module TypeAnswers (vzipWith, swapFirstTwo) where

import Vec

-- ANCHOR: zipWith
-- 🤝 Step 2: combine two Vecs of the same length with a function.
-- ステップ2：同じ長さの Vec を、関数で組み合わせる
vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith _ VNil      VNil      = VNil
vzipWith f (x :> xs) (y :> ys) = f x y :> vzipWith f xs ys
-- ANCHOR_END: zipWith

-- ANCHOR: swap
-- 🔀 Step 3: swap the first two; the type says "at least two".
-- ステップ3：先頭の2つを入れ替える。型が「2つ以上」と言っている
swapFirstTwo :: Vec (S (S n)) a -> Vec (S (S n)) a
swapFirstTwo (x :> y :> rest) = y :> x :> rest
-- ANCHOR_END: swap
-- ANCHOR_END: all
