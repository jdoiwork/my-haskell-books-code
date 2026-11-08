-- ANCHOR: pragmas
{-# LANGUAGE RankNTypes #-}
-- ANCHOR_END: pragmas
-- ANCHOR: all
-- | From the outside world into Vec: the length is known only at run time.
-- 外の世界から Vec へ：長さは、動かしてみるまでわからない
module Gate
  ( toThree
  , withVec
  , vlength
  ) where

-- ANCHOR: imports
import Vec
-- ANCHOR_END: imports

-- ANCHOR: gate
-- 🚪 A gate: check the length once, and get a Vec Three, or nothing.
-- 門番：長さを1回だけ確かめて、Vec Three を受け取るか、何もないか
toThree :: [a] -> Maybe (Vec Three a)
toThree [a, b, c] = Just (a :> b :> c :> VNil)
toThree _         = Nothing

-- 🎁 Any length: hand the Vec to a function that works for every n.
-- どんな長さでも：どの n でも動く関数に、Vec を渡す
withVec :: [a] -> (forall n. Vec n a -> r) -> r
withVec []       k = k VNil
withVec (x : xs) k = withVec xs (\v -> k (x :> v))

vlength :: Vec n a -> Int
vlength = length
-- ANCHOR_END: gate
-- ANCHOR_END: all
