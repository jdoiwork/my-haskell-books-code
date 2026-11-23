-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Doing things at once: sleep, and the IO.all family. Each builtin
--   only makes an action (Gather, Race, or a Sleep request); the
--   runner (Lam.Io) does it.
-- 同時にやる：sleep と IO.all の仲間。どの組み込みも、頼みごと（Gather・
-- Race・Sleep の頼みごと）を作るだけ。やるのは実行役（Lam.Io）
module Lam.Concurrent
  ( concurrentBuiltins
  ) where

import Lam.Builtins
import Lam.Eval
import Lam.Io (ioOf)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
concurrentBuiltins :: [Builtin]
concurrentBuiltins =
  -- ANCHOR: sleep
  -- sleep 500: wait for half a second.
  -- sleep 500：0.5 秒待つ
  [ prim1 "sleep" (TInt ~> tIO tUnit) $ \v -> case v of
      VInt ms | ms >= 0 -> Right (VIO (Ask (Sleep ms)))
      _ -> Left (BuiltinFailed "sleep" "needs an Int, 0 or more")
  -- ANCHOR_END: sleep
  -- ANCHOR: all
  -- IO.all [a, b, c]: do them at once, and wait for all the answers.
  -- IO.all [a, b, c]：まとめて同時にやり、答えを全部待つ
  , prim1 "IO.all" (tList (tIO a) ~> tIO (tList a)) $ \v ->
      VIO . Gather 0 WaitAll <$> actions v
  -- IO.allN 4 xs: the same, but at most 4 at once.
  -- IO.allN 4 xs：同じだが、同時に多くて4つ
  , prim2 "IO.allN" (TInt ~> tList (tIO a) ~> tIO (tList a)) $ \n v ->
      case n of
        VInt k | k >= 1 -> VIO . Gather (fromInteger k) WaitAll <$> actions v
        _ -> Left (BuiltinFailed "IO.allN" "needs an Int, 1 or more")
  -- ANCHOR_END: all
  -- ANCHOR: both
  -- IO.both a b: two of different types, at once, as a pair.
  -- IO.both a b：型のちがう2つを同時に。答えは組で
  , prim2 "IO.both" (tIO a ~> tIO b ~> tIO (tTuple [a, b])) $ \x y -> do
      ax <- ioOf x
      ay <- ioOf y
      Right (VIO (Bind (Gather 0 WaitAll [ax, ay]) pair))
  -- ANCHOR_END: both
  -- ANCHOR: allOk
  -- IO.allOk: at the first Err, cancel the others, and give that Err.
  -- IO.allOk：最初の Err で、ほかを取り消し、その Err を返す
  , prim1 "IO.allOk"
      (tList (tIO (tResult e a)) ~> tIO (tResult e (tList a))) $ \v ->
      VIO . Gather 0 StopAtErr <$> actions v
  -- ANCHOR_END: allOk
  -- ANCHOR: race
  -- IO.race [a, b]: do them at once; the first to end wins.
  -- IO.race [a, b]：同時にやり、最初に終わったものが勝ち
  , prim1 "IO.race" (tList (tIO a) ~> tIO a) $ \v ->
      VIO . Race <$> actions v
  -- ANCHOR_END: race
  -- ANCHOR: spawn
  -- spawn io: start it, and go on (like go in Go). await t: wait.
  -- spawn io：始めて、先へ進む（Go の go のように）。await t：待つ
  , prim1 "spawn" (tIO a ~> tIO (tTask a)) $ \v ->
      VIO . Spawn <$> ioOf v
  , prim1 "await" (tTask a ~> tIO a) $ \v -> case v of
      VTask t -> Right (VIO (Await t))
      _       -> Left (BuiltinFailed "await" "needs a Task")
  -- ANCHOR_END: spawn
  ]
  where
    a = TVar 0
    b = TVar 1
    e = TVar 2
    tResult x y = TCon "Result" [x, y]
    tTask x = TCon "Task" [x]
    actions v = list v >>= traverse ioOf
    pair v = case v of
      VList [x, y] -> Right (Pure (VTuple [x, y]))
      _            -> Left (BuiltinFailed "IO.both" "lost an answer")
-- ANCHOR_END: table
-- ANCHOR_END: all
