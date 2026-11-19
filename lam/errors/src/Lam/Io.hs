-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | IO for Lam: the builtins that make requests, and the runner that
--   does them. Making a request does nothing; only runAction does it.
-- Lam の IO：頼みごとを作る組み込みの関数と、それをやる実行役。
-- 頼みごとを作っても、何も起きない。やるのは runAction だけ。
module Lam.Io
  ( ioBuiltins
  , stopBuiltins
  , Handler
  , runAction
  , ioOf
  ) where

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins. Their names start with
--    IO., so they belong to the module IO (made in Lam.Prelude).
-- 組み込みの関数の表に足す行。名前が IO. で始まるので、モジュール IO
-- のもの（Lam.Prelude で作る）
ioBuiltins :: [Builtin]
ioBuiltins =
  -- ANCHOR: returnBind
  -- The two that every IO is made of. The instance Monad IO uses
  -- them, under the names GHC's base uses for the same job.
  -- どの IO も、この2つでつなぐ。instance Monad IO が使う。名前は、
  -- GHC の base が同じ仕事に付けている名前
  [ prim1 "IO.returnIO" (a ~> tIO a) $ \v ->
      Right (VIO (Pure v))
  , prim2 "IO.bindIO" (tIO a ~> (a ~> tIO b) ~> tIO b) $ \m f -> do
      action <- ioOf m
      -- The rest: call the Lam function f, and it gives the next IO.
      -- 続き：Lam の関数 f を呼ぶと、次の IO が返ってくる
      Right (VIO (Bind action (\x -> apply f x >>= ioOf)))
  -- ANCHOR_END: returnBind

  -- The screen and the keyboard
  -- 画面とキーボード
  , prim1 "IO.putStr" (tString ~> tIO tUnit) $ \v ->
      ask . PutStr <$> string v
  , prim1 "IO.putStrLn" (tString ~> tIO tUnit) $ \v ->
      ask . PutStr . (<> "\n") <$> string v
  , Builtin "IO.getLine" (closeOver (tIO (tMaybe tString))) (ask GetLine)

  -- Files: a failure is a value (Err), not a crash
  -- ファイル：失敗は値（Err）で返す。止まらない
  , prim1 "IO.readFile" (tString ~> tIO (tResult tString tString)) $ \v ->
      ask . ReadFile <$> string v
  , prim2 "IO.writeFile"
      (tString ~> tString ~> tIO (tResult tString tUnit)) $ \p t ->
      ask <$> (WriteFile <$> string p <*> string t)

  -- ANCHOR: finallyTry
  -- Cleaning up: the cleanup is done even when the body stops.
  -- 後始末：本体が止まっても、後始末はやる
  , prim2 "IO.finally" (tIO a ~> tIO b ~> tIO a) $ \body cleanup ->
      VIO <$> (Finally <$> ioOf body <*> ioOf cleanup)
  -- A stop as a value: Ok with the answer, or Err with the message.
  -- 止まったことを値に：答えの Ok か、メッセージの Err
  , prim1 "IO.try" (tIO a ~> tIO (tResult tString a)) $ \body ->
      VIO . Catch <$> ioOf body
  -- ANCHOR_END: finallyTry
  ]
  where
    a = TVar 0
    b = TVar 1
    ask = VIO . Ask
    tResult e x = TCon "Result" [e, x]
-- ANCHOR_END: table

-- ANCHOR: stop
-- 🛑 error and todo stop the program, with words for people. They
--    can stand for a value of any type a, as they never give one.
--    Not IO: they stop pure code too, as Haskell's error does.
-- error と todo は、人が読む文でプログラムを止める。値を返さないので、
-- どんな型 a の代わりにもなれる。IO ではない（Haskell の error と同じく、
-- 純粋なコードの中でも止める）
stopBuiltins :: [Builtin]
stopBuiltins =
  [ prim1 "error" (tString ~> a) $ \v -> Left . Raised =<< string v
  , prim1 "todo" (tString ~> a) $ \v -> Left . NotWrittenYet =<< string v
  ]
  where
    a = TVar 0
-- ANCHOR_END: stop

-- ANCHOR: ioOf
-- 🔍 The action inside an IO value.
-- IO の値の中の頼みごと
ioOf :: Value -> Either EvalError Action
ioOf v = case v of
  VIO action -> Right action
  _          -> Left (BuiltinFailed "IO.bindIO" "got a value that is not IO")
-- ANCHOR_END: ioOf

-- ANCHOR: runAction
-- 🙋 A handler does one request, in some monad m, and gives the answer
--    as a Lam value.  IO for the real world, State for a fake one.
-- ハンドラーは、頼みごとを1つ、何かのモナド m の中でやり、答えを
-- Lam の値で返す。本物の世界なら IO、偽物の世界なら State
type Handler m = Request -> m Value

-- 🏃 The runner: do the requests one by one, in the order of Bind.
-- 実行役：頼みごとを、Bind の順に1つずつやる
runAction :: Monad m => Handler m -> Action -> m (Either EvalError Value)
runAction handle action = case action of
  Pure v -> pure (Right v)
  Ask request -> Right <$> handle request
  Bind first rest -> do
    answer <- runAction handle first
    case answer >>= rest of
      Left err   -> pure (Left err)
      Right next -> runAction handle next
  -- ANCHOR: runFinally
  -- The cleanup comes after the body, whatever the body did. If the
  -- cleanup stops too, that stop is the one we tell (as Haskell's).
  -- 後始末は、本体が何をしても、本体のあとに。後始末も止まったら、
  -- そちらを伝える（Haskell と同じ）
  Finally body cleanup -> do
    answer <- runAction handle body
    after  <- runAction handle cleanup
    pure (after >> answer)
  -- ANCHOR_END: runFinally
  -- ANCHOR: runCatch
  Catch body -> do
    answer <- runAction handle body
    pure $ Right $ case answer of
      Right v  -> con "Ok" [v]
      Left err -> con "Err" [VString (renderError err)]
  -- ANCHOR_END: runCatch
-- ANCHOR_END: runAction
-- ANCHOR_END: all
