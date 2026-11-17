-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | IO for Lam: the builtins that make requests, and the runner that
--   does them. Making a request does nothing; only runAction does it.
-- Lam の IO：頼みごとを作る組み込みの関数と、それをやる実行役。
-- 頼みごとを作っても、何も起きない。やるのは runAction だけ。
module Lam.Io
  ( ioBuiltins
  , Handler
  , Stop (..)
  , runAction
  , ioOf
  ) where

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
ioBuiltins :: [Builtin]
ioBuiltins =
  -- The two that every IO is made of
  -- どの IO も、この2つでつなぐ
  [ prim1 "pure" (a ~> tIO a) $ \v ->
      Right (VIO (Pure v))
  , prim2 "bind" (tIO a ~> (a ~> tIO b) ~> tIO b) $ \m f -> do
      action <- ioOf m
      -- The rest: call the Lam function f, and it gives the next IO.
      -- 続き：Lam の関数 f を呼ぶと、次の IO が返ってくる
      Right (VIO (Bind action (\x -> apply f x >>= ioOf)))

  -- The screen and the keyboard
  -- 画面とキーボード
  , prim1 "putStr" (tString ~> tIO tUnit) $ \v ->
      ask . PutStr <$> string v
  , prim1 "putStrLn" (tString ~> tIO tUnit) $ \v ->
      ask . PutStr . (<> "\n") <$> string v
  , Builtin "getLine" (closeOver (tIO (tMaybe tString))) (ask GetLine)

  -- Files: a failure is a value (Err), not a crash
  -- ファイル：失敗は値（Err）で返す。止まらない
  , prim1 "readFile" (tString ~> tIO (tResult tString tString)) $ \v ->
      ask . ReadFile <$> string v
  , prim2 "writeFile"
      (tString ~> tString ~> tIO (tResult tString tUnit)) $ \p t ->
      ask <$> (WriteFile <$> string p <*> string t)
  ]
  <> scriptBuiltins
  where
    a = TVar 0
    b = TVar 1
    ask = VIO . Ask
    tMaybe t = TCon "Maybe" [t]
    tResult e x = TCon "Result" [e, x]
-- ANCHOR_END: table

-- ANCHOR: scriptTable
-- 📋 The rows for scripts: the standard input and error, the
--    arguments, the environment, and the exit code.
-- スクリプトのための行：標準入力と標準エラー、引数、環境変数、終了コード
scriptBuiltins :: [Builtin]
scriptBuiltins =
  -- ANCHOR: argsRows
  -- The arguments, and the environment variables.
  -- 引数と環境変数
  [ Builtin "getArgs" (closeOver (tIO (tList tString))) (ask GetArgs)
  , prim1 "lookupEnv" (tString ~> tIO (tMaybe tString)) $ \v ->
      ask . LookupEnv <$> string v
  , Builtin "getEnvironment" (closeOver (tIO (tMap tString)))
      (ask GetEnvironment)
  -- ANCHOR_END: argsRows
  -- ANCHOR: stdinRow
  -- All of the standard input, at once.
  -- 標準入力を、まとめて全部
  , Builtin "getContents" (closeOver (tIO tString)) (ask GetContents)
  -- ANCHOR_END: stdinRow
  -- ANCHOR: stderrRows
  -- The standard error: for messages, not for the answer.
  -- 標準エラー：答えではなく、知らせのため
  , prim1 "ePutStr" (tString ~> tIO tUnit) $ \v ->
      ask . PutErr <$> string v
  , prim1 "ePutStrLn" (tString ~> tIO tUnit) $ \v ->
      ask . PutErr . (<> "\n") <$> string v
  -- ANCHOR_END: stderrRows
  -- ANCHOR: exitRow
  -- exit n : IO a. It never gives a value, so a can be any type:
  -- if ... then exit 2 else pure 3  is fine.
  -- exit n は値をくれないので、a は何の型でもよい
  , prim1 "exit" (TInt ~> tIO a) $ \v -> case v of
      VInt n -> Right (VIO (Exit (fromInteger n)))
      _      -> Left (BuiltinFailed "exit" "needs an Int")
  -- ANCHOR_END: exitRow
  ]
  where
    a = TVar 0
    ask = VIO . Ask
    tMaybe t = TCon "Maybe" [t]
-- ANCHOR_END: scriptTable

-- ANCHOR: ioOf
-- 🔍 The action inside an IO value.
-- IO の値の中の頼みごと
ioOf :: Value -> Either EvalError Action
ioOf v = case v of
  VIO action -> Right action
  _          -> Left (BuiltinFailed "bind" "got a value that is not IO")
-- ANCHOR_END: ioOf

-- ANCHOR: runAction
-- 🙋 A handler does one request, in some monad m, and gives the answer
--    as a Lam value.  IO for the real world, State for a fake one.
-- ハンドラーは、頼みごとを1つ、何かのモナド m の中でやり、答えを
-- Lam の値で返す。本物の世界なら IO、偽物の世界なら State
type Handler m = Request -> m Value

-- ANCHOR: stop
-- 🛑 Why the runner stopped before the end: an error, or exit.
-- 実行役が、最後まで行かずに止まったわけ：エラーか、exit か
data Stop
  = Failed EvalError
  | Exited Int
  deriving (Eq, Show)
-- ANCHOR_END: stop

-- 🏃 The runner: do the requests one by one, in the order of Bind.
--    exit is done by the runner itself: it stops, and the rest of the
--    Binds is never called. No handler sees it.
-- 実行役：頼みごとを、Bind の順に1つずつやる。exit は実行役が自分で
-- やる：止まって、残りの Bind の続きは呼ばない。ハンドラーは見ない
runAction :: Monad m => Handler m -> Action -> m (Either Stop Value)
runAction handle action = case action of
  Pure v -> pure (Right v)
  Ask request -> Right <$> handle request
  Exit n -> pure (Left (Exited n))
  Bind first rest -> do
    answer <- runAction handle first
    case answer of
      Left stop -> pure (Left stop)
      Right v   -> case rest v of
        Left err   -> pure (Left (Failed err))
        Right next -> runAction handle next
-- ANCHOR_END: runAction
-- ANCHOR_END: all

