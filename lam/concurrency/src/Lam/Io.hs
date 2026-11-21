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
  , Concurrent (..)
  , runAction
  , ioOf
  ) where

import Control.Concurrent.Async (async, mapConcurrently, race, wait)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (Exception, bracket_, throwIO, try)
import Control.Monad (when)

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


-- 🏃 The runner: do the requests one by one, in the order of Bind.
--    exit is done by the runner itself: it stops, and the rest of the
--    Binds is never called. No handler sees it. Gather and Race are
--    done by the monad: Concurrent says how it does things at once.
-- 実行役：頼みごとを、Bind の順に1つずつやる。exit は実行役が自分で
-- やる：止まって、残りの Bind の続きは呼ばない。ハンドラーは見ない。
-- Gather と Race はモナドがやる。同時にやる方法は Concurrent が決める
runAction :: Concurrent m => Handler m -> Action -> m (Either Stop Value)
runAction handle action = case action of
  Pure v -> pure (Right v)
  Ask request -> Right <$> handle request
  Exit n -> pure (Left (Exited n))
  Bind first rest -> do
    answer <- runAction handle first
    case answer of
      Left stop -> pure (Left stop)
      Right v   -> either (pure . Left . Failed) (runAction handle) (rest v)
  -- ANCHOR: runGather
  Gather n early actions -> do
    answer <- gather n (stopsOthers early) (map (runAction handle) actions)
    pure $ case answer of
      -- One stopped the others: a stop, or the first Err of IO.allOk.
      -- 1つがほかを取り消した：止まったか、IO.allOk の最初の Err
      Left first -> first
      Right answers -> do
        values <- sequence answers
        Right $ case early of
          WaitAll   -> VList values
          StopAtErr -> con "Ok" [VList [ v | VCon "Ok" _ [v] <- values ]]
  Race [] -> pure (Left (Failed (BuiltinFailed "IO.race" "got no actions")))
  Race actions -> firstOf (map (runAction handle) actions)
  Spawn job -> Right . VTask <$> spawnJob (runAction handle job)
  Await task -> awaitJob task
  -- ANCHOR_END: runGather

-- ANCHOR: stopsOthers
-- 🛎️ Does this answer stop the others? A stop always does (structured
--    concurrency: no job is left running alone). For IO.allOk, an Err
--    does, too.
-- この答えで、ほかを取り消すか。止まったら、いつも取り消す（構造化された
-- 並行性：置き去りの仕事を残さない）。IO.allOk なら、Err でも
stopsOthers :: Early -> Either Stop Value -> Bool
stopsOthers early answer = case (early, answer) of
  (_, Left _)                          -> True
  (StopAtErr, Right (VCon "Err" _ _)) -> True
  _                                    -> False
-- ANCHOR_END: stopsOthers

-- ANCHOR: concurrent
-- 🤝 How a monad does several jobs at once. The real world (IO) uses
--    threads; a fake world runs them one by one, on a fake clock.
-- モナドが、いくつかの仕事を同時にやる方法。本物の世界（IO）はスレッドで。
-- 偽物の世界は、偽の時計の上で1つずつ
class Monad m => Concurrent m where
  -- | Do the jobs, at most n at once (0: no limit), and give every
  --   answer, in the order of the list. If an answer is final, stop
  --   the others, and give that answer in Left.
  -- 仕事を多くて n 個ずつ同時にやり（0 は上限なし）、答えをリストの順に
  -- 全部返す。最後の答え（final）が出たら、ほかを取り消し、その答えを
  -- Left で返す
  gather :: Int -> (r -> Bool) -> [m r] -> m (Either r [r])
  -- | Do the jobs at once. The first to end wins; stop the others.
  -- 仕事を同時にやる。最初に終わったものが勝ち。ほかは取り消す
  firstOf :: [m r] -> m r
  -- | Start a job, and go on; wait for it later.
  -- 仕事を始めて、先へ進む。待つのは、あとで
  spawnJob :: m (Either Stop Value) -> m Task
  awaitJob :: Task -> m (Either Stop Value)
-- ANCHOR_END: concurrent

-- ANCHOR: concurrentIO
-- 🧵 The real world: one thread for each job, with async.
--    mapConcurrently waits for all; when one throws, it cancels the
--    others and throws again. A final answer is kept in a box and
--    thrown as StopTheOthers, to use that.
-- 本物の世界：仕事ごとに1つのスレッドを、async で。mapConcurrently は
-- 全部を待つ。1つが例外を投げると、ほかを取り消して投げ直す。最後の
-- 答えは箱にしまって StopTheOthers を投げ、その仕組みを使う
instance Concurrent IO where
  gather n final jobs = do
    -- At most n at once: a job takes one of n tickets (a semaphore).
    -- 多くて n 個：仕事は n 枚の札（セマフォ）の1枚を取ってから始める
    limit <- if n > 0 then Just <$> newQSem n else pure Nothing
    let ticket job = case limit of
          Just q  -> bracket_ (waitQSem q) (signalQSem q) job
          Nothing -> job
    box <- newEmptyMVar
    let one job = do
          r <- ticket job
          when (final r) $ do
            _ <- tryPutMVar box r      -- the first one wins / 先着1つ
            throwIO StopTheOthers
          pure r
    result <- try (mapConcurrently one jobs)
    case result of
      Right answers      -> pure (Right answers)
      Left StopTheOthers -> Left <$> readMVar box
  -- race a b: the first to end wins, and the other is cancelled.
  -- race a b：先に終わったほうが勝ち、もう片方は取り消される
  firstOf jobs = foldr1 (\a b -> either id id <$> race a b) jobs
  -- A thread of its own. When main ends, the program ends, and a job
  -- nobody waited for ends with it (the same as Go).
  -- 自分のスレッドで。main が終わればプログラムが終わり、だれも待たな
  -- かった仕事も一緒に終わる（Go と同じ）
  spawnJob job = RealTask <$> async job
  awaitJob task = case task of
    RealTask a      -> wait a
    FakeTask _ done -> pure done

-- 🛑 Thrown to stop the other jobs of gather.
-- gather のほかの仕事を取り消すために投げる
data StopTheOthers = StopTheOthers
  deriving Show

instance Exception StopTheOthers
-- ANCHOR_END: concurrentIO
-- ANCHOR_END: runAction
-- ANCHOR_END: all

