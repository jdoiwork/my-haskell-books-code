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
  , Stop (..)
  , Concurrent (..)
  , runAction
  , ioOf
  ) where

import Control.Concurrent.Async (async, mapConcurrently, race, wait)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (Exception, bracket_, mask, onException, throwIO,
                          try)
import Control.Monad (when)

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
  <> scriptBuiltins
  where
    a = TVar 0
    b = TVar 1
    ask = VIO . Ask
    tResult e x = TCon "Result" [e, x]
-- ANCHOR_END: table

-- ANCHOR: scriptTable
-- 📋 The rows for scripts: the arguments, the environment, the
--    standard input and error, and the exit code. They belong to the
--    module IO too, and every file has them without IO. in front.
-- スクリプトのための行：引数、環境変数、標準入力と標準エラー、終了コード。
-- これもモジュール IO のもので、どのファイルでも IO. なしで使える
scriptBuiltins :: [Builtin]
scriptBuiltins =
  -- ANCHOR: argsRows
  -- The arguments, and the environment variables.
  -- 引数と環境変数
  [ Builtin "IO.getArgs" (closeOver (tIO (tList tString))) (ask GetArgs)
  , prim1 "IO.lookupEnv" (tString ~> tIO (tMaybe tString)) $ \v ->
      ask . LookupEnv <$> string v
  , Builtin "IO.getEnvironment" (closeOver (tIO (tMap tString tString)))
      (ask GetEnvironment)
  -- ANCHOR_END: argsRows
  -- ANCHOR: stdinRow
  -- All of the standard input, at once.
  -- 標準入力を、まとめて全部
  , Builtin "IO.getContents" (closeOver (tIO tString)) (ask GetContents)
  -- ANCHOR_END: stdinRow
  -- ANCHOR: stderrRows
  -- The standard error: for messages, not for the answer.
  -- 標準エラー：答えではなく、知らせのため
  , prim1 "IO.ePutStr" (tString ~> tIO tUnit) $ \v ->
      ask . PutErr <$> string v
  , prim1 "IO.ePutStrLn" (tString ~> tIO tUnit) $ \v ->
      ask . PutErr . (<> "\n") <$> string v
  -- ANCHOR_END: stderrRows
  -- ANCHOR: exitRow
  -- exit n : IO a. It never gives a value, so a can be any type:
  -- if ... then exit 2 else pure 3  is fine.
  -- exit n は値をくれないので、a は何の型でもよい
  , prim1 "IO.exit" (TInt ~> tIO a) $ \v -> case v of
      VInt n -> Right (VIO (Exit (fromInteger n)))
      _      -> Left (BuiltinFailed "exit" "needs an Int")
  -- ANCHOR_END: exitRow
  ]
  where
    a = TVar 0
    ask = VIO . Ask
-- ANCHOR_END: scriptTable

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
  -- ANCHOR: runExit
  Exit n -> pure (Left (Exited n))
  -- ANCHOR_END: runExit
  Bind first rest -> do
    answer <- runAction handle first
    case answer of
      Left stop -> pure (Left stop)
      Right v   -> either (pure . Left . Failed) (runAction handle) (rest v)
  -- ANCHOR: runFinally
  -- The cleanup comes after the body, whatever the body did: an
  -- error, or exit, or even a cancel from outside (withCleanup). If
  -- the cleanup stops too, that stop is the one we tell (as Haskell's).
  -- 後始末は、本体が何をしても（エラーでも exit でも、外からの取り消し
  -- でも。withCleanup）、本体のあとに。後始末も止まったら、そちらを
  -- 伝える（Haskell と同じ）
  Finally body cleanup -> do
    (answer, after) <-
      withCleanup (runAction handle body) (runAction handle cleanup)
    pure (after >> answer)
  -- ANCHOR_END: runFinally
  -- ANCHOR: runCatch
  -- An error becomes an Err (its words, without the place), but exit
  -- goes on out: try does not catch it (as Rust's catch_unwind does
  -- not catch process::exit).
  -- エラーは Err に（場所を取った文で）。exit は、そのまま外へ出る。
  -- try は exit をつかまえない
  Catch body -> do
    answer <- runAction handle body
    pure $ case answer of
      Right v             -> Right (con "Ok" [v])
      Left (Failed err)   ->
        Right (con "Err" [VString (renderError (snd (placeOf err)))])
      Left stop@(Exited _) -> Left stop
  -- ANCHOR_END: runCatch
  -- ANCHOR: runGather
  Gather n early actions -> do
    answer <- gather n (stopsOthers early) (map (runAction handle) actions)
    pure $ case answer of
      -- One stopped the others: a stop, or the first Err of IO.allOk.
      -- 1つがほかを取り消した：止まったか、IO.allOk の最初の Err
      Left first -> first
      Right answers -> do
        values <- sequence answers
        Right (collected early values)
  Race [] -> pure (Left (Failed (BuiltinFailed "IO.race" "got no actions")))
  Race actions -> firstOf (map (runAction handle) actions)
  Spawn job -> Right . VTask <$> spawnJob (runAction handle job)
  Await task -> awaitJob task
  -- ANCHOR_END: runGather

-- 📦 All the answers: a list, or for IO.allOk, Ok with the list.
-- 答えの全部：リスト。IO.allOk なら、リストを Ok で包む
collected :: Early -> [Value] -> Value
collected early values = case early of
  WaitAll   -> VList values
  StopAtErr -> con "Ok" [VList [ v | VCon "Ok" _ [v] <- values ]]

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
  -- | Do the body, then the cleanup: even when the body is cancelled.
  -- 本体をやり、それから後始末を。本体が取り消されても
  withCleanup :: m a -> m b -> m (a, b)
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
  -- ANCHOR: withCleanupIO
  -- A cancel is an exception thrown into the thread (async's cancel).
  -- mask holds it back while we are not in the body; onException does
  -- the cleanup, and lets the cancel go on out.
  -- 取り消しは、スレッドに投げこまれる例外（async の cancel）。mask で、
  -- 本体の外では待ってもらう。onException が後始末をして、取り消しを
  -- そのまま外へ通す
  withCleanup body cleanup = mask $ \restore -> do
    answer <- restore body `onException` cleanup
    after  <- cleanup
    pure (answer, after)
  -- ANCHOR_END: withCleanupIO

-- 🛑 Thrown to stop the other jobs of gather.
-- gather のほかの仕事を取り消すために投げる
data StopTheOthers = StopTheOthers
  deriving Show

instance Exception StopTheOthers
-- ANCHOR_END: concurrentIO
-- ANCHOR_END: runAction
-- ANCHOR_END: all
