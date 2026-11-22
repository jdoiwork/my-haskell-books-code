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
  , ioOf
  , collected
  , stopsOthers
  , caught
  , downValue
  , jobName
  , firstFit
  , Me (..)
  , newRealChan
  , sendReal
  , recvSTM
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, mapConcurrently, race, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM
  ( STM, TVar, atomically, modifyTVar', newTVarIO, orElse, readTVar
  , readTVarIO, registerDelay, retry, stateTVar, writeTVar )
import Control.Exception
  ( BlockedIndefinitelyOnMVar (..), BlockedIndefinitelyOnSTM (..)
  , Exception, bracket_, catches, mask, onException, throwIO, try )
import qualified Control.Exception as E
import Control.Monad (unless, when)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

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

-- ANCHOR: concurrent
-- 🤝 How a monad runs an action, with all the jobs it starts. The
--    real world (IO) uses threads and STM; a fake world takes turns
--    on its fake clock (Lam.Scheduler): a job that waits for a
--    message must stop there, and let another job go on. So the class
--    has one method now: the whole runner.
-- モナドが、頼みごとを（それが始める仕事も全部）やる方法。本物の世界
-- （IO）はスレッドと STM で。偽物の世界は、偽の時計の上で順番に
-- （Lam.Scheduler）：メッセージを待つ仕事は、そこで止まって、ほかの仕事に
-- 番をゆずらなければならない。だからクラスのメソッドは、実行役まるごとの
-- 1つになった
class Monad m => Concurrent m where
  runAction :: Handler m -> Action -> m (Either Stop Value)
-- ANCHOR_END: concurrent

-- 🧵 The real world: main is job 0.
-- 本物の世界：main は 0 番の仕事
instance Concurrent IO where
  runAction = runReal (Me 0 "main")

-- ANCHOR: me
-- 🙋 Which job is running: its number, and its name for people
--    (main, job #3, actor <pid #2>).
-- どの仕事が動いているか：番号と、人が読む名前
data Me = Me Int Text
-- ANCHOR_END: me

-- ANCHOR: runReal
-- 🏃 The runner of the real world: do the requests in the order of
--    Bind. exit is done by the runner itself: it stops, and the rest
--    of the Binds is never called. The other signs (Gather, Spawn,
--    Send, ...) are done with threads (async) and STM.
-- 本物の世界の実行役：頼みごとを、Bind の順にやる。exit は実行役が自分で
-- やる：止まって、残りの Bind の続きは呼ばない。ほかの合図（Gather・
-- Spawn・Send など）は、スレッド（async）と STM で
runReal :: Me -> Handler IO -> Action -> IO (Either Stop Value)
runReal me handle action = case action of
  Pure v -> pure (Right v)
  Ask request -> Right <$> handle request
  -- ANCHOR: runExit
  Exit n -> pure (Left (Exited n))
  -- ANCHOR_END: runExit
  Bind first rest -> do
    answer <- runReal me handle first
    case answer of
      Left stop -> pure (Left stop)
      Right v   -> either (pure . Left . Failed) (runReal me handle) (rest v)
  -- ANCHOR: runFinally
  -- The cleanup comes after the body, whatever the body did: an
  -- error, or exit, or even a cancel from outside (withCleanup). If
  -- the cleanup stops too, that stop is the one we tell (as Haskell's).
  -- 後始末は、本体が何をしても（エラーでも exit でも、外からの取り消し
  -- でも。withCleanup）、本体のあとに。後始末も止まったら、そちらを
  -- 伝える（Haskell と同じ）
  Finally body cleanup -> do
    (answer, after) <-
      withCleanup (runReal me handle body) (runReal me handle cleanup)
    pure (after >> answer)
  -- ANCHOR_END: runFinally
  Catch body -> caught <$> runReal me handle body
  -- ANCHOR: runGather
  Gather n early actions -> do
    jobs <- mapM job actions
    answer <- waiting me "waits for its jobs (IO.all)" $
      Right <$> gatherIO n (stopsOthers early) jobs
    pure $ case answer of
      Left stop -> Left stop
      -- One stopped the others: a stop, or the first Err of IO.allOk.
      -- 1つがほかを取り消した：止まったか、IO.allOk の最初の Err
      Right (Left first) -> first
      Right (Right answers) -> collected early <$> sequence answers
  Race [] -> pure (Left (Failed (BuiltinFailed "IO.race" "got no actions")))
  Race actions -> do
    jobs <- mapM job actions
    answer <- waiting me "waits for the race (IO.race)" $
      Right <$> foldr1 (\a b -> either id id <$> race a b) jobs
    pure (either Left id answer)
  -- A thread of its own. When main ends, the program ends, and a job
  -- nobody waited for ends with it (the same as Go).
  -- 自分のスレッドで。main が終わればプログラムが終わり、だれも待たな
  -- かった仕事も一緒に終わる（Go と同じ）
  Spawn work -> do
    k <- newJobId
    t <- async (runReal (Me k (jobName k)) handle work)
    pure (Right (VTask (RealTask k t)))
  Await task -> case task of
    RealTask k t -> do
      answer <- waiting me ("waits for " <> jobName k <> " (await)")
                  (Right <$> waitCatch t)
      pure $ case answer of
        Left stop          -> Left stop
        Right (Right r)    -> r
        Right (Left oops)  ->
          Left (Failed (BuiltinFailed "await" (T.pack (show oops))))
    FakeTask _ -> pure (Left (Failed (BuiltinFailed "await" "a fake task")))
  -- ANCHOR_END: runGather
  -- ANCHOR: runChans
  NewChan capacity -> do
    c <- newRealChan capacity
    pure (Right (if capacity < 0 then VPid (Pid c Nothing) else VChan c))
  Send c v -> sendReal me c v
  Receive c -> receiveReal me c
  Close c -> closeReal c
  -- ANCHOR_END: runChans
  -- ANCHOR: runMail
  Select c fits late -> do
    picked <- selectReal me c fits late
    case picked of
      Left stop  -> pure (Left stop)
      Right next -> runReal me handle next
  Start box work -> do
    k <- newJobId
    let name = "actor " <> renderChan box
    t <- async (runActor (Me k name) handle work)
    pure (Right (VPid (Pid box (Just (RealTask k t)))))
  Monitor watched f box -> monitorReal watched f box
  -- ANCHOR_END: runMail
  where
    job work = do
      k <- newJobId
      pure (runReal (Me k (jobName k)) handle work)
-- ANCHOR_END: runReal

-- ANCHOR: runCatch
-- 🪤 IO.try: an error becomes an Err (its words, without the place),
--    but exit goes on out: try does not catch it (as Rust's
--    catch_unwind does not catch process::exit).
-- IO.try：エラーは Err に（場所を取った文で）。exit は、そのまま外へ出る。
-- try は exit をつかまえない
caught :: Either Stop Value -> Either Stop Value
caught answer = case answer of
  Right v              -> Right (con "Ok" [v])
  Left (Failed err)    ->
    Right (con "Err" [VString (renderError (snd (placeOf err)))])
  Left stop@(Exited _) -> Left stop
-- ANCHOR_END: runCatch

-- ANCHOR: withCleanupIO
-- 🧹 Do the body, then the cleanup: even when the body is cancelled.
--    A cancel is an exception thrown into the thread (async's cancel).
--    mask holds it back while we are not in the body; onException
--    does the cleanup, and lets the cancel go on out.
-- 本体をやり、それから後始末を。本体が取り消されても。取り消しは、
-- スレッドに投げこまれる例外（async の cancel）。mask で、本体の外では
-- 待ってもらう。onException が後始末をして、取り消しをそのまま外へ通す
withCleanup :: IO a -> IO b -> IO (a, b)
withCleanup body cleanup = mask $ \restore -> do
  answer <- restore body `onException` cleanup
  after  <- cleanup
  pure (answer, after)
-- ANCHOR_END: withCleanupIO

-- 🏷️ The name of a job, for people.
-- 人が読む、仕事の名前
jobName :: Int -> Text
jobName k = "job #" <> T.pack (show k)

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

-- ANCHOR: gatherIO
-- 🧵 Do the jobs, at most n at once (0: no limit), and give every
--    answer, in the order of the list. If an answer is final, stop
--    the others, and give that answer in Left. mapConcurrently waits
--    for all; when one throws, it cancels the others and throws
--    again. A final answer is kept in a box and thrown as
--    StopTheOthers, to use that.
-- 仕事を多くて n 個ずつ同時にやり（0 は上限なし）、答えをリストの順に
-- 全部返す。最後の答え（final）が出たら、ほかを取り消し、その答えを
-- Left で返す。mapConcurrently は全部を待つ。1つが例外を投げると、ほかを
-- 取り消して投げ直す。最後の答えは箱にしまって StopTheOthers を投げ、
-- その仕組みを使う
gatherIO :: Int -> (r -> Bool) -> [IO r] -> IO (Either r [r])
gatherIO n final jobs = do
  -- At most n at once: a job takes one of n tickets (a semaphore).
  -- 多くて n 個：仕事は n 枚の札（セマフォ）の1枚を取ってから始める
  limit <- if n > 0 then Just <$> newQSem n else pure Nothing
  let ticket work = case limit of
        Just q  -> bracket_ (waitQSem q) (signalQSem q) work
        Nothing -> work
  box <- newEmptyMVar
  let one work = do
        r <- ticket work
        when (final r) $ do
          _ <- tryPutMVar box r      -- the first one wins / 先着1つ
          throwIO StopTheOthers
        pure r
  result <- try (mapConcurrently one jobs)
  case result of
    Right answers      -> pure (Right answers)
    Left StopTheOthers -> Left <$> readMVar box

-- 🛑 Thrown to stop the other jobs of gather.
-- gather のほかの仕事を取り消すために投げる
data StopTheOthers = StopTheOthers
  deriving Show

instance Exception StopTheOthers
-- ANCHOR_END: gatherIO

-- ANCHOR: realChan
-- 📬 A new real channel: four STM variables, and a number.
-- 新しい本物のチャネル：STM の変数4つと、番号
newRealChan :: Int -> IO Chan
newRealChan capacity = do
  k <- atomically (stateTVar chanCounter (\n -> (n + 1, n + 1)))
  box <- RealBox <$> newTVarIO Seq.empty <*> newTVarIO False
                 <*> newTVarIO 0 <*> newTVarIO 0
  pure (Chan k capacity (Just box))

-- 📦 The box of a real channel.
-- 本物のチャネルの箱
boxOf :: Chan -> RealBox
boxOf c = case chanBox c of
  Just box -> box
  Nothing  -> error "a fake channel in the real world"
-- ANCHOR_END: realChan

-- ANCHOR: sendReal
-- 📤 Send: wait while the channel is full (Chan.new n). With
--    Chan.new 0, wait again, until a receiver took this very value
--    (its ticket). retry means: wait until one of the variables read
--    here changes, then try again from the top.
-- 送る：チャネルがいっぱいなら待つ（Chan.new n）。Chan.new 0 なら、
-- 受け手がこの値（の札）を取るまで、もう一度待つ。retry は「ここで
-- 読んだ変数のどれかが変わるまで待ち、また頭からやり直す」
sendReal :: Me -> Chan -> Value -> IO (Either Stop Value)
sendReal me c v = do
  let box = boxOf c
  put <- waiting me ("waits to send to " <> renderChan c) $ atomically $ do
    closed <- readTVar (boxClosed box)
    if closed
      then pure (Left (Failed (sendClosed c)))
      else do
        items <- readTVar (boxItems box)
        when (chanCapacity c > 0 && Seq.length items >= chanCapacity c)
          retry
        writeTVar (boxItems box) (items Seq.|> v)
        Right <$> stateTVar (boxSent box) (\n -> (n + 1, n + 1))
  case put of
    Left stop -> pure (Left stop)
    Right ticket
      | chanCapacity c == 0 ->
          fmap (const unit) <$>
            waiting me ("waits to hand over to " <> renderChan c)
              (atomically $ do
                taken <- readTVar (boxTaken box)
                when (taken < ticket) retry
                pure (Right ()))
      | otherwise -> pure (Right unit)

-- 🚫 Sending to a closed channel is a mistake (Go panics).
-- 閉じたチャネルに送るのは、まちがい（Go は panic する）
sendClosed :: Chan -> EvalError
sendClosed c = BuiltinFailed "Chan.send" (renderChan c <> " is closed")
-- ANCHOR_END: sendReal

-- ANCHOR: recvReal
-- 📥 Receive: the oldest value. If there is none: Nothing if the
--    channel is closed, or else wait.
-- 受け取る：いちばん古い値。なければ、閉じていれば Nothing、でなければ待つ
receiveReal :: Me -> Chan -> IO (Either Stop Value)
receiveReal me c =
  waiting me ("waits to receive from " <> renderChan c) $
    atomically (Right . maybeValue <$> recvSTM c)

-- 📥 The same, as one STM transaction: Just the value, or Nothing if
--    the channel is closed and empty. retry while it is open and empty.
-- 同じことを、STM の1つのトランザクションで：値を Just で。閉じて空なら
-- Nothing。開いていて空なら retry
recvSTM :: Chan -> STM (Maybe Value)
recvSTM c = do
  let box = boxOf c
  items <- readTVar (boxItems box)
  case Seq.viewl items of
    v Seq.:< rest -> do
      writeTVar (boxItems box) rest
      modifyTVar' (boxTaken box) (+ 1)
      pure (Just v)
    Seq.EmptyL -> do
      closed <- readTVar (boxClosed box)
      if closed then pure Nothing else retry

-- 🔒 Close: no more values. Closing twice is a mistake (as in Go).
-- 閉じる：もう送らない。2度閉じるのはまちがい（Go と同じ）
closeReal :: Chan -> IO (Either Stop Value)
closeReal c = atomically $ do
  let box = boxOf c
  closed <- readTVar (boxClosed box)
  if closed
    then pure (Left (Failed (BuiltinFailed "Chan.close"
           (renderChan c <> " is closed already"))))
    else Right unit <$ writeTVar (boxClosed box) True
-- ANCHOR_END: recvReal

-- ANCHOR: selectReal
-- 📭 receive: the first message that fits one of the branches (the
--    others stay, in order). With after, a timer races the messages.
-- receive：枝のどれかに合う最初のメッセージ（ほかは、順のまま残る）。
-- after があれば、タイマーがメッセージと競走する
selectReal
  :: Me -> Chan -> Matcher -> Maybe (Integer, Action)
  -> IO (Either Stop Action)
selectReal me c fits late = do
  let box = boxOf c
      scan = do
        items <- readTVar (boxItems box)
        case firstFit fits items of
          Nothing -> retry
          Just (i, found) -> do
            -- Taken, even when its branch went wrong (the error is the
            -- receiver's): the message never comes back to crash again.
            -- 枝がまちがえても、取ったことにする（エラーは受け手のもの）：
            -- そのメッセージが戻ってきて、また落とすことはない
            writeTVar (boxItems box) (Seq.deleteAt i items)
            pure (either (Left . Failed) Right found)
  waiting me ("waits to receive from " <> renderChan c) $ case late of
    Nothing -> atomically scan
    Just (ms, other) -> do
      timer <- registerDelay (fromInteger (ms * 1000))
      atomically $ scan `orElse` do
        rang <- readTVar timer
        if rang then pure (Right other) else retry
-- ANCHOR_END: selectReal

-- ANCHOR: firstFit
-- 🧲 The place of the first message that fits, and what to do then
--    (or the error of its branch: 1 // 0 in it).
-- 合う最初のメッセージの場所と、そのときにすること（か、その枝のエラー：
-- 枝の中の 1 // 0）
firstFit :: Matcher -> Seq Value -> Maybe (Int, Either EvalError Action)
firstFit fits = go 0 . toList
  where
    go i items = case items of
      [] -> Nothing
      msg : rest -> case fits msg of
        Left err        -> Just (i, Left err)
        Right (Just a)  -> Just (i, Right a)
        Right Nothing   -> go (i + 1) rest
-- ANCHOR_END: firstFit

-- ANCHOR: runActor
-- 🎭 An actor: a job with a mailbox. If it crashes and nobody watches
--    it (Actor.monitor), say so on the standard error, as Erlang does:
--    a crash is never silent.
-- アクター：受け箱を持つ仕事。落ちたのに、だれも見張っていなければ
-- （Actor.monitor）、Erlang と同じく標準エラーで知らせる。落ちたことを
-- 黙っていない
runActor :: Me -> Handler IO -> Action -> IO (Either Stop Value)
runActor me@(Me k name) handle work = do
  r <- runReal me handle work
  case downValue r of
    VCon "Crashed" _ [VString why] -> do
      watched <- Set.member k <$> readTVarIO watchedJobs
      unless watched $
        () <$ handle (PutErr (name <> " crashed: " <> why <> "\n"))
    _ -> pure ()
  pure r

-- 👀 Watch an actor: when its job ends, put f down in box. A thread of
--    its own waits for it.
-- アクターを見張る：その仕事が終わったら、f down を box に入れる。待つのは、
-- 自分のスレッド
monitorReal
  :: Pid -> (Value -> Either EvalError Value) -> Chan
  -> IO (Either Stop Value)
monitorReal watched f box = case pidJob watched of
  Just (RealTask k t) -> do
    atomically (modifyTVar' watchedJobs (Set.insert k))
    _ <- forkIO $ do
      ended <- try (waitCatch t)
      case ended of
        Right (Right r) -> deliver (downValue r)
        Right (Left oops) ->
          deliver (con "Crashed" [VString (T.pack (show oops))])
        Left BlockedIndefinitelyOnSTM -> pure ()
    pure (Right unit)
  _ -> pure (Left (Failed
         (BuiltinFailed "Actor.monitor" "needs a real actor")))
  where
    deliver down = case f down of
      Right msg ->
        atomically (modifyTVar' (boxItems (boxOf box)) (Seq.|> msg))
      Left _    -> pure ()
-- ANCHOR_END: runActor

-- ANCHOR: downValue
-- 🪦 How a job ended, as a Lam value: Normal, or Crashed why.
-- 仕事の終わり方を、Lam の値に：Normal か、Crashed why
downValue :: Either Stop Value -> Value
downValue r = case r of
  Right _           -> con "Normal" []
  Left (Exited 0)   -> con "Normal" []
  Left (Exited n)   -> con "Crashed" [VString ("exit " <> T.pack (show n))]
  Left (Failed err) -> con "Crashed" [VString (renderError err)]
-- ANCHOR_END: downValue

-- ANCHOR: waiting
-- ⏳ Do something that may wait, and write down who waits for what.
--    When every thread waits for a variable that no running thread
--    can reach, GHC finds it (at a garbage collection) and throws
--    BlockedIndefinitely to them. Then the list tells who waited for
--    what: a deadlock.
-- 待つかもしれないことをやり、だれが何を待っているかを書いておく。
-- どのスレッドも、動いているスレッドからは届かない変数を待っていたら、
-- GHC が（ごみ集めのときに）見つけ、BlockedIndefinitely を投げる。
-- そのとき、書いておいた表が、だれが何を待っていたかを教える：デッドロック
waiting :: Me -> Text -> IO (Either Stop a) -> IO (Either Stop a)
waiting (Me k name) what work = do
  atomically (modifyTVar' waitingJobs (Map.insert k (name, what)))
  r <- (work `catches`
          [ E.Handler (\BlockedIndefinitelyOnSTM -> deadlock)
          , E.Handler (\BlockedIndefinitelyOnMVar -> deadlock) ])
       `onException` done
  -- After a deadlock, keep the line: another job may still read it.
  -- デッドロックのあとは、行を残す：ほかの仕事が、まだ読むかもしれない
  case r of
    Left (Failed (Deadlock _)) -> pure ()
    _                          -> done
  pure r
  where
    done = atomically (modifyTVar' waitingJobs (Map.delete k))
    deadlock = do
      waits <- readTVarIO waitingJobs
      pure (Left (Failed (Deadlock (Map.elems waits))))

-- 📋 Tables for the whole program (there is one real world): who
--    waits for what, the jobs someone watches, and the numbers.
-- プログラム全体の表（本物の世界は1つ）：だれが何を待っているか、
-- 見張られている仕事、番号
waitingJobs :: TVar (Map Int (Text, Text))
waitingJobs = unsafePerformIO (newTVarIO Map.empty)
{-# NOINLINE waitingJobs #-}

watchedJobs :: TVar (Set Int)
watchedJobs = unsafePerformIO (newTVarIO Set.empty)
{-# NOINLINE watchedJobs #-}

jobCounter :: TVar Int
jobCounter = unsafePerformIO (newTVarIO 0)
{-# NOINLINE jobCounter #-}

chanCounter :: TVar Int
chanCounter = unsafePerformIO (newTVarIO 0)
{-# NOINLINE chanCounter #-}

newJobId :: IO Int
newJobId = atomically (stateTVar jobCounter (\n -> (n + 1, n + 1)))
-- ANCHOR_END: waiting

unit :: Value
unit = VTuple []
-- ANCHOR_END: runAction
-- ANCHOR_END: all
