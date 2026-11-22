-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | A fake world's runner: jobs take turns on a fake clock. A job
--   runs until it asks the world for something, or must wait (for a
--   message, a free place, another job). Then the job with the
--   earliest clock goes on. The same program gives the same turns,
--   every run (or, with a seed, other turns, the same for that seed).
-- 偽物の世界の実行役：仕事は、偽の時計の上で順番に動く。仕事は、世界に
-- 何かを頼むか、待たなければならない（メッセージ、空き、ほかの仕事）
-- ところまで動く。それから、時計のいちばん早い仕事が続ける。同じ
-- プログラムなら、いつも同じ順番（種を渡せば別の順番。同じ種なら同じ）
module Lam.Scheduler
  ( Clockwork (..)
  , schedule
  ) where

import Control.Monad (forM_, when)
import Control.Monad.State.Strict
  (StateT, evalStateT, get, gets, lift, modify')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Lam.Builtins (con)
import Lam.Eval
import Lam.Io
  (Handler, caught, collected, downValue, firstFit, jobName, stopsOthers)
-- ANCHOR_END: head

-- ANCHOR: clockwork
-- ⏰ What the scheduler needs from a fake world: its clock, numbers
--    for new channels, and (with a seed) a random number for each
--    turn, to mix the jobs whose clocks are the same.
-- スケジューラーが偽物の世界から借りるもの：時計、新しいチャネルの番号、
-- （種があれば）同じ時刻の仕事を混ぜるための、番ごとの乱数
data Clockwork m = Clockwork
  { clockNow  :: m Integer
  , clockSet  :: Integer -> m ()
  , newChanId :: m Int
  , tieBreak  :: m (Maybe Int)
  }
-- ANCHOR_END: clockwork

-- ANCHOR: thread
-- 🧵 A job of the fake world: its name, its clock, its place in the
--    line (the lower goes first at the same time), the rest of its
--    work (the stack of frames), and what it is doing now. A job that
--    was cancelled goes on only with its cleanups.
-- 偽物の世界の仕事：名前、時計、列の中の場所（同じ時刻なら小さいほうが
-- 先）、残りの仕事（札の山）、いましていること。取り消された仕事は、
-- 後始末だけを続ける
data Thread = Thread
  { thName      :: Text
  , thTime      :: Integer
  , thKey       :: Int
  , thStack     :: [Frame]
  , thState     :: ThreadState
  , thGroup     :: Maybe Int  -- the IO.all or IO.race it is a part of
  , thActor     :: Bool
  , thCancelled :: Bool
  }

-- ANCHOR: frame
-- 🧱 One frame of the stack: what to do when the action above it ends.
--    Then: the rest of a Bind (only for a value). Cleanup: the cleanup
--    of IO.finally (for a value or a stop). Catching: IO.try. After:
--    a cleanup is running; then go on with this answer.
-- 山の札1枚：上の頼みごとが終わったら、すること。Then：Bind の続き
-- （値のときだけ）。Cleanup：IO.finally の後始末（値でも止まっても）。
-- Catching：IO.try。After：後始末の途中。終わったら、この答えで続ける
data Frame
  = Then (Value -> Either EvalError Action)
  | Cleanup Action
  | Catching
  | After (Either Stop Value)
-- ANCHOR_END: frame

-- 🚦 Ready to go on with an action, waiting, or ended.
-- 続けられる、待っている、終わった
data ThreadState
  = Ready Action
  | Blocked Wait
  | Done (Either Stop Value)

-- ⏳ What a job waits for.
-- 仕事が待っているもの
data Wait
  = OnReceive Chan              -- Chan.recv: a value, or the close
  | OnRoom Chan Value           -- Chan.send: room in a full channel
  | OnTaken Chan Integer        -- Chan.new 0: a receiver for the ticket
  | OnMail Chan Matcher (Maybe (Integer, Action))  -- receive (deadline)
  | OnTask Int                  -- await
  | OnGroup Int                 -- IO.all, IO.race
-- ANCHOR_END: thread

-- ANCHOR: group
-- 👪 The jobs of one IO.all (or IO.race): who waits for them, the
--    ones not started yet, the ones running (and their places in the
--    list), the answers so far, and the latest end.
-- 1つの IO.all（か IO.race）の仕事たち：だれが待っているか、まだ始めて
-- いないもの、動いているもの（とリストの中の場所）、ここまでの答え、
-- いちばん遅い終わり
data Group = Group
  { gParent  :: Int
  , gRace    :: Bool
  , gEarly   :: Early
  , gLimit   :: Int
  , gTodo    :: [(Int, Action)]
  , gRunning :: Map Int Int
  , gDone    :: Map Int Value
  , gEnd     :: Integer
  , gClosing :: Maybe (Either Stop Value, Set Int)
  }

-- 📬 The values of a fake channel. (Its capacity is in the Chan.)
-- 偽物のチャネルの中身（容量は Chan の中）
data Box = Box
  { fakeItems  :: Seq Value
  , fakeClosed :: Bool
  , fakeSent   :: Integer
  , fakeTaken  :: Integer
  }

-- 📋 Everything the scheduler keeps.
-- スケジューラーが持っているもの全部
data Sched = Sched
  { sThreads  :: Map Int Thread
  , sNext     :: Int
  , sGroups   :: Map Int Group
  , sNextGroup :: Int
  , sBoxes    :: Map Int Box
  , sMonitors :: Map Int [(Value -> Either EvalError Value, Chan)]
  , sWatched  :: Set Int
  }
-- ANCHOR_END: group

type S m = StateT Sched m

-- ANCHOR: schedule
-- 🎬 Run an action as main (job 0), with every job it starts, until
--    main ends. When main ends, the program ends: the jobs left are
--    gone (the same as the real world). The clock stops at main's end.
-- 頼みごとを main（0 番の仕事）として、それが始める仕事も全部、main が
-- 終わるまで動かす。main が終われば、プログラムは終わり。残った仕事は
-- 消える（本物の世界と同じ）。時計は main の終わりで止まる
schedule
  :: Monad m => Clockwork m -> Handler m -> Action -> m (Either Stop Value)
schedule clock handle action = do
  start <- clockNow clock
  let main = Thread "main" start 0 [] (Ready action) Nothing False False
      sched = Sched (Map.singleton 0 main) 1 Map.empty 0 Map.empty
                    Map.empty Set.empty
  evalStateT (loop clock handle) sched

-- 🔁 Pick the job whose clock is the earliest (then the lowest place
--    in the line), let it go on until it must stop, wake the jobs it
--    let go on, and again.
-- 時計がいちばん早い（同じなら列の前の）仕事を選び、止まるところまで
-- 動かし、そのおかげで進める仕事を起こす。それをくり返す
loop :: Monad m => Clockwork m -> Handler m -> S m (Either Stop Value)
loop clock handle = do
  threads <- gets sThreads
  case thState <$> Map.lookup 0 threads of
    Just (Done r) -> do
      lift (clockSet clock (maybe 0 thTime (Map.lookup 0 threads)))
      pure r
    _ -> case candidates threads of
      [] -> do
        -- Nobody can go on, and main has not ended: a deadlock.
        -- だれも進めないのに、main が終わっていない：デッドロック
        waits <- whoWaits
        pure (Left (Failed (Deadlock waits)))
      cs -> do
        let (t, _, k) = minimum cs
        turn clock handle k t
        wake clock t
        loop clock handle

-- 🙋 The jobs that can go on, with when: the ready ones, and the ones
--    whose receive ... after has run out of time.
-- 進める仕事と、その時刻：用意のできたものと、receive … after の時間が
-- 切れたもの
candidates :: Map Int Thread -> [(Integer, Int, Int)]
candidates threads =
  [ (when', thKey th, k)
  | (k, th) <- Map.toList threads
  , Just when' <- [readyAt th] ]
  where
    readyAt th = case thState th of
      Ready _                                -> Just (thTime th)
      Blocked (OnMail _ _ (Just (deadline, _))) -> Just deadline
      _                                      -> Nothing
-- ANCHOR_END: schedule

-- ANCHOR: turn
-- ▶️ One turn of job k, at time t.
-- k 番の仕事の、1回の番。時刻 t に
turn :: Monad m => Clockwork m -> Handler m -> Int -> Integer -> S m ()
turn clock handle k t = do
  th <- thread k
  case thState th of
    Ready a -> advance clock handle k a
    -- The time of receive ... after is up: one last look, then the
    -- after branch.
    -- receive … after の時間切れ：最後にもう一度見て、だめなら after の枝
    Blocked (OnMail c fits (Just (_, late))) -> do
      found <- takeMail c fits
      setThread k th { thTime = t }
      case found of
        Left err         -> raise clock handle k (Failed err)
        Right (Just a)   -> advance clock handle k a
        Right Nothing    -> advance clock handle k late
    _ -> pure ()

-- ANCHOR: returned
-- ↩️ The action of job k gave v: the top frame says what is next.
-- k 番の仕事の頼みごとが v を返した。いちばん上の札が、次を決める
returned
  :: Monad m => Clockwork m -> Handler m -> Int -> Value -> S m ()
returned clock handle k v = do
  frame <- pop k
  case frame of
    Nothing             -> finish clock handle k (Right v)
    Just (Then rest)    -> either (raise clock handle k . Failed) go (rest v)
    Just (Cleanup c)    -> push k (After (Right v)) >> go c
    Just Catching       -> back (okOf v)
    Just (After r)      -> either (raise clock handle k) back r
  where
    go   = advance clock handle k
    back = returned clock handle k

-- 🛑 Job k stopped (an error, exit, or a cancel): go down the stack.
--    The rest of a Bind is skipped; a cleanup runs; IO.try turns an
--    error into an Err. If a cleanup stops too, that stop goes on (as
--    Haskell's).
-- k 番の仕事が止まった（エラー、exit、取り消し）：山を下りる。Bind の
-- 続きは飛ばし、後始末は動かし、IO.try はエラーを Err にする。後始末も
-- 止まったら、そちらの止まり方で続ける（Haskell と同じ）
raise :: Monad m => Clockwork m -> Handler m -> Int -> Stop -> S m ()
raise clock handle k stop = do
  frame <- pop k
  case frame of
    Nothing          -> finish clock handle k (Left stop)
    Just (Cleanup c) -> push k (After (Left stop)) >> advance clock handle k c
    Just Catching    -> case caught (Left stop) of
      Right v -> returned clock handle k v
      Left s  -> raise clock handle k s
    Just _           -> raise clock handle k stop
-- ANCHOR_END: returned

-- 🏃 Go on with job k, until it asks the world for something, must
--    wait, or ends.
-- k 番の仕事を、世界に頼むか、待つか、終わるところまで進める
advance :: Monad m => Clockwork m -> Handler m -> Int -> Action -> S m ()
advance clock handle k action = case action of
  Pure v -> returned clock handle k v
  Bind first rest -> push k (Then rest) >> advance clock handle k first
  Finally body cleanup ->
    push k (Cleanup cleanup) >> advance clock handle k body
  Catch body -> push k Catching >> advance clock handle k body
  Exit n -> raise clock handle k (Exited n)
  -- A request: the world's clock is this job's clock; the request may
  -- move it (sleep, a slow URL). Then the turn ends.
  -- 頼みごと：世界の時計を、この仕事の時計にする。頼みごとが時計を
  -- 進めることもある（sleep、遅い URL）。それで番はおしまい
  Ask request -> do
    th <- thread k
    lift (clockSet clock (thTime th))
    v <- lift (handle request)
    t' <- lift (clockNow clock)
    key <- keyFor clock k
    setThread k th { thTime = t', thKey = key, thState = Ready (Pure v) }
  Spawn work -> do
    j <- newThread clock jobName work Nothing False k
    advance clock handle k (Pure (VTask (FakeTask j)))
  Start box work -> do
    let name = const ("actor " <> renderChan box)
    j <- newThread clock name work Nothing True k
    advance clock handle k (Pure (VPid (Pid box (Just (FakeTask j)))))
  Await task -> waitFor clock handle k (OnTask (taskId task))
  NewChan capacity -> do
    n <- lift (newChanId clock)
    let c = Chan n capacity Nothing
    modify' (\s -> s { sBoxes = Map.insert n emptyBox (sBoxes s) })
    advance clock handle k
      (Pure (if capacity < 0 then VPid (Pid c Nothing) else VChan c))
  Send c v -> do
    sent <- putItem c v
    case sent of
      Left err -> raise clock handle k (Failed err)
      Right Nothing -> waitFor clock handle k (OnRoom c v)
      Right (Just ticket)
        | chanCapacity c == 0 -> waitFor clock handle k (OnTaken c ticket)
        | otherwise           -> advance clock handle k (Pure unit)
  Receive c -> waitFor clock handle k (OnReceive c)
  Close c -> do
    box <- fakeBoxOf c
    if fakeClosed box
      then raise clock handle k (Failed (BuiltinFailed "Chan.close"
             (renderChan c <> " is closed already")))
      else do
        setBox c box { fakeClosed = True }
        advance clock handle k (Pure unit)
  Select c fits late -> do
    th <- thread k
    let deadline = fmap (\(ms, a) -> (thTime th + ms, a)) late
    waitFor clock handle k (OnMail c fits deadline)
  Monitor watched f box -> do
    let j = maybe (-1) taskId (pidJob watched)
    modify' (\s -> s { sWatched = Set.insert j (sWatched s) })
    other <- gets (Map.lookup j . sThreads)
    case thState <$> other of
      Just (Done r) -> deliver f box (downValue r)
      _ -> modify' (\s -> s { sMonitors =
             Map.insertWith (flip (<>)) j [(f, box)] (sMonitors s) })
    advance clock handle k (Pure unit)
  Gather n early actions -> startGroup clock handle k False early n actions
  Race [] -> raise clock handle k
    (Failed (BuiltinFailed "IO.race" "got no actions"))
  Race actions -> startGroup clock handle k True WaitAll 0 actions
-- ANCHOR_END: turn

-- ANCHOR: waitFor
-- ⏳ Try what the job waits for now. If it can go on, go on;
--    if not, it waits (another job will wake it).
-- 待っているものを、いま試す。進めるなら進み、進めなければ待つ
--（ほかの仕事が起こしてくれる）
waitFor :: Monad m => Clockwork m -> Handler m -> Int -> Wait -> S m ()
waitFor clock handle k w = do
  tried <- attempt w
  case tried of
    Nothing -> modifyThread k (\th -> th { thState = Blocked w })
    Just (t, r) -> do
      modifyThread k (\th -> th { thTime = max t (thTime th) })
      advance clock handle k (actionOf r)

-- 🔓 Can the wait end now? If so: when (the clock of the one who let
--    it go), and what to do next. Taking a value changes the channel.
-- 待つのを、いま終えられるか。終えられるなら、いつか（起こしてくれた
-- 仕事の時計）と、次にすること。値を取ると、チャネルが変わる
attempt :: Monad m => Wait -> S m (Maybe (Integer, Either Stop Action))
attempt w = case w of
  OnReceive c -> do
    box <- fakeBoxOf c
    case Seq.viewl (fakeItems box) of
      v Seq.:< rest -> do
        setBox c box { fakeItems = rest, fakeTaken = fakeTaken box + 1 }
        now (Right (Pure (con "Just" [v])))
      Seq.EmptyL
        | fakeClosed box -> now (Right (Pure (con "Nothing" [])))
        | otherwise     -> pure Nothing
  OnRoom c v -> do
    sent <- putItem c v
    case sent of
      Left err -> now (Left (Failed err))
      Right Nothing -> pure Nothing
      Right (Just _) -> now (Right (Pure unit))
  OnTaken c ticket -> do
    box <- fakeBoxOf c
    if fakeTaken box >= ticket then now (Right (Pure unit)) else pure Nothing
  OnMail c fits _ -> do
    found <- takeMail c fits
    case found of
      Left err       -> now (Left (Failed err))
      Right (Just a) -> now (Right a)
      Right Nothing  -> pure Nothing
  OnTask j -> do
    other <- gets (Map.lookup j . sThreads)
    pure $ case other of
      Just Thread { thState = Done r, thTime = t } ->
        Just (t, either Left (Right . Pure) r)
      _ -> Nothing
  OnGroup _ -> pure Nothing
  where
    -- "now": the clock of the job that let it go is set by wake.
    -- 「いま」：起こした仕事の時計は、wake が入れる
    now r = pure (Just (0, r))
-- ANCHOR_END: waitFor

-- ANCHOR: wake
-- ⏰ After a turn at time t, wake every job whose wait can end now,
--    again and again, until nobody more can.
-- 時刻 t の番のあとで、待つのを終えられる仕事を全部起こす。もう
-- だれも起きなくなるまで、くり返す
wake :: Monad m => Clockwork m -> Integer -> S m ()
wake clock t = do
  threads <- gets sThreads
  woke <- fmap or $ mapM one
    [ (k, w) | (k, Thread { thState = Blocked w }) <- Map.toList threads ]
  when woke (wake clock t)
  where
    one (k, w) = do
      tried <- attempt w
      case tried of
        Nothing -> pure False
        Just (t', r) -> do
          key <- keyFor clock k
          modifyThread k $ \th -> th
            { thTime = maximum [t, t', thTime th]
            , thKey = key
            , thState = Ready (actionOf r) }
          pure True

-- 🎬 What to do after a wait: go on, or stop (a send to a closed
--    channel, a mistake in a branch of receive).
-- 待ったあとにすること：続けるか、止まるか（閉じたチャネルに送った、
-- receive の枝のまちがい）
actionOf :: Either Stop Action -> Action
actionOf r = case r of
  Right a             -> a
  Left (Exited n)     -> Exit n
  Left (Failed err)   -> Bind (Pure unit) (\_ -> Left err)
-- ANCHOR_END: wake

-- ANCHOR: finish
-- 🏁 Job k ended with r (its stack is empty). Tell the ones who watch
--    it, say so if an actor crashed and nobody watched, and tell its
--    IO.all. A cancelled job has done its cleanups: tell its group,
--    which waits for them before it lets the parent go on.
-- k 番の仕事が r で終わった（山は空）。見張っている仕事に知らせ、だれも
-- 見張っていないアクターが落ちたなら、そう言い、IO.all にも知らせる。
-- 取り消された仕事は、後始末を終えた：グループに知らせる。グループは、
-- それを待ってから親を続けさせる
finish
  :: Monad m => Clockwork m -> Handler m -> Int -> Either Stop Value
  -> S m ()
finish clock handle k r = do
  th <- thread k
  setThread k th { thState = Done r, thStack = [] }
  if thCancelled th
    then forM_ (thGroup th) $ \g -> cleanedUp clock handle g k (thTime th)
    else do
      let down = downValue r
      watchers <- gets (Map.findWithDefault [] k . sMonitors)
      forM_ watchers $ \(f, box) -> deliver f box down
      watched <- gets (Set.member k . sWatched)
      case down of
        VCon "Crashed" _ [VString why] | thActor th, not watched -> do
          lift (clockSet clock (thTime th))
          let said = thName th <> " crashed: " <> why <> "\n"
          _ <- lift (handle (PutErr said))
          pure ()
        _ -> pure ()
      forM_ (thGroup th) $ \g -> childEnded clock handle g k r (thTime th)
-- ANCHOR_END: finish

-- ANCHOR: groups
-- 👪 Start an IO.all (or an IO.race): make the jobs, start as many as
--    the limit lets, and let the parent wait.
-- IO.all（か IO.race）を始める：仕事を作り、上限まで始め、親を待たせる
startGroup
  :: Monad m => Clockwork m -> Handler m -> Int -> Bool -> Early -> Int
  -> [Action] -> S m ()
startGroup clock handle k isRace early n actions
  | null actions = advance clock handle k (Pure (collected early []))
  | otherwise = do
      gid <- gets sNextGroup
      th <- thread k
      let group = Group k isRace early n (zip [0 ..] actions) Map.empty
                        Map.empty (thTime th) Nothing
      modify' (\s -> s { sGroups = Map.insert gid group (sGroups s)
                       , sNextGroup = gid + 1 })
      setThread k th { thState = Blocked (OnGroup gid) }
      fill clock gid (thTime th)

-- ➕ Start more jobs of the group, while there is room.
-- 枠が空いているあいだ、グループの仕事をさらに始める
fill :: Monad m => Clockwork m -> Int -> Integer -> S m ()
fill clock gid t = do
  found <- gets (Map.lookup gid . sGroups)
  forM_ found $ \group -> case gTodo group of
    (i, work) : rest
      | gLimit group <= 0 || Map.size (gRunning group) < gLimit group -> do
          j <- newThread clock jobName work (Just gid) False
                 (gParent group)
          modifyThread j (\th -> th { thTime = t })
          putGroup gid group
            { gTodo = rest, gRunning = Map.insert j i (gRunning group) }
          fill clock gid t
    _ -> pure ()

-- 🔔 A job of the group ended at time t.
-- グループの仕事が、時刻 t に終わった
childEnded
  :: Monad m => Clockwork m -> Handler m -> Int -> Int -> Either Stop Value
  -> Integer -> S m ()
childEnded clock handle gid j r t = do
  found <- gets (Map.lookup gid . sGroups)
  forM_ found $ \group -> do
    let i = Map.findWithDefault 0 j (gRunning group)
        group' = group { gRunning = Map.delete j (gRunning group)
                       , gEnd = max t (gEnd group) }
    if gRace group || stopsOthers (gEarly group) r
      then do
        -- The first to end wins (race), or one stops the others. The
        -- others are cancelled; the parent waits for their cleanups.
        -- 最初に終わったものが勝つ（race）か、1つがほかを取り消す。ほかは
        -- 取り消し、親は、その後始末を待つ
        cleaning <- concat <$> mapM (cancel t) (Map.keys (gRunning group'))
        putGroup gid group'
          { gRunning = Map.empty, gTodo = [], gEnd = t
          , gClosing = Just (r, Set.fromList cleaning) }
        closeIfClean clock handle gid
      else do
        let done = Map.insert i (either (const unit) id r) (gDone group')
        putGroup gid group' { gDone = done }
        fill clock gid t
        now' <- gets (Map.lookup gid . sGroups)
        case now' of
          Just g | Map.null (gRunning g), null (gTodo g) ->
            resume clock handle gid (gParent g) (gEnd g)
              (Right (collected (gEarly g) (Map.elems (gDone g))))
          _ -> pure ()

-- 🧹 A cancelled job of the group has done its cleanups, at time t.
-- グループの取り消された仕事が、時刻 t に後始末を終えた
cleanedUp
  :: Monad m => Clockwork m -> Handler m -> Int -> Int -> Integer -> S m ()
cleanedUp clock handle gid j t = do
  found <- gets (Map.lookup gid . sGroups)
  forM_ found $ \group -> forM_ (gClosing group) $ \(r, left) -> do
    putGroup gid group { gEnd = max t (gEnd group)
                       , gClosing = Just (r, Set.delete j left) }
    closeIfClean clock handle gid

-- 🚪 When no cancelled job is still cleaning up, the parent goes on.
-- 後始末の途中の仕事がなくなったら、親が続ける
closeIfClean :: Monad m => Clockwork m -> Handler m -> Int -> S m ()
closeIfClean clock handle gid = do
  found <- gets (Map.lookup gid . sGroups)
  forM_ found $ \group -> case gClosing group of
    Just (r, left) | Set.null left ->
      resume clock handle gid (gParent group) (gEnd group) r
    _ -> pure ()

-- ↩️ The group is over: the parent goes on at time t, with r.
-- グループはおしまい：親は時刻 t から、r で続ける
resume
  :: Monad m => Clockwork m -> Handler m -> Int -> Int -> Integer
  -> Either Stop Value -> S m ()
resume clock handle gid parent t r = do
  modify' (\s -> s { sGroups = Map.delete gid (sGroups s) })
  th <- thread parent
  key <- keyFor clock parent
  setThread parent th { thTime = max t (thTime th), thKey = key
                      , thState = Ready (Pure unit) }
  case r of
    Left stop -> raise clock handle parent stop
    Right v   -> modifyThread parent (\p -> p { thState = Ready (Pure v) })

-- ANCHOR: cancel
-- ✂️ Cancel job j at time t, and the groups it waits for. A job with
--    cleanups on its stack (IO.finally, defer) goes on with only them,
--    from time t, as if it had stopped there; a job without them is
--    gone at once. The answer: the jobs still cleaning up.
-- j 番の仕事を時刻 t に取り消す。その仕事が待っているグループも。山に
-- 後始末（IO.finally、defer）のある仕事は、時刻 t から後始末だけを
-- 続ける（そこで止まったように）。ない仕事は、すぐに消える。答えは、
-- まだ後始末をしている仕事
cancel :: Monad m => Integer -> Int -> S m [Int]
cancel t j = do
  found <- gets (Map.lookup j . sThreads)
  inner <- case thState <$> found of
    Just (Blocked (OnGroup gid)) -> do
      g <- gets (Map.lookup gid . sGroups)
      modify' (\s -> s { sGroups = Map.delete gid (sGroups s) })
      concat <$> mapM (cancel t) (maybe [] (Map.keys . gRunning) g)
    _ -> pure []
  case found of
    Just th | any cleans (thStack th) -> do
      let busy = any isAfter (thStack th)
      setThread j th
        { thCancelled = True
        , thStack = if busy then thStack th else filter cleans (thStack th)
        , thState = if busy then thState th else Ready (Exit 0)
        , thTime = if busy then thTime th else t }
      pure (j : inner)
    _ -> do
      modify' (\s -> s { sThreads = Map.delete j (sThreads s) })
      pure inner
  where
    cleans f = case f of
      Cleanup _ -> True
      After _   -> True
      _         -> False
    isAfter f = case f of
      After _ -> True
      _       -> False
-- ANCHOR_END: cancel
-- ANCHOR_END: groups

-- ANCHOR: deadlock
-- 🧊 Nobody can go on: who waits, and for what.
-- だれも進めない：だれが、何を待っているか
whoWaits :: Monad m => S m [(Text, Text)]
whoWaits = do
  s <- get
  pure [ (thName th, what s w)
       | th@Thread { thState = Blocked w } <- Map.elems (sThreads s) ]
  where
    what s w = case w of
      OnReceive c   -> "waits to receive from " <> renderChan c
      OnRoom c _    -> "waits to send to " <> renderChan c
      OnTaken c _   -> "waits to hand over to " <> renderChan c
      OnMail c _ _  -> "waits to receive from " <> renderChan c
      OnTask j      -> "waits for " <> jobName j <> " (await)"
      OnGroup gid   -> case Map.lookup gid (sGroups s) of
        Just g | gRace g -> "waits for the race (IO.race)"
        _                -> "waits for its jobs (IO.all)"
-- ANCHOR_END: deadlock

-- 🧰 Small helpers.
-- 小さな道具
thread :: Monad m => Int -> S m Thread
thread k = gets (Map.findWithDefault gone k . sThreads)
  where gone = Thread "gone" 0 k [] (Done (Right unit)) Nothing False False

-- 🧱 Put a frame on the stack of job k, or take the top one off.
-- k 番の仕事の山に札を積む。いちばん上の札を取る
push :: Monad m => Int -> Frame -> S m ()
push k f = modifyThread k (\th -> th { thStack = f : thStack th })

pop :: Monad m => Int -> S m (Maybe Frame)
pop k = do
  th <- thread k
  case thStack th of
    []         -> pure Nothing
    f : more   -> Just f <$ setThread k th { thStack = more }

putGroup :: Monad m => Int -> Group -> S m ()
putGroup gid g = modify' (\s -> s { sGroups = Map.insert gid g (sGroups s) })

-- 🎁 IO.try of a value: Ok with it.
-- 値を IO.try したもの：Ok で包む
okOf :: Value -> Value
okOf v = either (const unit) id (caught (Right v))

setThread :: Monad m => Int -> Thread -> S m ()
setThread k th = modify' (\s -> s { sThreads =
  if Map.member k (sThreads s) then Map.insert k th (sThreads s)
                               else sThreads s })

modifyThread :: Monad m => Int -> (Thread -> Thread) -> S m ()
modifyThread k f =
  modify' (\s -> s { sThreads = Map.adjust f k (sThreads s) })

-- 🆕 A new job, started at the clock of its parent p.
-- 新しい仕事。親 p の時計で始める
newThread
  :: Monad m => Clockwork m -> (Int -> Text) -> Action -> Maybe Int
  -> Bool -> Int -> S m Int
newThread clock name work group actor p = do
  j <- gets sNext
  t <- thTime <$> thread p
  key <- keyFor clock j
  let th = Thread (name j) t key [] (Ready work) group actor False
  modify' (\s -> s { sThreads = Map.insert j th (sThreads s), sNext = j + 1 })
  pure j

-- 🔢 The place in the line for a job that is ready again: its number,
--    or a random one (with a seed).
-- また用意のできた仕事の、列の中の場所：番号か、（種があれば）乱数
keyFor :: Monad m => Clockwork m -> Int -> S m Int
keyFor clock k = fromMaybe k <$> lift (tieBreak clock)

fakeBoxOf :: Monad m => Chan -> S m Box
fakeBoxOf c = gets (Map.findWithDefault emptyBox (chanId c) . sBoxes)

emptyBox :: Box
emptyBox = Box Seq.empty False 0 0

setBox :: Monad m => Chan -> Box -> S m ()
setBox c box =
  modify' (\s -> s { sBoxes = Map.insert (chanId c) box (sBoxes s) })

-- 📤 Put a value in, if there is room: Just its ticket. A closed
--    channel is a mistake.
-- 空きがあれば値を入れる：札を Just で。閉じたチャネルは、まちがい
putItem :: Monad m => Chan -> Value -> S m (Either EvalError (Maybe Integer))
putItem c v = do
  box <- fakeBoxOf c
  if fakeClosed box
    then pure (Left (BuiltinFailed "Chan.send"
                       (renderChan c <> " is closed")))
    else if chanCapacity c > 0 && Seq.length (fakeItems box) >= chanCapacity c
      then pure (Right Nothing)
      else do
        let ticket = fakeSent box + 1
        setBox c box { fakeItems = fakeItems box Seq.|> v, fakeSent = ticket }
        pure (Right (Just ticket))

-- 📭 Take the first message that fits, if any (even if its branch
--    went wrong: see firstFit).
-- 合う最初のメッセージがあれば、取る（枝がまちがえても：firstFit を見る）
takeMail
  :: Monad m => Chan -> Matcher -> S m (Either EvalError (Maybe Action))
takeMail c fits = do
  box <- fakeBoxOf c
  case firstFit fits (fakeItems box) of
    Nothing -> pure (Right Nothing)
    Just (i, found) -> do
      setBox c box { fakeItems = Seq.deleteAt i (fakeItems box) }
      pure (Just <$> found)

-- 📮 Put f down in a mailbox (for a monitor).
-- 受け箱に f down を入れる（見張りのため）
deliver
  :: Monad m => (Value -> Either EvalError Value) -> Chan -> Value
  -> S m ()
deliver f box down = case f down of
  Right msg -> do
    b <- fakeBoxOf box
    setBox box b { fakeItems = fakeItems b Seq.|> msg }
  Left _ -> pure ()

unit :: Value
unit = VTuple []

-- ANCHOR_END: all
