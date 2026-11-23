-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Jobs that talk: Go's channels (Chan) and Erlang's mailboxes
--   (Actor). Each builtin only makes an action; the runner does it,
--   and waits when it must.
-- 仕事どうしで話す：Go のチャネル（Chan）と、Erlang の受け箱（Actor）。
-- どの組み込みも、頼みごとを作るだけ。やるのは実行役で、待つべきときは
-- 待つ
module Lam.Channels
  ( channelBuiltins
  , actorBuiltins
  ) where

import Lam.Builtins
import Lam.Eval
import Lam.Io (ioOf)
import Lam.Syntax (Name)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: chanTable
-- 📋 The rows for Go's channels.
-- Go のチャネルの行
channelBuiltins :: [Builtin]
channelBuiltins =
  -- Chan.new 0: hand each value over. Chan.new 3: keep up to 3.
  -- Chan.new 0：1つずつ手渡す。Chan.new 3：3つまでためる
  [ prim1 "Chan.new" (TInt ~> tIO (tChan a)) $ \v -> case v of
      VInt n | n >= 0 -> Right (VIO (NewChan (fromInteger n)))
      _ -> Left (BuiltinFailed "Chan.new" "needs an Int, 0 or more")
  -- Chan.send c v: put v in; wait while there is no room.
  -- Chan.send c v：v を入れる。空きがなければ待つ
  , prim2 "Chan.send" (tChan a ~> a ~> tIO tUnit) $ \c x ->
      (\ch -> VIO (Send ch x)) <$> chanOf "Chan.send" c
  -- Chan.recv c: Just the next value; Nothing once it is closed and
  -- empty. Wait while it is open and empty.
  -- Chan.recv c：次の値を Just で。閉じて空なら Nothing。開いていて
  -- 空なら待つ
  , prim1 "Chan.recv" (tChan a ~> tIO (tMaybe a)) $ \c ->
      VIO . Receive <$> chanOf "Chan.recv" c
  -- Chan.close c: no more values. The receivers get Nothing at the end.
  -- Chan.close c：もう送らない。受け手は、最後に Nothing を受け取る
  , prim1 "Chan.close" (tChan a ~> tIO tUnit) $ \c ->
      VIO . Close <$> chanOf "Chan.close" c
  ]
  where
    a = TVar 0
    tChan x = TCon "Chan" [x]
    tMaybe x = TCon "Maybe" [x]
-- ANCHOR_END: chanTable

-- ANCHOR: actorTable
-- 📋 The rows for Erlang's mailboxes. A Pid msg takes only msg: the
--    type checker stops a message of another type.
-- Erlang の受け箱の行。Pid msg が受け取るのは msg だけ。ほかの型の
-- メッセージは、型検査器が止める
actorBuiltins :: [Builtin]
actorBuiltins =
  -- Actor.new: a mailbox for this job (to get answers in).
  -- Actor.new：この仕事のための受け箱（答えを受け取るため）
  [ Builtin "Actor.new" (closeOver (tIO (tPid m))) (VIO (NewChan (-1)))
  -- Actor.spawnOn box f: start f box as a job of its own; box is its
  -- mailbox. (Actor.spawn f makes the box, too.)
  -- Actor.spawnOn box f：f box を自分の仕事として始める。box が受け箱。
  -- （Actor.spawn f は、箱も作る）
  , prim2 "Actor.spawnOn" (tPid m ~> (tPid m ~> tIO a) ~> tIO (tPid m)) $
      \p f -> do
        box <- pidOf "Actor.spawnOn" p
        job <- apply f p >>= ioOf
        Right (VIO (Start (pidChan box) job))
  -- Actor.send pid msg: put msg in the mailbox. It never waits.
  -- Actor.send pid msg：受け箱に msg を入れる。決して待たない
  , prim2 "Actor.send" (tPid m ~> m ~> tIO tUnit) $ \p x -> do
      box <- pidOf "Actor.send" p
      Right (VIO (Send (pidChan box) x))
  -- ANCHOR: receiveRows
  -- What receive opens into (see the parser): the branches as one
  -- function, msg -> Maybe (IO a).
  -- receive が開く先（パーサーを見る）：枝を1つの関数にしたもの
  , prim2 "Actor.receiveWith"
      (tPid m ~> (m ~> tMaybe (tIO a)) ~> tIO a) $ \p f -> do
      box <- pidOf "receive" p
      Right (VIO (Select (pidChan box) (matcherOf f) Nothing))
  , prim4 "Actor.receiveAfter"
      (tPid m ~> (m ~> tMaybe (tIO a)) ~> TInt ~> tIO a ~> tIO a) $
      \p f t late -> do
        box <- pidOf "receive" p
        ms <- case t of
          VInt n | n >= 0 -> Right n
          _ -> Left (BuiltinFailed "receive" "after needs an Int, 0 or more")
        other <- ioOf late
        Right (VIO (Select (pidChan box) (matcherOf f) (Just (ms, other))))
  -- ANCHOR_END: receiveRows
  -- ANCHOR: monitorRow
  -- Actor.monitor pid me f: when the job of pid ends, me gets f down.
  -- Actor.monitor pid me f：pid の仕事が終わったら、me に f down が届く
  , prim3 "Actor.monitor"
      (tPid a ~> tPid m ~> (tDown ~> m) ~> tIO tUnit) $ \p q f -> do
      watched <- pidOf "Actor.monitor" p
      me <- pidOf "Actor.monitor" q
      case pidJob watched of
        Nothing -> Left (BuiltinFailed "Actor.monitor"
          (renderChan (pidChan watched)
             <> " has no job (it was made with Actor.new)"))
        Just _ -> Right (VIO (Monitor watched (apply f) (pidChan me)))
  -- ANCHOR_END: monitorRow
  ]
  where
    a = TVar 0
    m = TVar 1
    tPid x = TCon "Pid" [x]
    tMaybe x = TCon "Maybe" [x]
    tDown = TCon "Down" []
-- ANCHOR_END: actorTable

-- ANCHOR: matcherOf
-- 🧲 The Lam function of the branches, as a Matcher: Just io, or
--    Nothing (this message stays in the mailbox).
-- 枝の Lam の関数を、Matcher に：Just io か、Nothing（このメッセージは
-- 受け箱に残る）
matcherOf :: Value -> Matcher
matcherOf f msg = do
  r <- apply f msg
  case r of
    VCon "Just" _ [io] -> Just <$> ioOf io
    _                  -> Right Nothing
-- ANCHOR_END: matcherOf


-- 🔍 The channel, or the mailbox, inside a value.
-- 値の中のチャネル、受け箱
chanOf :: Name -> Value -> Either EvalError Chan
chanOf n v = case v of
  VChan c -> Right c
  _       -> Left (BuiltinFailed n "needs a Chan")

pidOf :: Name -> Value -> Either EvalError Pid
pidOf n v = case v of
  VPid p -> Right p
  _      -> Left (BuiltinFailed n "needs a Pid")
-- ANCHOR_END: all
