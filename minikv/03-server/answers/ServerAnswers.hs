-- ANCHOR: all
-- | Answers to the training of "A TCP server".
-- 「TCP サーバーにする」のトレーニングの解答
module ServerAnswers
  ( runCommands
  , acceptCounting
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', stateTVar)
import Control.Exception (bracket_)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (Socket, close)

import MiniKv.Protocol
import MiniKv.Server
import MiniKv.Store
import MiniKv.Wire

-- ANCHOR: together
-- 📦 Step 2: run several commands in one atomically. Nobody can come in
--    between them. (A failing command does not undo the others, as in
--    Redis's MULTI: "no one comes in between" is not "all or nothing".)
-- ステップ2：いくつものコマンドを、1つの atomically で。あいだに割りこめない。
-- （途中のコマンドが失敗しても、ほかは戻さない。Redis の MULTI と同じく、
-- 「割りこまれない」と「全部か無しか」は別の約束）
runCommands :: TVar Store -> [[ByteString]] -> IO [Wire]
runCommands storeVar requests = case traverse toCommand requests of
  Left message -> pure [WError ("ERR " <> encodeUtf8 message)]
  Right commands -> do
    replies <- atomically (stateTVar storeVar (applyAll commands))
    pure (map replyWire replies)
-- ANCHOR_END: together

-- ANCHOR: counting
-- 👥 Step 3: count the clients. Each guest adds 1 when it starts and
--    takes 1 away however it ends (bracket_ pairs them).
-- ステップ3：つながっている数を数える。お客さんは、始めに 1 足し、
-- どう終わっても 1 引く（bracket_ が組にする）
acceptCounting :: TVar Int -> TVar Store -> Socket -> IO ()
acceptCounting clients storeVar server = forever $ do
  client <- acceptNext server
  let arrive = atomically (modifyTVar' clients (+ 1))
      leave  = atomically (modifyTVar' clients (subtract 1))
  forkFinally (bracket_ arrive leave (serveClient storeVar client))
              (\_ -> close client)
-- ANCHOR_END: counting
-- ANCHOR_END: all
