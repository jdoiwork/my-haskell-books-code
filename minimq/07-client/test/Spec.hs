module Main (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (race, withAsync)
import Control.Exception (IOException, throwIO, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)
import Test.Hspec

import MiniMq.Client
import MiniMq.Crew
import MiniMq.Guild
import MiniMq.Protocol
import MiniMq.Server
import MiniMq.Typed
import MiniMq.Types
import MiniMq.Wire (Conn)
import MqAnswers

-- ANCHOR: setup
-- 🏗️ The guild's desks and mailboxes, made with the typed client.
-- 型つきのクライアントで作った、ギルドの仕分け台と郵便受け
setUp :: Conn -> IO ()
setUp conn = do
  declareDesk conn questBoard
  declareDesk conn graveyard
  declareMailbox conn dragonTeam
  declareMailbox conn deadLetters
  bindTo conn questBoard "quest.dragon.#" dragonTeam
  bindTo conn graveyard  ""               deadLetters

-- ✉️ The first value `consume` hands over; then stop consuming. `race`
--    may stop `consume` before it says ACK: this only checks the value.
-- consume が渡す最初の値を受け取って、受け取るのをやめる。race は、
-- ACK を言う前に consume を止めるかもしれない。確かめるのは値だけ
firstOf :: Conn -> Mailbox a -> IO a
firstOf conn box = do
  slot <- newEmptyMVar
  either id (const (error "consume never ends"))
    <$> race (takeMVar slot) (consume conn box (putMVar slot))

-- 😱 What a handler throws when the job goes wrong.
-- 仕事がうまくいかなかったときに、受け手の関数が投げるもの
scary :: IOException
scary = userError "too scary"
-- ANCHOR_END: setup

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  describe "the typed client" $ do
    -- 送った Quest が、そのまま Quest として届く
    it "sends a Quest and gets the same Quest" $ withServer $ \port ->
      withClient port $ \c -> do
        setUp c
        publishTo c questBoard "quest.dragon.north" (Quest "Red dragon" 500)
          `shouldReturn` 1
        timeout 1000000 (firstOf c dragonTeam)
          `shouldReturn` Just (Quest "Red dragon" 500)
    -- 処理が戻ったら ACK。だから次の手紙が来て、終わった手紙は戻らない
    it "says ACK when the handler returns" $ withServer $ \port -> do
      let red  = Quest "Red dragon" 500
          blue = Quest "Blue dragon" 300
      withClient port $ \c -> do
        setUp c
        _ <- publishTo c questBoard "quest.dragon" red
        _ <- publishTo c questBoard "quest.dragon" blue
        -- One letter at a time (prefetch 1): blue comes only after red
        -- got its ACK. Then hold blue, with no answer, and hang up.
        -- 一度に1通（prefetch 1）。blue は、red に ACK が届いてから来る。
        -- blue は返事をせずに持ったまま、つながりを切る
        reds <- newIORef (0 :: Int)
        slot <- newEmptyMVar
        let handle q
              | q == red  = atomicModifyIORef' reds (\n -> (n + 1, ()))
              | otherwise = putMVar slot q >> threadDelay 10000000
            secondOf c2 = race (takeMVar slot) (consume c2 dragonTeam handle)
        withClient port (timeout 1000000 . secondOf)
          `shouldReturn` Just (Left blue)
        readIORef reds `shouldReturn` 1
      -- Blue comes back; red, which got its ACK, does not.
      -- blue は戻ってくる。ACK をもらった red は戻らない
      withClient port $ \c ->
        timeout 1000000 (firstOf c dragonTeam) `shouldReturn` Just blue
    -- 処理が例外を投げたら、手紙は戻り、次の受け手に届く
    it "puts a letter back when the handler throws" $ withServer $ \port -> do
      withClient port $ \c -> do
        setUp c
        _ <- publishTo c questBoard "quest.dragon" (Quest "Blue dragon" 300)
        try @IOException (consume c dragonTeam (\_ -> throwIO scary))
          `shouldReturn` Left scary
      withClient port $ \c ->
        timeout 1000000 (firstOf c dragonTeam)
          `shouldReturn` Just (Quest "Blue dragon" 300)
    -- 読めない手紙は、受け手の関数に渡らず、すぐに dead-letters へ
    it "rejects a letter it cannot read, straight to dead-letters" $
      withServer $ \port -> withClient port $ \c -> do
        setUp c
        _ <- request c (Publish "quests" (Message "quest.dragon" "not json"))
        calls <- newIORef (0 :: Int)
        let count _ = atomicModifyIORef' calls (\n -> (n + 1, ()))
        withClient port $ \c2 ->
          withAsync (consume c2 dragonTeam count) $ \_ ->
            timeout 1000000 (firstOf c deadLetters)
              `shouldReturn` Just "not json"
        readIORef calls `shouldReturn` 0
    -- 監督役の子が倒れたら、つながりが閉じ、始め直した子に同じ手紙が届く
    it "a crew that falls over gets the letter again" $
      withServer $ \port -> do
        let green = Quest "Green dragon" 200
        withClient port $ \c -> do
          setUp c
          publishTo c questBoard "quest.dragon" green `shouldReturn` 1
        tries <- newIORef (0 :: Int)
        slot  <- newEmptyMVar
        let handle _outbox q = do
              n <- atomicModifyIORef' tries (\k -> (k + 1, k + 1))
              if n == 1 then throwIO scary else putMVar slot (q, n)
        timeout 2000000 (race (takeMVar slot) (dragonCrew port handle))
          `shouldReturn` Just (Left (green, 2))
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "lineQuest reads what it writes" $
      fromBody lineQuest (toBody lineQuest (Quest "Slay a slime" 30))
        `shouldBe` Right (Quest "Slay a slime" 30)
    it "invmap turns a codec of Text into a codec of lengths" $ do
      let plainText = Codec id Right :: Codec Text
          sizes = invmap T.length (`T.replicate` "x") plainText
      toBody sizes 3 `shouldBe` "xxx"
      fromBody sizes "xxxx" `shouldBe` Right 4
