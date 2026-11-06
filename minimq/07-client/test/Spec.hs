module Main (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (race)
import Control.Exception (IOException, throwIO, try)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)
import Test.Hspec

import MiniMq.Client
import MiniMq.Guild
import MiniMq.Protocol
import MiniMq.Server
import MiniMq.Typed
import MiniMq.Types
import MiniMq.Wire (Conn)
import MqAnswers

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

-- ✉️ The first value `consume` hands over; then stop consuming.
-- consume が渡す最初の値を受け取って、受け取るのをやめる
firstOf :: Conn -> Mailbox a -> IO a
firstOf conn box = do
  slot <- newEmptyMVar
  either id (const (error "consume never ends"))
    <$> race (takeMVar slot) (consume conn box (putMVar slot))

scary :: IOException
scary = userError "too scary"

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
    -- 読めない手紙は、すぐに dead-letters へ
    it "rejects a letter it cannot read, straight to dead-letters" $
      withServer $ \port -> withClient port $ \c -> do
        setUp c
        _ <- request c (Publish "quests" (Message "quest.dragon" "not json"))
        withClient port $ \c2 ->
          timeout 1000000 (consume c2 dragonTeam (\_ -> pure ()))
            `shouldReturn` Nothing
        timeout 1000000 (firstOf c deadLetters)
          `shouldReturn` Just "not json"
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
