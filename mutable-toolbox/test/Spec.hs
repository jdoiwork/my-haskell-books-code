-- | Tests for the "choosing a box" chapter.
-- 「書き換えられる箱の使い分け」の章のテスト
module Main (main) where

import Control.Concurrent.Async (forConcurrently, replicateConcurrently_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (ErrorCall (..), throwIO, try)
import Control.Monad.State.Strict (evalState)
import Data.IORef
import Data.List (sort)
import qualified Data.Text as T
import Test.Hspec

import Appraise
import Board
import Core
import Debounce (debounceDemo)
import Gather
import Stumble (stumbleDemo)
import Tickets
import ToolboxAnswers

main :: IO ()
main = hspec $ do
  describe "tickets" $ do
    -- 「同時に配っても、番号は重ならない」
    it "never hands out the same number twice" $
      ticketRush `shouldReturn` True
    -- 「続き番号をまとめて渡す」
    it "hands out numbers in a row for a party" $ do
      box <- newTicketBox
      rows <- forConcurrently [1 .. 50 :: Int] (\_ -> takeTickets box 4)
      sort (concat rows) `shouldBe` [1 .. 200]
      all (\r -> r == take 4 [minimum r ..]) rows `shouldBe` True

  describe "the pure core in four boxes" $
    -- 「どの箱でも、同じ番号を返す」
    it "gives the same numbers in every box" $ do
      let titles = ["slime", "goblin", "dragon"]
          viaState = evalState (mapM pinState titles) emptyBoard
      ioref <- newIORef emptyBoard
      viaIORef <- mapM (pinIORef ioref) titles
      mvar <- newMVar emptyBoard
      viaMVar <- mapM (pinMVar mvar) titles
      tvar <- newTVarIO emptyBoard
      viaTVar <- mapM (atomically . pinTVar tvar) titles
      [viaState, viaIORef, viaMVar, viaTVar] `shouldBe` replicate 4 [1, 2, 3]

  describe "cachedOnce" $ do
    -- 「同時に頼まれても、鑑定は1回」
    it "appraises once even when asked at the same time" $ do
      calls <- newIORef 0
      cache <- newCache
      replicateConcurrently_ 20 (cachedOnce cache (slowAppraise calls) "sword")
      readIORef calls `shouldReturn` 1
    -- 「失敗は覚えず、次はもう一度鑑定する」
    it "does not remember a failure" $ do
      cache <- newCache
      r <- try (cachedOnce cache (\_ -> throwIO (ErrorCall "cursed")) "ring")
      r `shouldBe` Left (ErrorCall "cursed")
      cachedOnce cache (pure . T.length) "ring" `shouldReturn` 4

  describe "debounce" $
    -- 「続けて来た変更は、1回の保存にまとまる」
    it "puts changes close together into one save" $
      debounceDemo `shouldReturn` [["a", "b", "c"], ["d"]]

  describe "STM" $ do
    -- 「3人そろえば、パーティーになる」
    it "gathers a party" $
      gatherDemo ["Lambda", "Python", "Rust"] 1000000
        `shouldReturn` Just ["Lambda", "Python", "Rust"]
    -- 「そろわなければ、時間切れ」
    it "gives up when time runs out" $
      gatherDemo ["Lambda", "Python"] 300000 `shouldReturn` Nothing
    -- 「机が空くまで待つ」
    it "waits for a free table" $ do
      party <- newTVarIO ["a", "b", "c"]
      tables <- newTVarIO 1
      _ <- atomically (seatParty 3 party tables)
      readTVarIO tables `shouldReturn` 0
    -- 「急ぎの依頼が先」
    it "takes urgent quests first" $ do
      urgent <- newTQueueIO
      normal <- newTQueueIO
      atomically (writeTQueue normal "herbs" >> writeTQueue urgent "dragon")
      atomically (nextQuest urgent normal) `shouldReturn` "dragon"
      atomically (nextQuest urgent normal) `shouldReturn` "herbs"

  describe "the board" $ do
    -- 「閉店までに、全部の依頼をこなす」
    it "does every quest before closing" $ do
      let quests = [T.pack ("quest-" ++ show i) | i <- [1 .. 30 :: Int]]
      results <- guildDay 3 quests
      sort (map snd results) `shouldBe` sort quests
    -- 「上限つきの掲示板は、あふれない」
    it "never holds more than the bound" $ do
      board <- newTVarIO []
      peak <- newIORef 0
      let quests = [T.pack (show i) | i <- [1 .. 20 :: Int]]
          watch = do
            n <- length <$> readTVarIO board
            atomicModifyIORef' peak (\p -> (max p n, ()))
      taken <- newIORef []
      -- Post and take at the same time.
      -- 貼るのと取るのを、同時に
      _ <- forConcurrently [True, False] $ \poster ->
        if poster
          then mapM_ (\q -> atomically (postBounded 3 board q) >> watch) quests
          else mapM_ (\_ -> atomically (takeBounded board)
                        >>= \q -> modifyIORef' taken (q :)) quests
      readIORef peak >>= (`shouldSatisfy` (<= 3))
      reverse <$> readIORef taken `shouldReturn` quests
    -- 「時間内に来なければ Nothing」
    it "takeWithin gives up" $ do
      queue <- newTQueueIO
      takeWithin 100000 queue `shouldReturn` Nothing
      atomically (writeTQueue queue "herbs")
      takeWithin 100000 queue `shouldReturn` Just "herbs"

  describe "exceptions" $
    -- 「途中で転んだら、箱には何が残る？」
    it "leaves different things in different boxes" $
      stumbleDemo `shouldReturn` [(70, 0), (100, 0), (100, 0)]
