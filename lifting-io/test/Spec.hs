module Main (main) where

import Control.Exception (finally)
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (evalStateT, runStateT)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

import Lift.Anywhere
import Lift.App
import Lift.Ledger
import Lift.Lost
import Lift.Stack
import Lift.Unlift
import LiftAnswers

-- 🧪 An Env whose logger collects the lines in an IORef.
-- ロガーが IORef に行を集める Env
collecting :: IO (Env, IO [Text])
collecting = do
  logs <- newIORef []
  let env = guildEnv { envLog = \line -> modifyIORef logs (++ [line]) }
  pure (env, readIORef logs)

-- 🧪 A fresh file in the temporary directory, removed afterwards.
-- 一時ディレクトリーの新しいファイル。あとで消す
withLedger :: (FilePath -> IO a) -> IO a
withLedger use = do
  dir <- getTemporaryDirectory
  let path = dir </> "lifting-io-ledger.txt"
  exists <- doesFileExist path
  if exists then removeFile path else pure ()
  use path `finally` removeFile path

quests :: [(Text, Int)]
quests = [("slime", 50), ("goblin", 80)]

main :: IO ()
main = hspec $ do
  describe "lift and liftIO" $ do
    -- 「3階建てで、金貨を数えてログを書く」
    it "counts the gold on three floors, and logs" $ do
      (env, logs) <- collecting
      runQuestDay env quests `shouldReturn` Right 130
      logs `shouldReturn`
        ["accepted: slime", "accepted: goblin", "total: 130 G"]
    -- 「報酬のない依頼で止まる」
    it "stops at a quest with no reward" $ do
      (env, logs) <- collecting
      runQuestDay env [("slime", 50), ("dragon", 0), ("goblin", 80)]
        `shouldReturn` Left (NoReward "dragon")
      logs `shouldReturn` ["accepted: slime"]

  describe "MonadIO" $ do
    -- 「lift のない版も、同じログを書く」
    it "questDay2 logs the same, without lift" $ do
      (env, logs) <- collecting
      r <- runApp env (runExceptT (runStateT (questDay2 quests) 0))
      r `shouldBe` Right ((), 130)
      logs `shouldReturn`
        ["accepted: slime", "accepted: goblin", "total: 130 G"]
    -- 「announce は IO でも StateT でも動く」
    it "announce works in IO and in StateT" $ do
      announce "in IO" :: IO ()
      evalStateT (announce "in StateT") (0 :: Int)

  describe "functions that take IO steps" $ do
    -- 「手ではさんでも、withRunInIO でも、unliftio でも、同じ帳簿になる」
    it "writes the same ledger in three ways" $
      withLedger $ \path -> do
        (env, logs) <- collecting
        runApp env (recordByHand path ["a"])
        runApp env (recordWith path ["b"])
        runApp env (record path ["c"])
        TIO.readFile path `shouldReturn` "a\nb\nc\n"
        logs `shouldReturn` ["recorded: a", "recorded: b", "recorded: c"]
    -- 「同時に鑑定しても、答えは同じ順」
    it "appraises at the same time, answers in order" $ do
      runApp guildEnv (appraiseAllByHand ["sword", "herb"])
        `shouldReturn` [500, 400]
      runApp guildEnv (appraiseAll ["sword", "herb"])
        `shouldReturn` [500, 400]
    -- 「ドラゴンが来ても、受付は閉まる」
    it "closes the desk even when a dragon comes" $ do
      (env, logs) <- collecting
      r <- runApp env deskTrouble
      either show show r `shouldBe` "Dragon"
      logs `shouldReturn` ["desk: open", "a dragon!", "desk: closed"]
    -- 「コールバックは、App の環境を覚えている」
    it "a callback remembers the Env" $ do
      (env, logs) <- collecting
      cb <- runApp env (callbackFor "patrol")
      cb >> cb
      logs `shouldReturn` ["called back: patrol", "called back: patrol"]

  describe "why StateT cannot be unlifted" $ do
    -- 「片方のメモ帳を捨てると、3 G が消える」
    it "one notebook is thrown away: 3 G are lost" $
      lostGold `shouldReturn` 5
    -- 「流れごとに数えて足せば、消えない」
    it "count in each flow, then add: nothing is lost" $
      savedGold `shouldReturn` 8

  describe "answers" $ do
    -- 「店は開いて、閉まる」
    it "the shop opens and closes" $ do
      (env, logs) <- collecting
      runApp env shopDay
      logs `shouldReturn` ["shop: open", "sold: herb", "shop: closed"]
    -- 「logLines はどこでも動く」
    it "logLines works anywhere" $ do
      logLines ["slime", "goblin"] :: IO ()
      runApp guildEnv (logLines ["dragon"])
      evalStateT (logLines ["herb"]) (0 :: Int)
