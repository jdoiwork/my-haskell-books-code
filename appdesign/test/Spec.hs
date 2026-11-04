module Main (main) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.SQLite.Simple (withConnection)
import Effectful (runEff, runPureEff)
import Test.Hspec

import App.ByHand
import App.Class
import App.Effect
import App.Enlist
import App.Env
import App.Roster
import App.Stacks
import App.Types
import AppAnswers

lambda, python :: NewAdventurer
lambda = NewAdventurer "Lambda" "mage" 3
python = NewAdventurer "Python" "healer" 2

-- 🧪 Run an App on a fresh in-memory database, collecting the log.
-- 新しいメモリー上のデータベースで App を動かし、ログを集める
withApp :: App a -> IO (a, [Text])
withApp app = withConnection ":memory:" $ \conn -> do
  logs <- newIORef []
  let env = Env { envConn = conn
                , envLog = \line -> modifyIORef logs (++ [line]) }
  result <- runApp env (setup >> app)
  lines' <- readIORef logs
  pure (result, lines')

main :: IO ()
main = hspec $ do
  describe "by hand and with App" $ do
    -- 手で渡しても、App でも、同じ結果
    it "agree" $ do
      byHand <- withConnection ":memory:" $ \conn -> do
        setupH conn
        a <- enlistH conn (\_ -> pure ()) lambda
        b <- enlistH conn (\_ -> pure ()) lambda
        pure (a, b)
      (withApp_, _) <- withApp ((,) <$> enlist lambda <*> enlist lambda)
      byHand `shouldBe` withApp_
      withApp_ `shouldBe` (Right 1, Left (NameTaken "Lambda"))
    -- withContext は、その部分のログにだけ札を付ける
    it "logs with a context tag" $ do
      (_, logs) <- withApp (enlist lambda >> addAdventurer python)
      logs `shouldBe` ["[enlist] added Lambda", "added Python"]
  describe "ExceptT" $ do
    -- 最初の問題で止まる
    it "stops at the first problem" $ do
      (results, _) <- withApp $ mapM enlist
        [NewAdventurer "" "mage" 0, NewAdventurer "Rust" "knight" 0]
      results `shouldBe` [Left EmptyName, Left (BadLevel 0)]
    -- case の階段で書いても、同じ答え
    it "agrees with the case version" $ do
      let tries = [lambda, lambda, NewAdventurer "" "x" 5, python]
      (a, _) <- withApp (mapM enlist tries)
      (b, _) <- withApp (mapM enlistCase tries)
      a `shouldBe` b
  describe "stacks" $
    -- 重ねる順番で、エラーのあとの状態が変わる
    it "the order of the layers matters" $ do
      stateOutside `shouldBe` Left "boom"
      exceptOutside `shouldBe` (Left "boom", 1)
  describe "type class" $ do
    -- 純粋な代役で、データベースなしにテストできる
    it "runs on the pure Fake" $ do
      let (results, roster) = runFake (mapM enlistM [lambda, lambda, python])
      results `shouldBe` [Right 1, Left (NameTaken "Lambda"), Right 2]
      Map.keys roster `shouldBe` ["Lambda", "Python"]
    -- 本物の App でも、同じ仕事が動く
    it "runs on the real App" $ do
      (results, _) <- withApp (mapM enlistM [lambda, lambda])
      results `shouldBe` [Right 1, Left (NameTaken "Lambda")]
  describe "effectful" $ do
    -- 純粋なハンドラーで
    it "runs with the pure handler" $ do
      let (results, roster) =
            runPureEff (runRosterPure (mapM enlistE [lambda, lambda]))
      results `shouldBe` [Right 1, Left (NameTaken "Lambda")]
      Map.size roster `shouldBe` 1
    -- SQLite のハンドラーで
    it "runs with the SQLite handler" $ do
      results <- withConnection ":memory:" $ \conn -> do
        runApp (Env conn (\_ -> pure ())) setup
        runEff (runRosterSqlite conn (mapM enlistE [lambda, lambda]))
      results `shouldBe` [Right 1, Left (NameTaken "Lambda")]
  describe "answers" $
    -- ステップ3：全員のレベル上げ。ログには札が付く
    it "promoteAll" $ do
      (everyone, logs) <- withApp $ do
        _ <- addAdventurer lambda
        _ <- addAdventurer python
        promoteAll
        listAll
      map advLevel everyone `shouldBe` [4, 3]
      logs `shouldBe`
        [ "added Lambda", "added Python"
        , "[festival] level up: Lambda", "[festival] level up: Python" ]
